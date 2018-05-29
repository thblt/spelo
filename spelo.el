;;; SPELO -- Stupid Persistence for Emacs Lisp Objects

;; Copyright (c) 2018 Thibault Polge <thibault@thb.lt>

;; Author: Thibault Polge <thibault@thb.lt>
;; Homepage: https://github.com/thblt/spelo
;; Keywords: tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see https://www.gnu.org/licenses.

;;; Commentary:

;; See README.org

;;; Code:

(require 'dash)
(require 'eieio)

;;; Variables

(defvar spelo-default-file (expand-file-name "var/spelo.el" user-emacs-directory))

(defvar spelo--variables nil
  "List of `spelo--variable' objects managed by Spelo.")

(defvar spelo--files nil
  "List of destination files Spelo knows about.")

(defvar spelo--initialized nil)

;;; Interactive interface

;;;###autoload
(cl-defun spelo-add-variable (var &key file &key load &key save &key serializer &key replace)
  "Manage VAR with Spelo."
  (interactive
   (let ((vars))
     (mapatoms (lambda (o)  (add-to-list 'vars o)))
     (list (completing-read "Variable: " vars))))
  (--when-let (spelo--get-variable var)
    (if replace
        (spelo-delete-variable it)
        (error "Variable `%s' is already managed by Spelo" var)))

  (let ((obj (spelo--variable :variable var :serializer serializer)))
    (add-to-list 'spelo--variables obj)
    (spelo-set-file obj file)
    (apply 'spelo-add-load-function obj (if (listp load) load (list load)))
    (apply 'spelo-add-save-function obj (if (listp save) save (list save)))
  obj))

(defun spelo-delete-variable (obj)
  "Stop managing VAR."
  (interactive (list
                (spelo--get-variable
                 (completing-read
                  "Variable: "
                  (mapcar (lambda (o) (oref o variable))
                          spelo--variables)))))
  ;;@TODO
  (setq spelo--variables (delete obj spelo--variables)))

;;; Emacs Lisp interface

(defun spelo-initialize (self-manage)
  "Initialize Spelo.

If SELF-MANAGE is non-nil, Spelo will make the list of variables
it manages persistent.  If SELF-MANAGE is a string, it will be
used as the file in which to store the list of Spelo variables."

  (unless after-init-time
    (add-hook 'after-init-hook 'spelo--after-init-hook-function))
  (add-hook 'kill-emacs-hook 'spelo--kill-emacs-hook-function)

  (when self-manage
    (let ((obj (spelo-add-variable 'spelo--variables)))
      (when (stringp self-manage)
        (spelo-set-file ))
    )

  )
(defun spelo-set-file (var &optional file)
  "@TODO."
  (when file
    (setq file (expand-file-name file))
    (make-directory (file-name-directory file) t)
    (unless (member file spelo--files)
      (add-to-list 'spelo--files file)))
  (oset var file file))

;;;; Getters

(defun spelo-managed-variables ()
  "List variables managed by Spelo."
  (cl-remove-duplicates (mapcar (lambda (obj) (oref obj variable)) spelo--variables)))

(defun spelo--get-variable (var)
  "Return the `spelo--variable' object corresponding to VAR, or
nil if VAR is not managed by Spelo."
  (when (stringp var) (setq var (intern var)))
  (cl-some
   (lambda (obj)
     (and
      (equal var (oref obj variable))
      obj))
   spelo--variables))

(defmacro spelo--with-variable-object (var &rest body)
  "Evaluate BODY with OBJ bound to the object corresponding to VAR.

Raise an error if there's no such object."
  (declare (indent defun))
  `(let ((obj (spelo--get-variable ,var)))
     (unless obj
       (error (format "Variable `%s' is not managed by Spelo" ,var)))
     (progn ,@body)))

(defun spelo-variable-load-functions (var)
  "Return the functions that trigger the reloading of the file associated to VAR."
  (spelo--with-variable-object var (eieio-oref obj 'load)))

(defun spelo-variable-save-functions (var)
  "Return the functions that trigger saving back the value of VAR."
  (spelo--with-variable-object var (eieio-oref obj 'save)))

(cl-defun spelo-variable-file (var)
  "@TODO"
  (spelo--with-variable-object var (eieio-oref obj 'file)))

(defalias 'spelo-managed-p 'spelo--get-variable
  "Determine if VAR is managed by Spelo.

Return non-nil if Spelo manages VAR.")

(defun spelo--group-by-file (&rest objects)
  "Group OBJECTS by file.

Return value is a list of cons of type (FILE . (VARIABLE ...)) (a
plist).  FILE are strings, VARIABLE are the provided objects."
  (let ((ret))
    (mapcar (lambda (obj)
              (setq ret
                    (lax-plist-put
                     ret
                     (oref obj file)
                     (cons obj (lax-plist-get ret (oref obj file))))))
            objects)
  ret))

;;; Advising

(defun spelo-add-load-function (obj &rest funcs)
  (spelo--add-function obj 'load funcs))

(defun spelo-add-save-function (obj &rest funcs)
  (spelo--add-function obj 'save funcs))

(defun spelo-remove-load-function (obj &rest funcs)
  (spelo--remove-function obj 'load funcs))

(defun spelo-remove-save-function (obj &rest funcs)
  (spelo--remove-function obj 'save funcs))

(defun spelo--add-function (obj op funcs)
  "Advise functions FUNC for VAR.  OP is either load or
save, a symbol."
  ;;(unless (listp funcs) (setq funcs (list funcs)))
  (mapcar (lambda (f)
            (assert (symbolp f))
            (let ((objfuncs (eieio-oref obj op)))
              (unless (member f objfuncs)
                (eieio-oset obj op (cons f objfuncs))))
            (case op
              ('load (spelo--advise load f))
              ('save (spelo--advise save f))
              (t (error "[Spelo] Bad operation %s" op))))
          funcs))

(defun spelo--remove-function (obj op funcs)
  "@TODO."
  (let ((otherfuncs (cl-remove-duplicates
                     (-flatten
                      (mapcar
                       (lambda (f) (unless (equal obj f)
                         (eieio-oref f op)))
                       spelo--variables)))))
    (mapcar (lambda (f)
              ;; Delete from list
              (eieio-oset obj op (delete f (eieio-oref obj op)))
              ;; Maybe delete advice
              (unless (member f otherfuncs)
                (advice-remove f (spelo--advice-symbol op f))))
            funcs)))

(defun spelo--advice-symbol (op func)
  (intern (format "--spelo--%s-advice-for:%s" op func)))

(defmacro spelo--advise (op func)
  (let ((advice (spelo--advice-symbol op (eval func))))
    `(progn
       (unless (fboundp ',advice)
         (defun ,advice (&rest _)
           (,(intern (format "spelo--run-%s-advice" op)) ,(eval func))))
       (message "Advising %s with %s" ,func ',advice)
       (advice-add
        ',(eval func)
        ,(case op ('load :before) ('save :after) (t (error "Bad operatio in spelo--advise %s" op)))
        ',advice))))

;; (setq myfunc 'message)
;; (spelo--advise load myfunc)

;;; Serialization, deserialization

(defmacro spelo-setq (symbol value)
  "A utility macro so that Spelo serialization files correctly evaluate as Elisp sources."
  `(setq ,symbol ,value))

(defun spelo--lookup-definition (obj)
  "Position the point before the definition of OBJ in the current buffer.

Return t if the definition was found, nil otherwise."
  (beginning-of-buffer)
    (deactivate-mark t)
    (re-search-forward (format "^(spelo-setq %s " (regexp-quote
                                                   (symbol-name
                                                    (oref obj variable))))
                       nil t))

(defun spelo-restore (&rest objects)
  "Restore value of OBJECTS from serialization files."
  (setq obj (spelo--group-by-file (obj)))
  (mapcar (lambda (fobjs)
            (spelo--restore-from-file (car fobjs) (cdr fobjs)))))

(defun spelo--restore-from-file (file &rest objects)
  "Load FILE and restore OBJECTS from there."
  (with-temp-buffer
    (insert-file-contents file)
    (mapcar (lambda (xobj)
              (save-excursion
                (setf
                 (oref obj variable)
                 (funcall (oref obj deserializer) (spelo--read obj))))))))

(defun spelo--read (obj)
  "Return the unevaluated value of OBJ

Look up the definition of OBJ in the current buffer, and return
it as an unevaluated lisp form."
  (and (spelo--lookup-definition obj)
       (read (buffer-substring (point) (progn (forward-sexp) (point))))))

(defun spelo--write (obj)
  (if (spelo--lookup-definition obj)
      (kill-sexp)
    (end-of-buffer)
    (insert (format "\n(spelo-setq %s ')\n" (oref obj variable)))
    (backward-char 2))
  (prin1 (symbol-value (oref obj variable)) 'insert))

(defun spelo--run-load-advice (func)
  "Responder for load (:before) advice on FUNC."
  (message Hello", running LOAD advice on %s" func)
  ()
  )

(defun spelo--run-save-advice (func)
  "@TODO"
  (message "Hello, running SAVE advice on %s" func)
  )


(defun spelo--save-for-function (func &rest args)
  "@TODO"
  ;;(apply func args)
  (let ((files (cl-delete-duplicates
            (-non-nil
             (mapcar (lambda (obj)
                       (and (member func (oref obj load))
                            (oref obj file)))
                     spelo--variables)))))

    (mapc
     (lambda (file)
       (message "[Spelo] Serializing into %s because '%s' was invoked" file func)
       (load file t))
     files)))

;;; The spelo--variable class

(defclass spelo--variable nil
  ((variable
    :type symbol
    :initarg :variable
    :doc "The name of this variable.")

   (file
    :type (or null string)
    :initarg :file
    :initform nil
    :doc "Serialization file for this variable.")

   (load
    :type (or null list)
    :initarg :load
    :initform nil
    :doc "Functions which should trigger loading of this variable's file.")

   (save
    :type (or null list)
    :initarg :save
    :initform nil
    :doc "Functions which should trigger saving of this variable's file.")

   (serializer
    :type (or list symbol)
    :initarg :serializer
    :initform 'spelo-serialize
    :doc "The serialization function to use for this variable.
    Serializers take a Lisp Object as their unique argument and
    must (insert) the serialized data at the current position in
    the current buffer. ")))

(provide 'spelo)

;;; spelo.el ends here
