;;; spelo-tests -- Tests for Spelo

;; Copyright (c) 2018 Thibault Polge <thibault@thb.lt>

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

(require 'ert)
(require 'spelo)

(defmacro spelo-tests--on-test-env (&rest body)
  `(progn
     (setq spelo--variables nil
           spelo-files nil)

     (defconst spelo--testdir (expand-file-name
                               (make-temp-name "spelo-tests")
                               (temporary-file-directory)))
     (defconst spelo--file1 (expand-file-name "file1" spelo--testdir))
     (defconst spelo--file2 (expand-file-name "file2" spelo--testdir))
     (defconst spelo--file3 (expand-file-name "file3" spelo--testdir))
     (defvar spelo--var1 "Value")
     (defvar spelo--var2 "Value")
     (defvar spelo--var3 "Value")

     (defun spelo--func1 ()
       "This function is part of the Spelo test suite."
       (message "Running spelo--func1"))
     (defun spelo--func2 ()
       "This function is part of the Spelo test suite."
       (message "Running spelo--func2"))

     ,@body

     (makunbound 'spelo--file1)
     (makunbound 'spelo--file2)
     (makunbound 'spelo--file3)
     (makunbound 'spelo--var1)
     (makunbound 'spelo--var2)
     (makunbound 'spelo--var3)
     (fmakunbound 'spelo--func1)
     (fmakunbound 'spelo--func2)

     (delete-directory spelo--testdir t)))

(defun spelo-tests--compare-elements (a b)
  "Compare elements of list regardless of ordering."
  (when (equal (length a) (length b))
    (mapc
     (lambda (elt)
       (setq b (gnus-delete-first elt b)))
     a)
    (null b)))

(ert-deftest spelo:add-variable:setters-and-getters ()
  "Test the public interface.  This ensures objects get created
and modified correctly, but doesn't validate Spelo features, only
the sanity of the public interface."
  (spelo-tests--on-test-env
   (should
    (spelo-add-variable 'spelo--var1
                        :file spelo--file1
                        :replace t
                        :load 'spelo--func1
                        :save '(spelo--func1 spelo--func2)))

   ;; We don't register the same variable twice.
   (should-error (spelo-add-variable 'spelo--var1))

   ;; Test that values are correctly set.
   (let ((obj (car spelo--variables)))
     (should (equal 'spelo--var1 (oref obj variable)))
     (should (equal spelo--file1 (oref obj file)))
     (should (equal (list 'spelo--func1) (oref obj load)))
     (should (equal (list 'spelo--func2 'spelo--func1) (oref obj save))))

   ;; Reset the variable
   (should (spelo-add-variable 'spelo--var1 :replace t))

   ;; Test that the value was correctly registered
   (should (equal (list (spelo--get-variable 'spelo--var1)) spelo--variables))

   ;; Delete the variable
   (spelo-delete-variable (spelo--get-variable 'spelo--var1))
   (should (equal 0 (length spelo--variables)))
   ))

(ert-deftest spelo:add-variable:advising:0 ()
  "Test that `spelo-add-variable' correctly installs advices.  Case 0: no advices."
  (spelo-tests--on-test-env
   ;; Case 1: without any advice on constructor.
   (let ((obj (spelo-add-variable 'spelo--var2))
         (loadadv1 (spelo--advice-symbol 'load 'spelo--func1))
         (loadadv2 (spelo--advice-symbol 'load 'spelo--func2))
         (saveadv1 (spelo--advice-symbol 'save 'spelo--func1))
         (saveadv2 (spelo--advice-symbol 'save 'spelo--func2)))

     (should (equal nil (oref obj load)))
     (should (equal nil (oref obj save))))))

(ert-deftest spelo:add-variable:advising:1 ()
  "Test that `spelo-add-variable' correctly installs advices.
Case 1: one function per slot."
  (spelo-tests--on-test-env
   ;; Case 1: without any advice on constructor.
   (let ((obj (spelo-add-variable 'spelo--var2
                                  :load 'spelo--func1
                                  :save 'spelo--func2))
         (loadadv1 (spelo--advice-symbol 'load 'spelo--func1))
         (loadadv2 (spelo--advice-symbol 'load 'spelo--func2))
         (saveadv1 (spelo--advice-symbol 'save 'spelo--func1))
         (saveadv2 (spelo--advice-symbol 'save 'spelo--func2)))

     (should (equal 1 (length (oref obj load))))
     (should (equal 1 (length (oref obj save))))

     (should (advice-member-p loadadv1 'spelo--func1))
     (should-not (advice-member-p loadadv2 'spelo--func2))
     (should-not (advice-member-p saveadv1 'spelo--func1))
     (should (advice-member-p saveadv2 'spelo--func2)))))

(ert-deftest spelo:add-variable:advising:2 ()
  "Test that `spelo-add-variable' correctly installs advices.
Case 2: two functions per slot."
  (spelo-tests--on-test-env
   ;; Case 1: without any advice on constructor.
   (let ((obj (spelo-add-variable 'spelo--var2
                                  :load '(spelo--func1 spelo--func2)
                                  :save '(spelo--func1 spelo--func2)))
         (loadadv1 (spelo--advice-symbol 'load 'spelo--func1))
         (loadadv2 (spelo--advice-symbol 'load 'spelo--func2))
         (saveadv1 (spelo--advice-symbol 'save 'spelo--func1))
         (saveadv2 (spelo--advice-symbol 'save 'spelo--func2)))

     (should (equal 2 (length (oref obj load))))
     (should (equal 2 (length (oref obj save))))

     (should (advice-member-p loadadv1 'spelo--func1))
     (should (advice-member-p loadadv2 'spelo--func2))
     (should (advice-member-p saveadv1 'spelo--func1))
     (should (advice-member-p saveadv2 'spelo--func2)))))

(ert-deftest spelo:add-*-function:1 ()
  "Check behavior of `spelo-add-load-function',
and `spelo-add-load-function', adding a single function."
  (spelo-tests--on-test-env
   ;; Case 1: without any advice on constructor.
   (let ((obj (spelo-add-variable 'spelo--var2))
         (loadadv1 (spelo--advice-symbol 'load 'spelo--func1))
         (loadadv2 (spelo--advice-symbol 'load 'spelo--func2))
         (saveadv1 (spelo--advice-symbol 'save 'spelo--func1))
         (saveadv2 (spelo--advice-symbol 'save 'spelo--func2)))

     (spelo-add-load-function obj 'spelo--func1)
     (spelo-add-save-function obj 'spelo--func2)

     ;; Test object field
     (should (equal (oref obj load) (list 'spelo--func1)))
     (should (equal (oref obj save) (list 'spelo--func2)))

     ;; Test advices
     (should (advice-member-p loadadv1 'spelo--func1))
     (should-not (advice-member-p loadadv2 'spelo--func2))
     (should-not (advice-member-p saveadv1 'spelo--func1))
     (should (advice-member-p saveadv2 'spelo--func2)))))

(ert-deftest spelo:add-*-function:2 ()
  "Check behavior of `spelo-add-load-function',
and `spelo-add-load-function', adding multiple functions in a
single call."
  (spelo-tests--on-test-env
   ;; Case 1: without any advice on constructor.
   (let ((obj (spelo-add-variable 'spelo--var2))
         (loadadv1 (spelo--advice-symbol 'load 'spelo--func1))
         (loadadv2 (spelo--advice-symbol 'load 'spelo--func2))
         (saveadv1 (spelo--advice-symbol 'save 'spelo--func1))
         (saveadv2 (spelo--advice-symbol 'save 'spelo--func2)))

     (spelo-add-load-function obj 'spelo--func1 'spelo--func2)
     (spelo-add-save-function obj 'spelo--func1 'spelo--func2)

     ;; Test object field
     (should (equal (oref obj load) (list 'spelo--func2 'spelo--func1)))
     (should (equal (oref obj save) (list 'spelo--func2 'spelo--func1)))

     ;; Test advices
     (should (advice-member-p loadadv1 'spelo--func1))
     (should (advice-member-p loadadv2 'spelo--func2))
     (should (advice-member-p saveadv1 'spelo--func1))
     (should (advice-member-p saveadv2 'spelo--func2)))))

(ert-deftest spelo:remove-*-function:1 ()
  "Check behavior of `spelo-remove-load-function',
and `spelo-add-remove-function', removing a single function."
  (spelo-tests--on-test-env
   ;; Case 1: without any advice on constructor.
   (let ((obj (spelo-add-variable
               'spelo--var2
               :load '(spelo--func1  spelo--func2)
               :save '(spelo--func1  spelo--func2)))
         (loadadv1 (spelo--advice-symbol 'load 'spelo--func1))
         (loadadv2 (spelo--advice-symbol 'load 'spelo--func2))
         (saveadv1 (spelo--advice-symbol 'save 'spelo--func1))
         (saveadv2 (spelo--advice-symbol 'save 'spelo--func2)))

     (spelo-remove-load-function obj 'spelo--func1)
     (spelo-remove-save-function obj 'spelo--func2)

     ;; Test object field
     (should (equal (oref obj load) (list 'spelo--func2)))
     (should (equal (oref obj save) (list 'spelo--func1)))

     ;; Test advices
     (should-not (advice-member-p loadadv1 'spelo--func1))
     (should (advice-member-p loadadv2 'spelo--func2))
     (should (advice-member-p saveadv1 'spelo--func1))
     (should-not (advice-member-p saveadv2 'spelo--func2)))))

(ert-deftest spelo:remove-*-function:2 ()
  "Check behavior of `spelo-remove-load-function',
and `spelo-remove-save-function', removing multiple functions in
a single call."
  (spelo-tests--on-test-env
   ;; Case 1: without any advice on constructor.
   (let ((obj (spelo-add-variable
               'spelo--var2
               :load '(spelo--func1  spelo--func2)
               :save '(spelo--func1  spelo--func2)))
         (loadadv1 (spelo--advice-symbol 'load 'spelo--func1))
         (loadadv2 (spelo--advice-symbol 'load 'spelo--func2))
         (saveadv1 (spelo--advice-symbol 'save 'spelo--func1))
         (saveadv2 (spelo--advice-symbol 'save 'spelo--func2)))

     (spelo-remove-load-function obj 'spelo--func1 'spelo--func2)
     (spelo-remove-save-function obj 'spelo--func1 'spelo--func2)

     ;; Test object field
     (should (equal (oref obj load) nil))
     (should (equal (oref obj save) nil))

     ;; Test advices
     (should-not (advice-member-p loadadv1 'spelo--func1))
     (should-not (advice-member-p loadadv2 'spelo--func2))
     (should-not (advice-member-p saveadv1 'spelo--func1))
     (should-not (advice-member-p saveadv2 'spelo--func2)))))

(ert-deftest spelo:advices ()
  (spelo-tests--on-test-env
   (spelo-add-variable 'spelo--var1 :save 'spelo--func1 :load 'spelo--func2)
   (setq spelo--var1 18)
   (spelo--func1)
   (setq spelo--var1 nil)
   (spelo--func2)
   (should (equal spelo--var1 18))))

(provide 'spelo-tests)

;;; spelo-tests ends here
