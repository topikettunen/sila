(defpackage #:sila/utilities
  (:use #:cl)
  (:export #:required))
(in-package #:sila/utilities)

(defun required (name)
  "A function to call as a slot initializer when it's required."
  (declare (type symbol name))
  (error "A slot ~S (of package ~S) is required but not supplied" name (symbol-package name)))
