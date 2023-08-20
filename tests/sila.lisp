(defpackage #:sila/tests/main
  (:use #:cl
        #:sila
        #:rove))
(in-package #:sila/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :sila)' in your Lisp.

(defvar *bin-name* "sila")
(defvar *tmp-name* "tmp.s")
(defvar *tmp-bin-name* "tmp")

(defun compile-program-and-compare-rc (input expected-rc)
  (multiple-value-bind (out err rc)
      (uiop:run-program (list (format nil "./~a" *bin-name*) input)
                        :output :string
                        :error-output :string)
    (declare (ignore err rc))
    (with-open-file (fstream *tmp-name*
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
      (format fstream out))
    (uiop:run-program (list "gcc" "-static" "-o" *tmp-bin-name* *tmp-name*))
    (multiple-value-bind (out err got-rc)
        (uiop:run-program (format nil "./~a" *tmp-bin-name*)
                          ;; Since we're working with arbitrary return codes.
                          :ignore-error-status t)
      (declare (ignore out err))
      (= expected-rc got-rc))))

(deftest test-compilation-and-compare-rc
  (ok (compile-program-and-compare-rc "0" 0))
  (ok (compile-program-and-compare-rc "5 + 40 - 20" 25)))
