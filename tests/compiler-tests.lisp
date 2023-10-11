(in-package #:sila/tests)

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

;;; To speed up these, these probably could be run in parallel.

#+linux
(deftest test-compilation-and-compare-rc
  (ok (compile-program-and-compare-rc "0;" 0))
  (ok (compile-program-and-compare-rc "5 + 40 - 20;" 25))
  (ok (compile-program-and-compare-rc "2 / (1 + 1) * 8;" 8))
  (ok (compile-program-and-compare-rc "- -10;" 10))
  (ok (compile-program-and-compare-rc "-10+20;" 10))
  (ok (compile-program-and-compare-rc "0==1;" 0))
  (ok (compile-program-and-compare-rc "1!=1;" 0))
  (ok (compile-program-and-compare-rc "0==0;" 1))
  (ok (compile-program-and-compare-rc "1!=0;" 1))
  (ok (compile-program-and-compare-rc "0<1;" 1))
  (ok (compile-program-and-compare-rc "1<1;" 0))
  (ok (compile-program-and-compare-rc "1<=1;" 1))
  (ok (compile-program-and-compare-rc "2<=1;" 0))
  (ok (compile-program-and-compare-rc "0<1;" 1))
  (ok (compile-program-and-compare-rc "1<1;" 0))
  (ok (compile-program-and-compare-rc "1>=1;" 1))
  (ok (compile-program-and-compare-rc "1>=2;" 0))
  (ok (compile-program-and-compare-rc "1; 2; 3;" 3))
  (ok (compile-program-and-compare-rc "a<-8; a;" 8))
  (ok (compile-program-and-compare-rc "a<-3; b<-5; a+b;" 8))
  (ok (compile-program-and-compare-rc "foo<-3; bar<-5; foo+bar;" 8))
  (ok (compile-program-and-compare-rc "foo2<-3; bar2<-5; foo2+bar2;" 8)))
