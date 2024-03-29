(defpackage #:sila/tests/compiler
  (:use #:cl
        #:sila
        #:rove))
(in-package #:sila/tests/compiler)

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
  (testing "Integer"
    (ok (compile-program-and-compare-rc "{ return 0; }" 0))
    (ok (compile-program-and-compare-rc "{ ;;;;; return 1; }" 1)))

  (testing "Arithmetics"
    (ok (compile-program-and-compare-rc "{ return 5 + 40 - 20; }" 25))
    (ok (compile-program-and-compare-rc "{ return 2 / (1 + 1) * 8; }" 8)))

  (testing "Unary"
    (ok (compile-program-and-compare-rc "{ return - -10; }" 10))
    (ok (compile-program-and-compare-rc "{ return -10+20; }" 10)))

  (testing "Comparisons"
    (ok (compile-program-and-compare-rc "{ return 0==1; }" 0))
    (ok (compile-program-and-compare-rc "{ return 1!=1; }" 0))
    (ok (compile-program-and-compare-rc "{ return 0==0; }" 1))
    (ok (compile-program-and-compare-rc "{ return 1!=0; }" 1))
    (ok (compile-program-and-compare-rc "{ return 0<1; }" 1))
    (ok (compile-program-and-compare-rc "{ return 1<1; }" 0))
    (ok (compile-program-and-compare-rc "{ return 1<=1; }" 1))
    (ok (compile-program-and-compare-rc "{ return 2<=1; }" 0))
    (ok (compile-program-and-compare-rc "{ return 0<1; }" 1))
    (ok (compile-program-and-compare-rc "{ return 1<1; }" 0))
    (ok (compile-program-and-compare-rc "{ return 1>=1; }" 1))
    (ok (compile-program-and-compare-rc "{ return 1>=2; }" 0)))

  (testing "Multiple statements"
    (ok (compile-program-and-compare-rc "{ return 1; 2; 3; }" 1))
    (ok (compile-program-and-compare-rc "{ 1; return 2; 3; }" 2))
    (ok (compile-program-and-compare-rc "{ 1; 2; return 3; }" 3)))

  (testing "Variables"
    (ok (compile-program-and-compare-rc "{ a:=8; return a; }" 8))
    (ok (compile-program-and-compare-rc "{ a:=3; b:=5; return a+b; }" 8))
    (ok (compile-program-and-compare-rc "{ foo:=3; bar:=5; return foo+bar; }" 8))
    (ok (compile-program-and-compare-rc "{ foo2:=3; bar2:=5; return foo2+bar2; }" 8)))

  (testing "Block"
    (ok (compile-program-and-compare-rc "{ 1; { 2; } return 3; }" 3)))

  (testing "Conditionals"
    (ok (compile-program-and-compare-rc "{ if 1 { return 1; } return 2; }" 1))
    (ok (compile-program-and-compare-rc "{ if 0 { return 1; } else { return 2; } }" 2))
    (ok (compile-program-and-compare-rc "{ if 1<0 { return 1; } else { return 2; } }" 2)))

  (testing "For loop"
    (ok (compile-program-and-compare-rc "{ for ;; { return 3; } return 5; }" 3))
    (ok (compile-program-and-compare-rc "{ i:=0; j:=0;for i:=0; i<=10; i:=i+1 {j := i+j;} return j; }" 55)))

  (testing "Loop"
    (ok (compile-program-and-compare-rc "{ loop { return 3; } return 5; }" 3))
    (ok (compile-program-and-compare-rc "{ i := 0; loop { i := 3; break; } return i; }" 3))
    (ok (compile-program-and-compare-rc "{ i := 0; loop { i := i + 1; if i == 10 { break; } } return i; }" 10))))
