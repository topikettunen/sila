(defpackage #:sila/tests/emit-asm-x86-64
  (:use #:cl
        #:sila
        #:rove))
(in-package #:sila/tests/emit-asm-x86-64)

;; NOTE: To run this test file, execute `(asdf:test-system :sila)' in your Lisp.

(deftest test-emit-asm-integer
  (let ((emit-asm-tests '(("0" . "  .globl main
main:
  mov $0, %rax
  ret
")
                          ("42" . "  .globl main
main:
  mov $42, %rax
  ret
"))))
    (dolist (test emit-asm-tests)
      (ok (string-equal (sila::emit-asm (car test)) (cdr test))
          (format nil "Expect to be equal: ~%~A~%=>~A"
                       `(sila::emit-asm ,(car test))
                       (cdr test))))))

(deftest test-emit-asm-add-sub
  (let ((emit-asm-tests '(("5+20-4" . "  .globl main
main:
  mov $5, %rax
  add $20, %rax
  sub $4, %rax
  ret
")
                          ("    5   +  20  -  4   " . "  .globl main
main:
  mov $5, %rax
  add $20, %rax
  sub $4, %rax
  ret
"))))
    (dolist (test emit-asm-tests)
      (ok (string-equal (sila::emit-asm (car test)) (cdr test))
          (format nil "Expect to be equal: ~%~A~%=>~A"
                       `(sila::emit-asm ,(car test))
                       (cdr test))))))
