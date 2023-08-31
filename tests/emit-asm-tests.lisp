(in-package #:sila/tests)

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
          (format nil "Expect to be equal: ~%~a~%=>~a"
                  `(sila::emit-asm ,(car test))
                  (cdr test))))))

(deftest test-emit-asm-add-sub
  (let ((emit-asm-tests '(("5+20-4" . "  .globl main
main:
  mov $4, %rax
  push %rax
  mov $20, %rax
  push %rax
  mov $5, %rax
  pop %rdi
  add %rdi, %rax
  pop %rdi
  sub %rdi, %rax
  ret
")
                          ("    5   +  20  -  4   " . "  .globl main
main:
  mov $4, %rax
  push %rax
  mov $20, %rax
  push %rax
  mov $5, %rax
  pop %rdi
  add %rdi, %rax
  pop %rdi
  sub %rdi, %rax
  ret
"))))
    (dolist (test emit-asm-tests)
      (ok (string-equal (sila::emit-asm (car test)) (cdr test))
          (format nil "Expect to be equal: ~%~a~%=>~a"
                  `(sila::emit-asm ,(car test))
                  (cdr test))))))

(deftest test-emit-asm-div-mul-parens
  (let ((emit-asm-tests '(("2 / (1 + 1) * 8" . "  .globl main
main:
  mov $8, %rax
  push %rax
  mov $1, %rax
  push %rax
  mov $1, %rax
  pop %rdi
  add %rdi, %rax
  push %rax
  mov $2, %rax
  pop %rdi
  cqo
  idiv %rdi, %rax
  pop %rdi
  imul %rdi, %rax
  ret
"))))
    (dolist (test emit-asm-tests)
      (ok (string-equal (sila::emit-asm (car test)) (cdr test))
          (format nil "Expect to be equal: ~%~a~%=>~a"
                  `(sila::emit-asm ,(car test))
                  (cdr test))))))

(deftest test-emit-asm-unary
  (let ((emit-asm-tests '(("- -10" . "  .globl main
main:
  mov $10, %rax
  neg %rax
  neg %rax
  ret
")
                          ("-10+20" . "  .globl main
main:
  mov $20, %rax
  push %rax
  mov $10, %rax
  neg %rax
  pop %rdi
  add %rdi, %rax
  ret
")
                          ("- - -10" . "  .globl main
main:
  mov $10, %rax
  neg %rax
  neg %rax
  neg %rax
  ret
"))))
    (dolist (test emit-asm-tests)
      (ok (string-equal (sila::emit-asm (car test)) (cdr test))
          (format nil "Expect to be equal: ~%~a~%=>~a"
                  `(sila::emit-asm ,(car test))
                  (cdr test))))))

(deftest test-emit-asm-comparisons
  (let ((emit-asm-tests '(("1==1" . "  .globl main
main:
  mov $1, %rax
  push %rax
  mov $1, %rax
  pop %rdi
  cmp %rdi, %rax
  sete %al
  movzb %al, %rax
  ret
")
                           ("1>=1" . "  .globl main
main:
  mov $1, %rax
  push %rax
  mov $1, %rax
  pop %rdi
  cmp %rdi, %rax
  setge %al
  movzb %al, %rax
  ret
")
                           ("1<=1" . "  .globl main
main:
  mov $1, %rax
  push %rax
  mov $1, %rax
  pop %rdi
  cmp %rdi, %rax
  setle %al
  movzb %al, %rax
  ret
")
                           ("1<1" . "  .globl main
main:
  mov $1, %rax
  push %rax
  mov $1, %rax
  pop %rdi
  cmp %rdi, %rax
  setl %al
  movzb %al, %rax
  ret
")
                            ("1>1" . "  .globl main
main:
  mov $1, %rax
  push %rax
  mov $1, %rax
  pop %rdi
  cmp %rdi, %rax
  setg %al
  movzb %al, %rax
  ret
"))))
    (dolist (test emit-asm-tests)
      (ok (string-equal (sila::emit-asm (car test)) (cdr test))
          (format nil "Expect to be equal: ~%~a~%=>~a"
                  `(sila::emit-asm ,(car test))
                  (cdr test))))))
