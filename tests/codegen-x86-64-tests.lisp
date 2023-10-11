(in-package #:sila/tests)

;; NOTE: To run this test file, execute `(asdf:test-system :sila)' in your Lisp.

(deftest test-emit-x86-64-integer
  (let ((emit-asm-tests '(("0;" . "  .globl main
main:
  push %rbp
  mov %rsp, %rbp
  sub $0, %rsp
  mov $0, %rax
  mov %rbp, %rsp
  pop %rbp
  ret
")
                          ("42;" . "  .globl main
main:
  push %rbp
  mov %rsp, %rbp
  sub $0, %rsp
  mov $42, %rax
  mov %rbp, %rsp
  pop %rbp
  ret
"))))
    (dolist (test emit-asm-tests)
      (ok (string-equal (sila/codegen:emit-asm (car test) :indent 2 :indent-tabs nil) (cdr test))
          (format nil "Expect to be equal: ~%~a~%=>~a"
                  `(sila/codegen:emit-asm ,(car test) :indent 2 :indent-tabs nil)
                  (cdr test))))))

(deftest test-emit-x86-64-add-sub
  (let ((emit-asm-tests '(("5+20-4;" . "  .globl main
main:
  push %rbp
  mov %rsp, %rbp
  sub $0, %rsp
  mov $4, %rax
  push %rax
  mov $20, %rax
  push %rax
  mov $5, %rax
  pop %rdi
  add %rdi, %rax
  pop %rdi
  sub %rdi, %rax
  mov %rbp, %rsp
  pop %rbp
  ret
")
                          ("    5   +  20  -  4   ;" . "  .globl main
main:
  push %rbp
  mov %rsp, %rbp
  sub $0, %rsp
  mov $4, %rax
  push %rax
  mov $20, %rax
  push %rax
  mov $5, %rax
  pop %rdi
  add %rdi, %rax
  pop %rdi
  sub %rdi, %rax
  mov %rbp, %rsp
  pop %rbp
  ret
"))))
    (dolist (test emit-asm-tests)
      (ok (string-equal (sila/codegen:emit-asm (car test) :indent 2 :indent-tabs nil) (cdr test))
          (format nil "Expect to be equal: ~%~a~%=>~a"
                  `(sila/codegen:emit-asm ,(car test) :indent 2 :indent-tabs nil)
                  (cdr test))))))

(deftest test-emit-x86-64-div-mul-parens
  (let ((emit-asm-tests '(("2 / (1 + 1) * 8;" . "  .globl main
main:
  push %rbp
  mov %rsp, %rbp
  sub $0, %rsp
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
  mov %rbp, %rsp
  pop %rbp
  ret
"))))
    (dolist (test emit-asm-tests)
      (ok (string-equal (sila/codegen:emit-asm (car test) :indent 2 :indent-tabs nil) (cdr test))
          (format nil "Expect to be equal: ~%~a~%=>~a"
                  `(sila/codegen:emit-asm ,(car test) :indent 2 :indent-tabs nil)
                  (cdr test))))))

(deftest test-emit-x86-64-unary
  (let ((emit-asm-tests '(("- -10;" . "  .globl main
main:
  push %rbp
  mov %rsp, %rbp
  sub $0, %rsp
  mov $10, %rax
  neg %rax
  neg %rax
  mov %rbp, %rsp
  pop %rbp
  ret
")
                          ("-10+20;" . "  .globl main
main:
  push %rbp
  mov %rsp, %rbp
  sub $0, %rsp
  mov $20, %rax
  push %rax
  mov $10, %rax
  neg %rax
  pop %rdi
  add %rdi, %rax
  mov %rbp, %rsp
  pop %rbp
  ret
")
                          ("- - -10;" . "  .globl main
main:
  push %rbp
  mov %rsp, %rbp
  sub $0, %rsp
  mov $10, %rax
  neg %rax
  neg %rax
  neg %rax
  mov %rbp, %rsp
  pop %rbp
  ret
"))))
    (dolist (test emit-asm-tests)
      (ok (string-equal (sila/codegen:emit-asm (car test) :indent 2 :indent-tabs nil) (cdr test))
          (format nil "Expect to be equal: ~%~a~%=>~a"
                  `(sila/codegen:emit-asm ,(car test) :indent 2 :indent-tabs nil)
                  (cdr test))))))

(deftest test-emit-x86-64-comparisons
  (let ((emit-asm-tests '(("1==1;" . "  .globl main
main:
  push %rbp
  mov %rsp, %rbp
  sub $0, %rsp
  mov $1, %rax
  push %rax
  mov $1, %rax
  pop %rdi
  cmp %rdi, %rax
  sete %al
  movzb %al, %rax
  mov %rbp, %rsp
  pop %rbp
  ret
")
                           ("1>=1;" . "  .globl main
main:
  push %rbp
  mov %rsp, %rbp
  sub $0, %rsp
  mov $1, %rax
  push %rax
  mov $1, %rax
  pop %rdi
  cmp %rdi, %rax
  setge %al
  movzb %al, %rax
  mov %rbp, %rsp
  pop %rbp
  ret
")
                           ("1<=1;" . "  .globl main
main:
  push %rbp
  mov %rsp, %rbp
  sub $0, %rsp
  mov $1, %rax
  push %rax
  mov $1, %rax
  pop %rdi
  cmp %rdi, %rax
  setle %al
  movzb %al, %rax
  mov %rbp, %rsp
  pop %rbp
  ret
")
                           ("1<1;" . "  .globl main
main:
  push %rbp
  mov %rsp, %rbp
  sub $0, %rsp
  mov $1, %rax
  push %rax
  mov $1, %rax
  pop %rdi
  cmp %rdi, %rax
  setl %al
  movzb %al, %rax
  mov %rbp, %rsp
  pop %rbp
  ret
")
                            ("1>1;" . "  .globl main
main:
  push %rbp
  mov %rsp, %rbp
  sub $0, %rsp
  mov $1, %rax
  push %rax
  mov $1, %rax
  pop %rdi
  cmp %rdi, %rax
  setg %al
  movzb %al, %rax
  mov %rbp, %rsp
  pop %rbp
  ret
"))))
    (dolist (test emit-asm-tests)
      (ok (string-equal (sila/codegen:emit-asm (car test) :indent 2 :indent-tabs nil) (cdr test))
          (format nil "Expect to be equal: ~%~a~%=>~a"
                  `(sila/codegen:emit-asm ,(car test) :indent 2 :indent-tabs nil)
                  (cdr test))))))

(deftest test-emit-x86-64-variables
  (let ((emit-asm-tests '(("a<-8;a;" . "  .globl main
main:
  push %rbp
  mov %rsp, %rbp
  sub $16, %rsp
  lea -8(%rbp), %rax
  push %rax
  mov $8, %rax
  pop %rdi
  mov %rax, (%rdi)
  lea -8(%rbp), %rax
  mov (%rax), %rax
  mov %rbp, %rsp
  pop %rbp
  ret
")
                          ("foo<-5;bar<-8;foo+bar;" . "  .globl main
main:
  push %rbp
  mov %rsp, %rbp
  sub $32, %rsp
  lea -24(%rbp), %rax
  push %rax
  mov $5, %rax
  pop %rdi
  mov %rax, (%rdi)
  lea -16(%rbp), %rax
  push %rax
  mov $8, %rax
  pop %rdi
  mov %rax, (%rdi)
  lea -16(%rbp), %rax
  mov (%rax), %rax
  push %rax
  lea -24(%rbp), %rax
  mov (%rax), %rax
  pop %rdi
  add %rdi, %rax
  mov %rbp, %rsp
  pop %rbp
  ret
"))))
    (dolist (test emit-asm-tests)
      (ok (string-equal (sila/codegen:emit-asm (car test) :indent 2 :indent-tabs nil) (cdr test))
          (format nil "Expect to be equal: ~%~a~%=>~a"
                  `(sila/codegen:emit-asm ,(car test) :indent 2 :indent-tabs nil)
                  (cdr test))))))
