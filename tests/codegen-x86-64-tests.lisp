(in-package #:sila/tests)

;; NOTE: To run this test file, execute `(asdf:test-system :sila)' in your Lisp.

(defun run-emit-code-tests (testcases)
  (dolist (test testcases)
    (ok (string-equal (sila/codegen:emit-code (car test) :indent 2 :indent-tabs nil) (cdr test))
        (format nil "Expect to be equal: ~%~a~%=>~a"
                `(sila/codegen:emit-code ,(car test) :indent 2 :indent-tabs nil)
                (cdr test))))
  (values))

(deftest test-emit-x86-64-integer
  (let ((tests '(("{ return 0; }" . "  .globl main
main:
  push %rbp
  mov %rsp, %rbp
  sub $0, %rsp
  mov $0, %rax
  jmp .L.return
.L.return:
  mov %rbp, %rsp
  pop %rbp
  ret
")
                 ("{ return 42; }" . "  .globl main
main:
  push %rbp
  mov %rsp, %rbp
  sub $0, %rsp
  mov $42, %rax
  jmp .L.return
.L.return:
  mov %rbp, %rsp
  pop %rbp
  ret
"))))
    (run-emit-code-tests tests)))

(deftest test-emit-x86-64-add-sub
  (let ((tests '(("{ return 5+20-4; }" . "  .globl main
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
  jmp .L.return
.L.return:
  mov %rbp, %rsp
  pop %rbp
  ret
")
                 ("{ return    5   +  20  -  4   ; }" . "  .globl main
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
  jmp .L.return
.L.return:
  mov %rbp, %rsp
  pop %rbp
  ret
"))))
    (run-emit-code-tests tests)))

(deftest test-emit-x86-64-div-mul-parens
  (let ((tests '(("{ return 2 / (1 + 1) * 8; }" . "  .globl main
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
  jmp .L.return
.L.return:
  mov %rbp, %rsp
  pop %rbp
  ret
"))))
    (run-emit-code-tests tests)))

(deftest test-emit-x86-64-unary
  (let ((tests '(("{ return - -10; }" . "  .globl main
main:
  push %rbp
  mov %rsp, %rbp
  sub $0, %rsp
  mov $10, %rax
  neg %rax
  neg %rax
  jmp .L.return
.L.return:
  mov %rbp, %rsp
  pop %rbp
  ret
")
                 ("{ return -10+20; }" . "  .globl main
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
  jmp .L.return
.L.return:
  mov %rbp, %rsp
  pop %rbp
  ret
")
                 ("{ return - - -10; }" . "  .globl main
main:
  push %rbp
  mov %rsp, %rbp
  sub $0, %rsp
  mov $10, %rax
  neg %rax
  neg %rax
  neg %rax
  jmp .L.return
.L.return:
  mov %rbp, %rsp
  pop %rbp
  ret
"))))
    (run-emit-code-tests tests)))

(deftest test-emit-x86-64-comparisons
  (let ((tests '(("{ return 1==1; }" . "  .globl main
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
  jmp .L.return
.L.return:
  mov %rbp, %rsp
  pop %rbp
  ret
")
                 ("{ return 1>=1; }" . "  .globl main
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
  jmp .L.return
.L.return:
  mov %rbp, %rsp
  pop %rbp
  ret
")
                 ("{ return 1<=1; }" . "  .globl main
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
  jmp .L.return
.L.return:
  mov %rbp, %rsp
  pop %rbp
  ret
")
                 ("{ return 1<1; }" . "  .globl main
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
  jmp .L.return
.L.return:
  mov %rbp, %rsp
  pop %rbp
  ret
")
                 ("{ return 1>1; }" . "  .globl main
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
  jmp .L.return
.L.return:
  mov %rbp, %rsp
  pop %rbp
  ret
"))))
    (run-emit-code-tests tests)))

(deftest test-emit-multiple-statements
  (let ((tests '(("{ return 1;2;3; }" . "  .globl main
main:
  push %rbp
  mov %rsp, %rbp
  sub $0, %rsp
  mov $1, %rax
  jmp .L.return
  mov $2, %rax
  mov $3, %rax
.L.return:
  mov %rbp, %rsp
  pop %rbp
  ret
")
                 ("{ 1;return 2;3; }" . "  .globl main
main:
  push %rbp
  mov %rsp, %rbp
  sub $0, %rsp
  mov $1, %rax
  mov $2, %rax
  jmp .L.return
  mov $3, %rax
.L.return:
  mov %rbp, %rsp
  pop %rbp
  ret
")
                 ("{ 1;2;return 3; }" . "  .globl main
main:
  push %rbp
  mov %rsp, %rbp
  sub $0, %rsp
  mov $1, %rax
  mov $2, %rax
  mov $3, %rax
  jmp .L.return
.L.return:
  mov %rbp, %rsp
  pop %rbp
  ret
"))))
    (run-emit-code-tests tests)))

(deftest test-emit-x86-64-variables
  (let ((tests '(("{ a:=8;return a; }" . "  .globl main
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
  jmp .L.return
.L.return:
  mov %rbp, %rsp
  pop %rbp
  ret
")
                 ("{ foo:=5;bar:=8;return foo+bar; }" . "  .globl main
main:
  push %rbp
  mov %rsp, %rbp
  sub $16, %rsp
  lea -16(%rbp), %rax
  push %rax
  mov $5, %rax
  pop %rdi
  mov %rax, (%rdi)
  lea -8(%rbp), %rax
  push %rax
  mov $8, %rax
  pop %rdi
  mov %rax, (%rdi)
  lea -8(%rbp), %rax
  mov (%rax), %rax
  push %rax
  lea -16(%rbp), %rax
  mov (%rax), %rax
  pop %rdi
  add %rdi, %rax
  jmp .L.return
.L.return:
  mov %rbp, %rsp
  pop %rbp
  ret
"))))
    (run-emit-code-tests tests)))

(deftest test-emit-nested-compound-statements
  (let ((tests '(("{ 1; { 2; } return 3; }" . "  .globl main
main:
  push %rbp
  mov %rsp, %rbp
  sub $0, %rsp
  mov $1, %rax
  mov $2, %rax
  mov $3, %rax
  jmp .L.return
.L.return:
  mov %rbp, %rsp
  pop %rbp
  ret
"))))
    (run-emit-code-tests tests)))
