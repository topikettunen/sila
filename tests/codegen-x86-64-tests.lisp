(defpackage #:sila/tests/codegen
  (:use #:cl
        #:sila
        #:rove))
(in-package #:sila/tests/codegen)

;; NOTE: To run this test file, execute `(asdf:test-system :sila)' in your Lisp.

(defmacro testing-codegen (desc testcases)
  `(testing ,desc
     ,@(loop :for test :in testcases
             :collect `(ok (string= (sila/codegen:emit-code ,(car test)
                                                            :indent 2
                                                            :indent-tabs nil)
                                    ,(cdr test))
                           ,(car test)))))

(deftest test-codegen-x86-64
  (testing-codegen
   "Integer"
   (("{ return 0; }" . "  .globl main
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
")))

  (testing-codegen
   "Add and subtraction"
   (("{ return 5+20-4; }" . "  .globl main
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
")))

  (testing-codegen
   "Division and multiplication"
   (("{ return 2 / (1 + 1) * 8; }" . "  .globl main
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
")))

  (testing-codegen
   "Unary"
   (("{ return - -10; }" . "  .globl main
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
")))

  (testing-codegen
   "Comparison"
   (("{ return 1==1; }" . "  .globl main
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
")))

  (testing-codegen
   "Multiple statements"
   (("{ return 1;2;3; }" . "  .globl main
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
")))

  (testing-codegen
   "Variables"
   (("{ a:=8;return a; }" . "  .globl main
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
")))

  (testing-codegen
   "Block"
   (("{ 1; { 2; } return 3; }" . "  .globl main
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
")))

  (testing-codegen
   "Conditional"
   (("{ if 0 { return 1; } else { return 2; } }" . "  .globl main
main:
  push %rbp
  mov %rsp, %rbp
  sub $0, %rsp
  mov $0, %rax
  cmp $0, %rax
  jne .L.else.1
  mov $1, %rax
  jmp .L.return
  jmp .L.end.1
.L.else.1:
  mov $2, %rax
  jmp .L.return
.L.end.1:
.L.return:
  mov %rbp, %rsp
  pop %rbp
  ret
")))

  (testing-codegen
   "For loop"
   (("{ for ;; { return 3; } return 5; }" . "  .globl main
main:
  push %rbp
  mov %rsp, %rbp
  sub $0, %rsp
.L.begin.1:
  mov $3, %rax
  jmp .L.return
  jmp .L.begin.1
.L.end.1:
  mov $5, %rax
  jmp .L.return
.L.return:
  mov %rbp, %rsp
  pop %rbp
  ret
")
    ("{ i:=0; j:=0;for i:=0; i<=10; i:=i+1 {j := i+j;} return j; }" . "  .globl main
main:
  push %rbp
  mov %rsp, %rbp
  sub $16, %rsp
  lea -16(%rbp), %rax
  push %rax
  mov $0, %rax
  pop %rdi
  mov %rax, (%rdi)
  lea -8(%rbp), %rax
  push %rax
  mov $0, %rax
  pop %rdi
  mov %rax, (%rdi)
  lea -16(%rbp), %rax
  push %rax
  mov $0, %rax
  pop %rdi
  mov %rax, (%rdi)
.L.begin.1:
  mov $10, %rax
  push %rax
  lea -16(%rbp), %rax
  mov (%rax), %rax
  pop %rdi
  cmp %rdi, %rax
  setle %al
  movzb %al, %rax
  cmp $0, %rax
  je .L.end.1
  lea -8(%rbp), %rax
  push %rax
  lea -8(%rbp), %rax
  mov (%rax), %rax
  push %rax
  lea -16(%rbp), %rax
  mov (%rax), %rax
  pop %rdi
  add %rdi, %rax
  pop %rdi
  mov %rax, (%rdi)
  lea -16(%rbp), %rax
  push %rax
  mov $1, %rax
  push %rax
  lea -16(%rbp), %rax
  mov (%rax), %rax
  pop %rdi
  add %rdi, %rax
  pop %rdi
  mov %rax, (%rdi)
  jmp .L.begin.1
.L.end.1:
  lea -8(%rbp), %rax
  mov (%rax), %rax
  jmp .L.return
.L.return:
  mov %rbp, %rsp
  pop %rbp
  ret
"))))
