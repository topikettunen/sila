(defpackage #:sila/codegen
  (:use #:cl)
  (:import-from #:alexandria
                #:appendf)
  (:import-from #:sila/conditions
                #:parser-error)
  (:import-from #:sila/parser
                #:ast-node-kind
                #:ast-node-val
                #:ast-node-lhs
                #:ast-node-rhs)
  (:export #:generate-expr))
(in-package #:sila/codegen)

(defun asm-inst (inst)
  (list inst))

(defun asm-push ()
  (asm-inst "push %rax"))

(defun asm-pop (reg)
  (asm-inst (format nil "pop %~a" reg)))

(defun generate-expr (node &optional (insts '()))
  "Recursively generate the x86-64 assembly code."
  (let ((kind (ast-node-kind node)))
    (cond ((eq kind :number)
           (appendf insts (asm-inst (format nil "mov $~d, %rax"
                                            (ast-node-val node))))
           (return-from generate-expr insts))
          ((eq kind :neg)
           (appendf insts (generate-expr (ast-node-lhs node)))
           (appendf insts (asm-inst "neg %rax"))
           (return-from generate-expr insts)))
    (appendf insts (generate-expr (ast-node-rhs node)))
    (appendf insts (asm-push))
    (appendf insts (generate-expr (ast-node-lhs node)))
    (appendf insts (asm-pop "rdi"))
    (cond ((eq kind :add)
           (appendf insts (asm-inst "add %rdi, %rax")))
          ((eq kind :sub)
           (appendf insts (asm-inst "sub %rdi, %rax")))
          ((eq kind :mul)
           (appendf insts (asm-inst "imul %rdi, %rax")))
          ((eq kind :div)
           (appendf insts (asm-inst "cqo"))
           (appendf insts (asm-inst "idiv %rdi, %rax")))
          ((or (eq kind :equal)
               (eq kind :not-equal)
               (eq kind :lesser-than)
               (eq kind :lesser-or-equal)
               (eq kind :greater-than)
               (eq kind :greater-or-equal))
           (appendf insts (asm-inst "cmp %rdi, %rax"))
           (cond ((eq kind :equal)
                  (appendf insts (asm-inst "sete %al")))
                 ((eq kind :not-equal)
                  (appendf insts (asm-inst "setne %al")))
                 ((eq kind :lesser-than)
                  (appendf insts (asm-inst "setl %al")))
                 ((eq kind :lesser-or-equal)
                  (appendf insts (asm-inst "setle %al")))
                 ((eq kind :greater-than)
                  (appendf insts (asm-inst "setg %al")))
                 ((eq kind :greater-or-equal)
                  (appendf insts (asm-inst "setge %al"))))
           (appendf insts (asm-inst "movzb %al, %rax")))
          (t (error 'parser-error)))))
