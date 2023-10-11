(defpackage #:sila/codegen
  (:use #:cl)
  (:import-from #:alexandria
                #:appendf
                #:flatten)
  (:import-from #:sila/conditions
                #:parser-error)
  (:import-from #:sila/lexer
                #:tokenize
                #:token-kind)
  (:import-from #:sila/parser
                #:ast-node-kind
                #:ast-node-value
                #:ast-node-variable
                #:ast-node-lhs
                #:ast-node-rhs
                #:object-offset
                #:func-body
                #:func-stack-size
                #:parse-program)
  (:export #:emit-asm))
(in-package #:sila/codegen)

(defparameter *stack-depth* 0)

(defun asm-inst (inst)
  (list inst))

(defun asm-push ()
  (incf *stack-depth*)
  (asm-inst "push %rax"))

(defun asm-pop (reg)
  (decf *stack-depth*)
  (asm-inst (format nil "pop %~a" reg)))

(defun generate-expr (node &optional (insts '()))
  "Recursively generate the x86-64 assembly code."
  (let ((kind (ast-node-kind node)))
    ;; TODO(topi): Lots of `appendf' here, maybe those could be cleaned
    ;; somehow.
    (cond ((eq kind :number)
           (appendf insts (asm-inst (format nil "mov $~d, %rax"
                                            (ast-node-value node))))
           (return-from generate-expr insts))
          ((eq kind :neg)
           (appendf insts (generate-expr (ast-node-lhs node)))
           (appendf insts (asm-inst "neg %rax"))
           (return-from generate-expr insts))
          ((eq kind :variable)
           (appendf insts (generate-address node))
           (appendf insts (asm-inst "mov (%rax), %rax"))
           (return-from generate-expr insts))
          ((eq kind :assign)
           (appendf insts (generate-address (ast-node-lhs node)))
           (appendf insts (asm-push))
           (appendf insts (generate-expr (ast-node-rhs node)))
           (appendf insts (asm-pop "rdi"))
           (appendf insts (asm-inst "mov %rax, (%rdi)"))
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

(defun generate-statement (node)
  (if (eq (ast-node-kind node) :expression-statement)
      (generate-expr (ast-node-lhs node))
      (error 'parser-error :error-msg "Invalid statement.")))

(defun generate-address (node)
  (if (eq (ast-node-kind node) :variable)
      (asm-inst (format nil
                        "lea ~a(%rbp), %rax"
                        (object-offset (ast-node-variable node))))
      (error 'parser-error :error-msg "Not an lvalue.")))

(defun emit-asm (src &key (print-to-stdout nil) (indent 2) (indent-tabs t))
  "Emit assembly code from given source code. Currently emits only x86-64 and
only Linux is tested."
  ;; Init environment.
  ;; TODO(topi): These should probably be set in some smarter place.
  (setf sila/parser::*local-variables* '()
        sila/codegen::*stack-depth* 0)
  (let ((indent (if indent-tabs
                    #\Tab
                    (coerce (make-list indent
                                       :initial-element #\Space)
                            'string))))
    (let ((prog (parse-program (tokenize src))))
      (format print-to-stdout
              "~{~a~%~}"
              (flatten
               (list
                ;; ASM Directive
                (format nil "~a.globl main" indent)
                ;; ASM Label
                "main:"
                ;; Prologue
                (format nil "~apush %rbp" indent)
                (format nil "~amov %rsp, %rbp" indent)
                (format nil "~asub $~a, %rsp" indent (func-stack-size prog))
                ;; ASM Routine
                (loop :for node :in (func-body prog)
                      :append (loop :for inst :in (generate-statement node)
                                    :collect (format nil "~a~a" indent inst)
                                    :do (unless (= 0 *stack-depth*)
                                          (error 'parser-error :error-msg "Stack depth not 0."))))
                ;; Epilogue
                (format nil "~amov %rbp, %rsp" indent)
                (format nil "~apop %rbp" indent)
                ;; Return
                (format nil "~aret" indent)))))))
