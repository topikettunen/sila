(defpackage #:sila/codegen
  (:use #:cl)
  (:import-from #:alexandria
                #:appendf
                #:flatten)
  (:import-from #:sila/lexer
                #:tokenize
                #:token-kind)
  (:import-from #:sila/parser
                #:ast-node-kind
                #:ast-node-value
                #:ast-node-variable
                #:ast-node-lhs
                #:ast-node-rhs
                #:ast-node-next
                #:object-offset
                #:func-body
                #:func-stack-size
                #:parse-program)
  (:export #:emit-code))
(in-package #:sila/codegen)

(defparameter *stack-depth* 0)

(defun asm-inst (inst)
  (list inst))

(defun asm-push ()
  (incf *stack-depth*)
  (asm-inst "push %rax"))

(defun asm-pop (reg)
  (assert (> *stack-depth* 0))
  (decf *stack-depth*)
  (asm-inst (format nil "pop %~a" reg)))

(defun generate-statement (node &optional (insts '()))
  (ecase (ast-node-kind node)
    (:return-statement
     (appendf insts (generate-code (ast-node-lhs node)))
     (appendf insts (asm-inst "jmp .L.return"))
     insts)
    (:expression-statement
     (generate-code (ast-node-lhs node)))))

(defun generate-address (node)
  (if (eql (ast-node-kind node) :variable)
      (asm-inst (format nil
                        "lea ~a(%rbp), %rax"
                        (object-offset (ast-node-variable node))))
      (error "Expected lvalue, got: ~a" node)))

(defun generate-code (node &optional (insts '()))
  "Recursively generate the x86-64 assembly code."
  (let ((kind (ast-node-kind node)))
    ;; TODO(topi): Lots of `appendf' here, maybe those could be cleaned
    ;; somehow.
    (case kind
      (:number
       (appendf insts (asm-inst (format nil "mov $~d, %rax"
                                        (ast-node-value node))))
       (return-from generate-code insts))
      (:neg
       (appendf insts (generate-code (ast-node-lhs node)))
       (appendf insts (asm-inst "neg %rax"))
       (return-from generate-code insts))
      (:variable
       (appendf insts (generate-address node))
       (appendf insts (asm-inst "mov (%rax), %rax"))
       (return-from generate-code insts))
      (:assign
       (appendf insts (generate-address (ast-node-lhs node)))
       (appendf insts (asm-push))
       (appendf insts (generate-code (ast-node-rhs node)))
       (appendf insts (asm-pop "rdi"))
       (appendf insts (asm-inst "mov %rax, (%rdi)"))
       (return-from generate-code insts))
      (otherwise (values)))
    (appendf insts (generate-code (ast-node-rhs node)))
    (appendf insts (asm-push))
    (appendf insts (generate-code (ast-node-lhs node)))
    (appendf insts (asm-pop "rdi"))
    (ecase kind
      (:add
       (appendf insts (asm-inst "add %rdi, %rax")))
      (:sub
       (appendf insts (asm-inst "sub %rdi, %rax")))
      (:mul
       (appendf insts (asm-inst "imul %rdi, %rax")))
      (:div
       (appendf insts (asm-inst "cqo"))
       (appendf insts (asm-inst "idiv %rdi, %rax")))
      ((:equal
        :not-equal
        :lesser-than
        :lesser-or-equal
        :greater-than
        :greater-or-equal)
       (appendf insts (asm-inst "cmp %rdi, %rax"))
       (case kind
         (:equal
          (appendf insts (asm-inst "sete %al")))
         (:not-equal
          (appendf insts (asm-inst "setne %al")))
         (:lesser-than
          (appendf insts (asm-inst "setl %al")))
         (:lesser-or-equal
          (appendf insts (asm-inst "setle %al")))
         (:greater-than
          (appendf insts (asm-inst "setg %al")))
         (:greater-or-equal
          (appendf insts (asm-inst "setge %al")))
         (otherwise (values)))
       (appendf insts (asm-inst "movzb %al, %rax"))))))

(defun emit-code (src &key (stream nil) (indent 2) (indent-tabs t))
  "Emit assembly code from given source code. Currently emits only x86-64 and
only Linux is tested."
  ;; Init environment.
  ;; TODO(topi): These should probably be set in some smarter place.
  (setf sila/parser::*local-variables* nil
        sila/codegen::*stack-depth* 0)
  (let ((indent (if indent-tabs
                    #\Tab
                    (coerce (make-list indent
                                       :initial-element #\Space)
                            'string))))
    (let ((program (parse-program (tokenize src))))
      ;; TODO(topi): these instructions probably should be collected to some
      ;; structure so they can be divided in to sections more easily when the
      ;; programs become more complex.
      (format stream
              "~{~a~%~}"
              (flatten
               (list
                ;; ASM Directive
                (format nil "~a.globl main" indent)
                ;; Main Label
                "main:"
                ;; Prologue
                (format nil "~apush %rbp" indent)
                (format nil "~amov %rsp, %rbp" indent)
                (format nil "~asub $~a, %rsp" indent (func-stack-size program))
                ;; ASM Routine
                (loop :for node := (func-body program)
                        :then (setf node (ast-node-next node))
                      :until (null node)
                      :append (loop :for inst :in (generate-statement node)
                                    :do (assert (= 0 *stack-depth*))
                                    :collect (format nil "~a~a" indent inst)))
                ;; Return label
                ".L.return:"
                ;; Epilogue
                (format nil "~amov %rbp, %rsp" indent)
                (format nil "~apop %rbp" indent)
                ;; Return
                (format nil "~aret" indent)))))))
