(defpackage #:sila/codegen
  (:use #:cl)
  (:local-nicknames
   (#:lex #:sila/lexer)
   (#:parser #:sila/parser))
  (:export #:emit-code))
(in-package #:sila/codegen)

(defparameter *stack-depth* 0)

(defun asm-push ()
  (incf *stack-depth*)
  (format nil "push %rax"))

(defun asm-pop (reg)
  (assert (> *stack-depth* 0))
  (decf *stack-depth*)
  (format nil "pop %~a" reg))

(defmacro do-vector-push-inst (generator insts)
  `(loop :for inst :across ,generator :do (vector-push-extend inst ,insts)))

(defun make-inst-array ()
  (make-array 0 :adjustable t :fill-pointer 0))

(defparameter *label-count* 0
  "Global counter for to be used in ASM labels.")

(defun generate-statement (node)
  (let ((insts (make-inst-array)))
    (cond
      ((parser:ast-node-block-p node)
       (loop :for body := (parser:ast-node-block-body node)
               :then (setf body (parser:next-node body))
             :until (null body)
             :do (do-vector-push-inst (generate-statement body) insts))
       insts)

      ((parser:ast-node-return-p node)
       (do-vector-push-inst (generate-expression
                             (parser:ast-node-return-expr node)) insts)
       (vector-push-extend (format nil "jmp .L.return") insts)
       insts)

      ((parser:ast-node-break-p node)
       (vector-push-extend (format nil "jmp .L.end.~d"
                                   (parser:ast-node-break-depth node)) insts)
       insts)

      ((parser:ast-node-cond-p node)
       (incf *label-count*)

       (let ((count *label-count*))
         (do-vector-push-inst (generate-expression
                               (parser:ast-node-cond-expr node)) insts)
         (vector-push-extend (format nil "cmp $0, %rax") insts)
         (vector-push-extend (format nil "je .L.else.~d" count) insts)

         (do-vector-push-inst (generate-statement
                               (parser:ast-node-cond-then node)) insts)
         (vector-push-extend (format nil "jmp .L.end.~d" count) insts)

         (vector-push-extend (format nil ".L.else.~d:" count) insts)
         (if (parser:ast-node-cond-else node)
             (do-vector-push-inst (generate-statement
                                   (parser:ast-node-cond-else node)) insts)
             (vector-push-extend (format nil "nop") insts))

         (vector-push-extend (format nil ".L.end.~d:" count) insts)

         (unless (parser:next-node node)
           (vector-push-extend (format nil "nop") insts)))
       insts)

      ((parser:ast-node-for-p node)
       (incf *label-count*)

       (let ((count *label-count*))
         (do-vector-push-inst (generate-statement
                               (parser:ast-node-for-init node)) insts)
         (vector-push-extend (format nil ".L.begin.~d:" count) insts)

         (when (parser:ast-node-for-cond node)
           (do-vector-push-inst (generate-expression
                                 (parser:ast-node-for-cond node)) insts)
           (vector-push-extend (format nil "cmp $0, %rax") insts)
           (vector-push-extend (format nil "je .L.end.~d" count) insts))

         (do-vector-push-inst (generate-statement
                               (parser:ast-node-for-body node)) insts)

         (when (parser:ast-node-for-inc node)
           (do-vector-push-inst (generate-expression
                                 (parser:ast-node-for-inc node)) insts))

         (vector-push-extend (format nil "jmp .L.begin.~d" count) insts)
         (vector-push-extend (format nil ".L.end.~d:" count) insts)

         (unless (parser:next-node node)
           (vector-push-extend (format nil "nop") insts)))
       insts)

      ((parser:ast-node-loop-p node)
       (incf *label-count*)

       (let ((count *label-count*))
         (vector-push-extend (format nil ".L.begin.~d:" count) insts)

         (do-vector-push-inst (generate-statement
                               (parser:ast-node-loop-body node)) insts)

         (vector-push-extend (format nil "jmp .L.begin.~d" count) insts)
         (vector-push-extend (format nil ".L.end.~d:" count) insts)

         (unless (parser:next-node node)
           (vector-push-extend (format nil "nop") insts)))
       insts)

      ((parser:ast-node-expression-p node)
       (do-vector-push-inst (generate-expression
                             (parser:ast-node-expression-expr node)) insts)
       insts))))

(defun generate-address (node)
  (if (parser:ast-node-variable-p node)
      (format nil "lea ~d(%rbp), %rax"
              (parser:object-offset (parser:ast-node-variable-object node)))
      (error "Expected lvalue, got: ~a" node)))

(defun generate-expression (node)
  "Recursively generate the x86-64 assembly code."
  (let ((insts (make-inst-array)))
    (cond
      ((parser:ast-node-integer-literal-p node)
       (vector-push-extend
        (format nil "mov $~d, %rax"
                (parser:ast-node-integer-literal-value node)) insts)
       insts)

      ((parser:ast-node-negate-p node)
       (do-vector-push-inst (generate-expression
                             (parser:ast-node-negate-value node)) insts)
       (vector-push-extend (format nil "neg %rax") insts)
       insts)

      ((parser:ast-node-variable-p node)
       (vector-push-extend (generate-address node) insts)
       (vector-push-extend (format nil "mov (%rax), %rax") insts)
       insts)

      ((parser:ast-node-assign-p node)
       (vector-push-extend (generate-address
                            (parser:ast-node-assign-var node)) insts)
       (vector-push-extend (asm-push) insts)
       (do-vector-push-inst (generate-expression
                             (parser:ast-node-assign-expr node)) insts)
       (vector-push-extend (asm-pop "rdi") insts)
       (vector-push-extend (format nil "mov %rax, (%rdi)") insts)
       insts)

      ((parser:ast-node-binop-p node)
       (generate-binop-expression node)))))

(defun generate-binop-expression (node)
  (let ((insts (make-inst-array)))
    (do-vector-push-inst (generate-expression
                          (parser:ast-node-binop-rhs node)) insts)
    (vector-push-extend (asm-push) insts)
    (do-vector-push-inst (generate-expression
                          (parser:ast-node-binop-lhs node)) insts)
    (vector-push-extend (asm-pop "rdi") insts)
    (let ((kind (parser:ast-node-binop-kind node)))
      (ecase kind
        (:add
         (vector-push-extend (format nil "add %rdi, %rax") insts))

        (:sub
         (vector-push-extend (format nil "sub %rdi, %rax") insts))

        (:mul
         (vector-push-extend (format nil "imul %rdi, %rax") insts))

        (:div
         (vector-push-extend (format nil "cqo") insts)
         (vector-push-extend (format nil "idiv %rdi, %rax") insts))

        ((:equal
          :not-equal
          :lesser-than
          :lesser-or-equal
          :greater-than
          :greater-or-equal)
         (vector-push-extend (format nil "cmp %rdi, %rax") insts)
         (case kind
           (:equal
            (vector-push-extend (format nil "sete %al") insts))

           (:not-equal
            (vector-push-extend (format nil "setne %al") insts))

           (:lesser-than
            (vector-push-extend (format nil "setl %al") insts))

           (:lesser-or-equal
            (vector-push-extend (format nil "setle %al") insts))

           (:greater-than
            (vector-push-extend (format nil "setg %al") insts))

           (:greater-or-equal
            (vector-push-extend (format nil "setge %al") insts))

           (otherwise (values)))
         (vector-push-extend (format nil "movzb %al, %rax") insts))))
    insts))

(defun emit-code (src &key (stream nil) (indent 2) (indent-tabs t))
  "Emit assembly code from given source code. Currently emits only x86-64 and
only Linux is tested."
  ;; Init environment
  (setf parser:*local-variables* nil
        *stack-depth* 0
        *label-count* 0)

  (let ((indent (if indent-tabs
                    #\Tab
                    (coerce (make-list indent
                                       :initial-element #\Space)
                            'string))))

    (let ((program (parser:parse-program (lex:tokenize src))))

      ;; TODO(topi): these instructions probably should be collected to some
      ;; structure so they can be divided in to sections more easily when the
      ;; programs become more complex.
      (format stream
              "~{~a~%~}"
              (alexandria:flatten
               (list
                ;; ASM Directive
                (format nil "~a.globl main" indent)

                ;; Main Label
                "main:"

                ;; Prologue
                (format nil "~apush %rbp" indent)
                (format nil "~amov %rsp, %rbp" indent)
                (format nil "~asub $~a, %rsp" indent
                        (parser:func-stack-size program))

                ;; ASM Routine
                (loop :for inst
                        :across (generate-statement (parser:func-body program))
                      :collect (if (string= (subseq inst 0 3) ".L.")
                                   ;; If instruction is label (.L. prefix),
                                   ;; don't indent it.
                                   (format nil "~a" inst)
                                   (format nil "~a~a" indent inst)))

                ;; Return label
                ".L.return:"

                ;; Epilogue
                (format nil "~amov %rbp, %rsp" indent)
                (format nil "~apop %rbp" indent)

                ;; Return
                (format nil "~aret" indent)))))))
