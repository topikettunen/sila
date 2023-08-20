(defpackage #:sila
  (:use #:cl)
  (:import-from #:sila/conditions
                #:lexer-error)
  (:import-from #:sila/lexer
                #:tokenize
                #:token-kind
                #:token-val
                #:token-next)
  (:export #:main))
(in-package #:sila)

(defun main ()
  "Sila programming language entrypoint."
  (multiple-value-bind (options free-args)
      (opts:get-opts)
    ;; TODO(topi): Add some useful options.
    (declare (ignore options))
    (unless (= (length free-args) 1)
      (format *error-output* "Invalid number of arguments, expected 1.~%")
      (opts:exit 1))
    (format *standard-output* (emit-asm (first free-args)))
    (opts:exit)))

(defun emit-asm (src)
  "Emit assembly code from given source code. Currently emits only x86-64 and only Linux
is tested."
  (let ((asm "")
        (tok (tokenize src)))
    (labels ((asm-conc (inst)
               (setf asm (concatenate 'string asm inst)))
             (asm-inst (inst)
               (format nil "  ~A~%" inst))
             (asm-directive (dir)
               (asm-inst dir))
             (asm-label (label)
               (format nil "~A:~%" label)))
      (asm-conc (asm-directive ".globl main"))
      (asm-conc (asm-label "main"))
      ;; For now, first token must be a number.
      (asm-conc (asm-inst (format nil "mov $~d, %rax" (token-val tok))))
      (setf tok (token-next tok))
      (loop :until (eq (token-kind tok) :eof)
            :do (cond ((eq (token-kind tok) :num)
                       (setf tok (token-next tok)))
                      ((char= (token-val tok) #\+)
                       (asm-conc (asm-inst
                                  (format nil "add $~d, %rax"
                                          (token-val (token-next tok)))))
                       (setf tok (token-next tok)))
                      ((char= (token-val tok) #\-)
                       (asm-conc (asm-inst
                                  (format nil "sub $~d, %rax"
                                          (token-val (token-next tok)))))
                       (setf tok (token-next tok)))))
      (asm-conc (asm-inst "ret"))
      asm)))
