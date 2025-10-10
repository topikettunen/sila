(in-package #:sila)

(util:defstruct-read-only ast-node-block
  body)

(util:defstruct-read-only ast-node-negate
  value)

(util:defstruct-read-only ast-node-integer-literal
  (value :type integer))

(util:defstruct-read-only ast-node-variable
  (object :type object))

(util:defstruct-read-only ast-node-return
  expr)

(util:defstruct-read-only ast-node-assign
  (var :type ast-node-variable)
  expr)

(util:defstruct-read-only ast-node-cond
  expr
  then
  else)

(deftype binop-kind ()
  '(member
    :add
    :sub
    :mul
    :div
    :equal
    :not-equal
    :lesser-than
    :lesser-or-equal
    :greater-than
    :greater-or-equal))

(util:defstruct-read-only ast-node-binop
  (kind :type binop-kind)
  lhs
  rhs)

;;; TODO(topi): maybe this struct is slightly too general?
(util:defstruct-read-only ast-node-expression
  expr)

(util:defstruct-read-only ast-node-for
  init
  inc
  cond
  body)

(util:defstruct-read-only ast-node-loop
  body)

(util:defstruct-read-only ast-node-break
  (depth :type integer))

(defvar *local-variables* nil
  "Global variable for holding local variable objects.")

(defstruct (object
            (:copier nil))
  (name (util:required-argument 'name) :type string :read-only t)
  (offset 0 :type integer))

(util:defstruct-read-only func
  body
  locals
  (stack-size :type integer))

(defun parse-statement-node (tokens)
  (alexandria:switch ((token-literal (first tokens)) :test #'string=)
    ("return"
     (multiple-value-bind (node rest)
         (parse-expression-node (rest tokens))
       (values (make-ast-node-return :expr node)
               (rest (skip-to-token ";" rest)))))
    ("if"
     (parse-cond-statement-node (rest tokens)))
    ("for"
     (parse-for-statement-node (rest tokens)))
    ("loop"
     (parse-loop-statement-node (rest tokens)))
    ("break"
     (values (make-ast-node-break :depth *break-depth*)
             (rest (skip-to-token ";" tokens))))
    ("{"
     (parse-compound-statement-node (rest tokens)))
    (otherwise
     (parse-expression-statement-node tokens))))

(defun parse-cond-statement-node (tokens)
  ;; TOK passed in should be the token after "if".
  (let (expr then else)
    (multiple-value-bind (expr-node rest)
        (parse-expression-node tokens)
      (setf expr expr-node
            tokens rest))
    (multiple-value-bind (then-node rest)
        (parse-statement-node tokens)
      (setf then then-node
            tokens rest))
    (when (string= (token-literal (first tokens)) "else")
      (multiple-value-bind (else-node rest)
          (parse-statement-node (rest tokens))
        (setf else else-node
              tokens rest)))
    (values (make-ast-node-cond :expr expr
                                :then then
                                :else else)
            tokens)))

(defvar *break-depth* 0
  "Depth counter for BREAK keyword to know from what level it should break out.")

(defun parse-for-statement-node (tokens)
  ;; TOK passed in should be the token after "for"
  (let (init cond inc body)
    (multiple-value-bind (init-node rest)
        (parse-expression-statement-node tokens)
      (setf init init-node
            tokens rest))
    (unless (string= (token-literal (first tokens)) ";")
      (multiple-value-bind (cond-node rest)
          (parse-expression-node tokens)
        (setf cond cond-node
              tokens rest)))
    (setf tokens (skip-to-token ";" tokens))
    ;; Entering "for" scope.
    (incf *break-depth*)
    (unless (string= (token-literal (second tokens)) "{")
      (multiple-value-bind (inc-node rest)
          (parse-expression-node (rest tokens))
        (setf inc inc-node
              tokens rest)))
    (setf tokens (skip-to-token "{" tokens))
    (multiple-value-bind (body-node rest)
        (parse-statement-node tokens)
      (setf body body-node
            tokens rest))
    ;; Left "for" scope.
    (decf *break-depth*)
    (values (make-ast-node-for :init init
                               :cond cond
                               :inc inc
                               :body body)
            tokens)))

(defun parse-loop-statement-node (tokens)
  ;; TOK passed in should be the opening brace of the block after the "loop"
  ;; keyword.
  ;; TODO(topi): Add proper error handling.
  (assert (string= (token-literal (first tokens)) "{"))
  ;; Entering "loop" scope.
  (incf *break-depth*)
  (multiple-value-bind (body rest)
      (parse-statement-node tokens)
    ;; Left "loop" scope.
    (decf *break-depth*)
    (values (make-ast-node-loop :body body)
            rest)))

(defun parse-compound-statement-node (tokens)
  (let ((block-tree (list)))
    (loop :until (string= (token-literal (first tokens)) "}")
          :do (multiple-value-bind (node rest)
                  (parse-statement-node tokens)
                (push node block-tree)
                (setf tokens rest)))
    (values (make-ast-node-block :body (nreverse block-tree))
            (rest tokens))))

(defun parse-expression-statement-node (tokens)
  ;; Empty statement, e.g. ';;; return 3;'
  (when (string= (token-literal (first tokens)) ";")
    (return-from parse-expression-statement-node
      (values (make-ast-node-block :body nil)
              (rest tokens))))
  (multiple-value-bind (node rest)
      (parse-expression-node tokens)
    (values (make-ast-node-expression :expr node)
            (rest (skip-to-token ";" rest)))))

(defun parse-expression-node (tokens)
  (parse-assign-node tokens))

(defun parse-assign-node (tokens)
  (let (node)
    (multiple-value-bind (eql-node rest)
        (parse-equality-node tokens)
      (setf node eql-node
            tokens rest))
    (when (string= (token-literal (first tokens)) ":=")
      (multiple-value-bind (expr-node rest)
          (parse-assign-node (rest tokens))
        (setf node (make-ast-node-assign :var node
                                         :expr expr-node)
              tokens rest)))
    (values node tokens)))

(defmacro define-binop-parser (name &key descent-parser comparison-symbols)
  "Macro for generating new binary operator parser rules."
  (let ((parser-name (intern (format nil "PARSE-~a-NODE" name)))
        (descent-parser-name (intern (format nil "PARSE-~a-NODE" descent-parser))))
    `(defun ,parser-name (tokens)
       (let (node)
         (multiple-value-bind (lhs rest)
             (,descent-parser-name tokens)
           (setf node lhs)
           (loop
            (cond
              ,@(loop :for symbol :in comparison-symbols
                      :collect `((string= (token-literal (first rest)) ,(car symbol))
                                 (multiple-value-bind (rhs rest2)
                                     (,descent-parser-name (rest rest))
                                   (setf node (make-ast-node-binop
                                               :kind ,(cdr symbol)
                                               :lhs node
                                               :rhs rhs))
                                   (setf rest rest2))))
              (t
               (return-from ,parser-name
                 (values node rest))))))))))

(define-binop-parser equality
  :descent-parser relational
  :comparison-symbols (("==" . :equal)
                       ("!=" . :not-equal)))

(define-binop-parser relational
  :descent-parser add
  :comparison-symbols (("<" . :lesser-than)
                       ("<=" . :lesser-or-equal)
                       (">" . :greater-than)
                       (">=" . :greater-or-equal)))

(define-binop-parser add
  :descent-parser multiplicative
  :comparison-symbols (("+" . :add)
                       ("-" . :sub)))

(define-binop-parser multiplicative
  :descent-parser unary
  :comparison-symbols (("*" . :mul)
                       ("/" . :div)))

(defun parse-unary-node (tokens)
  (alexandria:switch ((token-literal (first tokens)) :test #'string=)
    ("+"
     (parse-unary-node (rest tokens)))
    ("-"
     (multiple-value-bind (node rest)
         (parse-unary-node (rest tokens))
       (values (make-ast-node-negate :value node)
               rest)))
    (t
     (parse-primary-node tokens))))

(defun parse-primary-node (tokens)
  (flet ((find-local-var (name)
           (loop :for obj :in *local-variables*
                 :do (when (string= name (object-name obj))
                       (return-from find-local-var obj)))))
    (cond
      ((eq (token-kind (first tokens)) :num)
       (values (make-ast-node-integer-literal
                :value (parse-integer (token-literal (first tokens))))
               (rest tokens)))
      ((eq (token-kind (first tokens)) :ident)
       (let* ((name (token-literal (first tokens)))
              (var (find-local-var name)))
         (when (null var)
           (let ((new-obj (make-object :name name)))
             (setf var new-obj)
             ;; New object needs to be in the front.
             (push new-obj *local-variables*)))
         (values (make-ast-node-variable :object var)
                 (rest tokens))))
      ((string= (token-literal (first tokens)) "(")
       (multiple-value-bind (node rest)
           (parse-expression-node (rest tokens))
         (values node (rest (skip-to-token ")" rest)))))
      (t (error "Unexpected token value: ~a" tokens)))))

(defun parse-program (tokens)
  ;; Expect list of tokens to end in EOF
  (assert (eq (token-kind (util:lastcar tokens)) :eof))
  (labels ((align-to (n align)
             "Round N to the nearest multiple of ALIGN."
             (* (ceiling n align) align))
           (set-lvar-offsets (locals)
             (let ((offset 0))
               (loop :for obj :in locals
                     :do (progn
                           (incf offset 8)
                           (setf (object-offset obj) (- offset))))
               (align-to offset 16))))
    (let* ((parse-tree (list)))
      (loop :until (eq (token-kind (first tokens)) :eof)
            :do (multiple-value-bind (node rest)
                    (parse-statement-node tokens)
                  (push node parse-tree)
                  (setf tokens rest)))
      (let ((stack-size (set-lvar-offsets *local-variables*)))
        (make-func :body (nreverse parse-tree)
                   :locals *local-variables*
                   :stack-size stack-size)))))

;;;
;;; Parser Utilities
;;;

(defun skip-to-token (val tokens)
  "Search for token which is VAL. Returns NIL when not found."
  (let ((toks tokens))
    (loop :until (eq (token-kind (first toks)) :eof)
          :do (if (string= (token-literal (first toks)) val)
                  (return-from skip-to-token toks)
                  (setf toks (rest toks))))))
