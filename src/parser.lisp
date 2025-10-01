(defpackage #:sila/parser
  (:use #:cl)
  (:local-nicknames
   (#:lex #:sila/lexer)
   (#:util #:serapeum/bundle))
  (:export #:ast-node-block-p
           #:ast-node-block-body
           #:ast-node-block-next
           #:ast-node-integer-literal-p
           #:ast-node-integer-literal-value
           #:ast-node-negate-p
           #:ast-node-negate-value
           #:ast-node-variable-p
           #:ast-node-variable-object
           #:ast-node-return-p
           #:ast-node-return-expr
           #:ast-node-assign-p
           #:ast-node-assign-var
           #:ast-node-assign-expr
           #:ast-node-cond-p
           #:ast-node-cond-expr
           #:ast-node-cond-then
           #:ast-node-cond-else
           #:ast-node-binop-p
           #:ast-node-binop-kind
           #:ast-node-binop-lhs
           #:ast-node-binop-rhs
           #:ast-node-expression-p
           #:ast-node-expression-expr
           #:ast-node-for-p
           #:ast-node-for-init
           #:ast-node-for-cond
           #:ast-node-for-inc
           #:ast-node-for-body
           #:ast-node-loop-p
           #:ast-node-loop-body
           #:ast-node-break-p
           #:ast-node-break-depth
           #:*local-variables*
           #:object-offset
           #:func-body
           #:func-stack-size
           #:next-node
           #:parse-program))
(in-package #:sila/parser)

(defstruct (ast-node
            (:copier nil))
  (next nil :type t))

(defgeneric next-node (node))

(defstruct (ast-node-block
            (:include ast-node)
            (:copier nil))
  (body (util:required-argument 'body) :type t :read-only t))

(defmethod next-node ((node ast-node-block))
  (ast-node-block-next node))

(defstruct (ast-node-negate
            (:include ast-node)
            (:copier nil))
  (value (util:required-argument 'value) :type t :read-only t))

(defmethod next-node ((node ast-node-negate))
  (ast-node-negate-next node))

(defstruct (ast-node-integer-literal
            (:include ast-node)
            (:copier nil))
  (value (util:required-argument 'value) :type integer :read-only t))

(defmethod next-node ((node ast-node-integer-literal))
  (ast-node-integer-literal-next node))

(defstruct (ast-node-variable
            (:include ast-node)
            (:copier nil))
  (object (util:required-argument 'object) :type object :read-only t))

(defmethod next-node ((node ast-node-variable))
  (ast-node-variable-next node))

(defstruct (ast-node-return
            (:include ast-node)
            (:copier nil))
  (expr (util:required-argument 'expr) :type t :read-only t))

(defmethod next-node ((node ast-node-return))
  (ast-node-return-next node))

(defstruct (ast-node-assign
            (:include ast-node)
            (:copier nil))
  (var (util:required-argument 'var) :type ast-node-variable :read-only t)
  (expr (util:required-argument 'expr) :type t :read-only t))

(defmethod next-node ((node ast-node-assign))
  (ast-node-assign-next node))

(defstruct (ast-node-cond
            (:include ast-node)
            (:copier nil))
  (expr (util:required-argument 'expr) :type t :read-only t)
  (then (util:required-argument 'then) :type t :read-only t)
  (else (util:required-argument 'else) :type t :read-only t))

(defmethod next-node ((node ast-node-cond))
  (ast-node-cond-next node))

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

(defstruct (ast-node-binop
            (:include ast-node)
            (:copier nil))
  (kind (util:required-argument 'kind) :type binop-kind :read-only t)
  (lhs (util:required-argument 'lhs) :type t :read-only t)
  (rhs (util:required-argument 'rhs) :type t :read-only t))

(defmethod next-node ((node ast-node-binop))
  (ast-node-binop-next node))

;;; TODO(topi): maybe this struct is slightly too general?
(defstruct (ast-node-expression
            (:include ast-node)
            (:copier nil))
  (expr (util:required-argument 'expr) :type t :read-only t))

(defmethod next-node ((node ast-node-expression))
  (ast-node-expression-next node))

(defstruct (ast-node-for
             (:include ast-node)
             (:copier nil))
  (init (util:required-argument 'init) :type t :read-only t)
  (inc (util:required-argument 'inc) :type t :read-only t)
  (cond (util:required-argument 'cond) :type t :read-only t)
  (body (util:required-argument 'body) :type t :read-only t))

(defmethod next-node ((node ast-node-for))
  (ast-node-for-next node))

(defstruct (ast-node-loop
             (:include ast-node)
             (:copier nil))
  (body (util:required-argument 'body) :type t :read-only t))

(defmethod next-node ((node ast-node-loop))
  (ast-node-loop-next node))

(defstruct (ast-node-break
             (:include ast-node)
             (:copier nil))
  (depth (util:required-argument 'depth) :type integer :read-only t))

(defmethod next-node ((node ast-node-break))
  (ast-node-break-next node))

(defvar *local-variables* nil
  "Global variable for holding local variable objects.")

(defstruct (object
            (:copier nil))
  (name (util:required-argument 'name) :type string :read-only t)
  (offset 0 :type integer)
  (next nil :type t))

(defstruct (func
            (:copier nil))
  (body (util:required-argument 'body) :type t :read-only t)
  (locals (util:required-argument 'locals) :type t :read-only t)
  (stack-size 0 :type integer))

(defun parse-statement-node (tok)
  (alexandria:switch ((lex:token-value tok) :test #'string=)
    ("return"
     (multiple-value-bind (node rest)
         (parse-expression-node (lex:token-next tok))
       (values (make-ast-node-return :expr node)
               (lex:token-next (skip-to-token ";" rest)))))
    ("if"
     (parse-cond-statement-node (lex:token-next tok)))
    ("for"
     (parse-for-statement-node (lex:token-next tok)))
    ("loop"
     (parse-loop-statement-node (lex:token-next tok)))
    ("break"
     (values (make-ast-node-break :depth *break-depth*)
             (lex:token-next (skip-to-token ";" tok))))
    ("{"
     (parse-compound-statement-node (lex:token-next tok)))
    (otherwise
     (parse-expression-statement-node tok))))

(defun parse-cond-statement-node (tok)
  ;; TOK passed in should be the token after "if".
  (let (expr then else)
    (multiple-value-bind (expr-node rest)
        (parse-expression-node tok)
      (setf expr expr-node
            tok rest))
    (multiple-value-bind (then-node rest)
        (parse-statement-node tok)
      (setf then then-node
            tok rest))
    (when (string= (lex:token-value tok) "else")
      (multiple-value-bind (else-node rest)
          (parse-statement-node (lex:token-next tok))
        (setf else else-node
              tok rest)))
    (values (make-ast-node-cond :expr expr
                                :then then
                                :else else)
            tok)))

(defvar *break-depth* 0
  "Depth counter for BREAK keyword to know from what level it should break out.")

(defun parse-for-statement-node (tok)
  ;; TOK passed in should be the token after "for"
  (let (init cond inc body)
    (multiple-value-bind (init-node rest)
        (parse-expression-statement-node tok)
      (setf init init-node
            tok rest))
    (unless (string= (lex:token-value tok) ";")
      (multiple-value-bind (cond-node rest)
          (parse-expression-node tok)
        (setf cond cond-node
              tok rest)))
    (setf tok (skip-to-token ";" tok))
    ;; Entering "for" scope.
    (incf *break-depth*)
    (unless (string= (lex:token-value (lex:token-next tok)) "{")
      (multiple-value-bind (inc-node rest)
          (parse-expression-node (lex:token-next tok))
        (setf inc inc-node
              tok rest)))
    (setf tok (skip-to-token "{" tok))
    (multiple-value-bind (body-node rest)
        (parse-statement-node tok)
      (setf body body-node
            tok rest))
    ;; Left "for" scope.
    (decf *break-depth*)
    (values (make-ast-node-for :init init
                               :cond cond
                               :inc inc
                               :body body)
            tok)))

(defun parse-loop-statement-node (tok)
  ;; TOK passed in should be the opening brace of the block after the "loop"
  ;; keyword.
  ;; TODO(topi): Add proper error handling.
  (assert (string= (lex:token-value tok) "{"))
  ;; Entering "loop" scope.
  (incf *break-depth*)
  (multiple-value-bind (body rest)
      (parse-statement-node tok)
    ;; Left "loop" scope.
    (decf *break-depth*)
    (values (make-ast-node-loop :body body)
            rest)))

(defun parse-compound-statement-node (tok)
  (let* ((head (make-ast-node))
         (cur head))
    (loop :until (string= (lex:token-value tok) "}")
          :do (multiple-value-bind (node rest)
                  (parse-statement-node tok)
                (setf (ast-node-next cur) node)
                (setf cur (ast-node-next cur))
                (setf tok rest)))
    (values (make-ast-node-block :body (ast-node-next head))
            (lex:token-next tok))))

(defun parse-expression-statement-node (tok)
  ;; Empty statement, e.g. ';;; return 3;'
  (when (string= (lex:token-value tok) ";")
    (return-from parse-expression-statement-node
      (values (make-ast-node-block :body nil)
              (lex:token-next tok))))
  (multiple-value-bind (node rest)
      (parse-expression-node tok)
    (values (make-ast-node-expression :expr node)
            (lex:token-next (skip-to-token ";" rest)))))

(defun parse-expression-node (tok)
  (parse-assign-node tok))

(defun parse-assign-node (tok)
  (let (node)
    (multiple-value-bind (eql-node rest)
        (parse-equality-node tok)
      (setf node eql-node
            tok rest))
    (when (string= (lex:token-value tok) ":=")
      (multiple-value-bind (expr-node rest)
          (parse-assign-node (lex:token-next tok))
        (setf node (make-ast-node-assign :var node
                                         :expr expr-node)
              tok rest)))
    (values node tok)))

(defmacro define-binop-parser (name &key descent-parser comparison-symbols)
  "Macro for generating new binary operator parser rules."
  (let ((parser-name (intern (format nil "PARSE-~a-NODE" name)))
        (descent-parser-name (intern (format nil "PARSE-~a-NODE" descent-parser))))
    `(defun ,parser-name (tok)
       (let (node)
         (multiple-value-bind (lhs rest)
             (,descent-parser-name tok)
           (setf node lhs)
           (loop
            (cond
              ,@(loop :for symbol :in comparison-symbols
                      :collect `((string= (lex:token-value rest) ,(car symbol))
                                 (multiple-value-bind (rhs rest2)
                                     (,descent-parser-name (lex:token-next rest))
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

(defun parse-unary-node (tok)
  (alexandria:switch ((lex:token-value tok) :test #'string=)
    ("+"
     (parse-unary-node (lex:token-next tok)))

    ("-"
     (multiple-value-bind (node rest)
         (parse-unary-node (lex:token-next tok))
       (values (make-ast-node-negate :value node)
               rest)))

    (t
     (parse-primary-node tok))))

(defun parse-primary-node (tok)
  (flet ((find-local-var (name)
           (loop :for obj := *local-variables*
                   :then (setf obj (object-next obj))
                 :until (null obj)
                 :do (when (string= name (object-name obj))
                       (return-from find-local-var obj)))))
    (cond
      ((eq (lex:token-kind tok) :num)
       (values (make-ast-node-integer-literal
                :value (parse-integer (lex:token-value tok)))
               (lex:token-next tok)))
      ((eq (lex:token-kind tok) :ident)
       (let* ((name (lex:token-value tok))
              (var (find-local-var name)))
         (when (null var)
           (setf var (make-object :name name :next *local-variables*))
           ;; New object should be in front of the list.
           (setf *local-variables* var))
         (values (make-ast-node-variable :object var)
                 (lex:token-next tok))))
      ((string= (lex:token-value tok) "(")
       (multiple-value-bind (node rest)
           (parse-expression-node (lex:token-next tok))
         (values node (lex:token-next (skip-to-token ")" rest)))))
      (t (error "Unexpected token value: ~a" tok)))))

(defun parse-program (tok)
  (labels ((align-to (n align)
             "Round N to the nearest multiple of ALIGN."
             (* (ceiling n align) align))
           (set-lvar-offsets (program)
             (let ((offset 0))
               (loop :for obj := (func-locals program)
                       :then (setf obj (object-next obj))
                     :until (null obj)
                     :do (progn
                           (incf offset 8)
                           (setf (object-offset obj) (- offset))))
               (setf (func-stack-size program) (align-to offset 16)))
             (values)))
    (let* ((head (make-ast-node))
           (cur head))
      (loop :until (eq (lex:token-kind tok) :eof)
            :do (multiple-value-bind (node rest)
                    (parse-statement-node tok)
                  (setf (ast-node-next cur) node)
                  (setf cur (ast-node-next cur))
                  (setf tok rest)))
      (let ((program (make-func :body (ast-node-next head)
                                :locals *local-variables*)))
        (set-lvar-offsets program)
        program))))

;;;
;;; Parser Utilities
;;;

(defun skip-to-token (val tok)
  "Search for token which is VAL. Returns NIL when not found."
  (loop :for cur := tok
          :then (setf cur (lex:token-next cur))
        :until (eq (lex:token-kind cur) :eof)
        :do (when (string= (lex:token-value cur) val)
              (return-from skip-to-token cur))))
