(defpackage #:sila/parser
  (:use #:cl)
  (:import-from #:sila/conditions
                #:parser-error)
  (:import-from #:sila/lexer
                #:tokenize
                #:token-kind
                #:token-val
                #:token-next)
  (:export #:expression-node))
(in-package #:sila/parser)

(deftype ast-node-kind ()
  "Sila AST node kind."
  '(member
    :add     ; +
    :sub     ; -
    :mul     ; *
    :div     ; /
    :neg     ; unary -
    :number))

(defstruct ast-node
  "Structure for Sila AST nodes."
  kind
  val
  lhs
  rhs)

(defun skip-to-token (val tok)
  "Search for token which is `val'. Returns `nil' when not found.

TODO(topi): Probably should do some proper error handling if `val' isn't
found."
  (cond ((eq (token-kind tok) :eof) nil)
        ((equal (token-val tok) val) tok)
        (t (skip-to-token val (token-next tok)))))

(defun expression-node (tok)
  "expression-node ::== multiplicative-node ( '+' multiplicative-node
                                            | '-' multiplicative-node ) *"
  (multiple-value-bind (node rest)
      (multiplicative-node tok)
    (loop
      (cond ((eq (token-val rest) #\+)
             (multiple-value-bind (node2 rest2)
                 (multiplicative-node (token-next rest))
               (setf node (make-ast-node :kind :add :lhs node :rhs node2))
               (setf rest rest2)))
            ((eq (token-val rest) #\-)
             (multiple-value-bind (node2 rest2)
                 (multiplicative-node (token-next rest))
               (setf node (make-ast-node :kind :sub :lhs node :rhs node2))
               (setf rest rest2)))
            (t
             (return-from expression-node
               (values node rest)))))))

(defun multiplicative-node (tok)
  "multiplicative-node ::== unary-node ( '*' unary-node | '/' unary-node ) *"
  (multiple-value-bind (node rest)
      (unary-node tok)
    (loop
      (cond ((eq (token-val rest) #\*)
             (multiple-value-bind (node2 rest2)
                 (unary-node (token-next rest))
               (setf node (make-ast-node :kind :mul :lhs node :rhs node2))
               (setf rest rest2)))
            ((eq (token-val rest) #\/)
             (multiple-value-bind (node2 rest2)
                 (unary-node (token-next rest))
               (setf node (make-ast-node :kind :div :lhs node :rhs node2))
               (setf rest rest2)))
            (t
             (return-from multiplicative-node
               (values node rest)))))))

(defun unary-node (tok)
  "unary-node ::== ( '+' | '-' ) unary | primary-node"
  (cond ((eq (token-val tok) #\+)
         (unary-node (token-next tok)))
        ((eq (token-val tok) #\-)
         (multiple-value-bind (node rest)
             (unary-node (token-next tok))
           ;; In case something like '--10' is encountered.
           (when (eq (token-val rest) #\-)
             (unary-node (token-next rest)))
           (values (make-ast-node :kind :neg :lhs node)
                   rest)))
        (t
         (primary-node tok))))

(defun primary-node (tok)
  "primary-node ::== '(' expression-node ')' | number"
  (cond ((eq (token-kind tok) :num)
         (values (make-ast-node :kind :number :val (token-val tok))
                 (token-next tok)))
        ((eq (token-val tok) #\()
         (multiple-value-bind (node rest)
             (expression-node (token-next tok))
           (values node (token-next (skip-to-token #\) rest)))))
        (t (error 'parser-error))))
