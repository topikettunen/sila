(defpackage #:sila/parser
  (:use #:cl)
  (:import-from #:sila/conditions
                #:parser-error)
  (:import-from #:sila/lexer
                #:tokenize
                #:token-kind
                #:token-val
                #:token-next)
  (:export #:parse-expression-node))
(in-package #:sila/parser)

(deftype ast-node-kind ()
  "Sila AST node kind."
  '(member
    :add     ; +
    :sub     ; -
    :mul     ; *
    :div     ; /
    :neg     ; unary -
    :equal
    :not-equal
    :lesser-than
    :lesser-or-equal
    :greater-than
    :greater-or-equal
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

;;; TODO(topi): These parsing functions are quite similar to each other, maybe
;;; these could be wrapped into some macro?

(defun parse-expression-node (tok)
  "expression-node ::== equality"
  (parse-equality-node tok))

(defun parse-equality-node (tok)
  "equality-node ::== relational-node ( '==' relational-node
                                      | '!=' relational-node ) *"
  (multiple-value-bind (node rest)
      (parse-relational-node tok)
    (loop
      (cond ((string= (token-val rest) "==")
             (multiple-value-bind (node2 rest2)
                 (parse-relational-node (token-next rest))
               (setf node (make-ast-node :kind :equal :lhs node :rhs node2))
               (setf rest rest2)))
            ((string= (token-val rest) "!=")
             (multiple-value-bind (node2 rest2)
                 (parse-relational-node (token-next rest))
               (setf node (make-ast-node :kind :not-equal :lhs node :rhs node2))
               (setf rest rest2)))
            (t
             (return-from parse-equality-node
               (values node rest)))))))

(defun parse-relational-node (tok)
  "relational-node ::== add ( '<'  add
                            | '<=' add
                            | '>'  add
                            | '>=' add ) *"
  (multiple-value-bind (node rest)
      (parse-add-node tok)
    (loop
      (cond ((string= (token-val rest) "<")
             (multiple-value-bind (node2 rest2)
                 (parse-add-node (token-next rest))
               (setf node (make-ast-node :kind :lesser-than :lhs node :rhs node2))
               (setf rest rest2)))
            ((string= (token-val rest) "<=")
             (multiple-value-bind (node2 rest2)
                 (parse-add-node (token-next rest))
               (setf node (make-ast-node :kind :lesser-or-equal :lhs node :rhs node2))
               (setf rest rest2)))
            ((string= (token-val rest) ">")
             (multiple-value-bind (node2 rest2)
                 (parse-add-node (token-next rest))
               (setf node (make-ast-node :kind :greater-than :lhs node :rhs node2))
               (setf rest rest2)))
            ((string= (token-val rest) ">=")
             (multiple-value-bind (node2 rest2)
                 (parse-add-node (token-next rest))
               (setf node (make-ast-node :kind :greater-or-equal :lhs node :rhs node2))
               (setf rest rest2)))
            (t
             (return-from parse-relational-node
               (values node rest)))))))

(defun parse-add-node (tok)
  "add-node ::== multiplicative-node ( '+' multiplicative-node
                                     | '-' multiplicative-node ) *"
  (multiple-value-bind (node rest)
      (parse-multiplicative-node tok)
    (loop
      (cond ((string= (token-val rest) "+")
             (multiple-value-bind (node2 rest2)
                 (parse-multiplicative-node (token-next rest))
               (setf node (make-ast-node :kind :add :lhs node :rhs node2))
               (setf rest rest2)))
            ((string= (token-val rest) "-")
             (multiple-value-bind (node2 rest2)
                 (parse-multiplicative-node (token-next rest))
               (setf node (make-ast-node :kind :sub :lhs node :rhs node2))
               (setf rest rest2)))
            (t
             (return-from parse-add-node
               (values node rest)))))))

(defun parse-multiplicative-node (tok)
  "multiplicative-node ::== unary-node ( '*' unary-node | '/' unary-node ) *"
  (multiple-value-bind (node rest)
      (parse-unary-node tok)
    (loop
      (cond ((string= (token-val rest) "*")
             (multiple-value-bind (node2 rest2)
                 (parse-unary-node (token-next rest))
               (setf node (make-ast-node :kind :mul :lhs node :rhs node2))
               (setf rest rest2)))
            ((string= (token-val rest) "/")
             (multiple-value-bind (node2 rest2)
                 (parse-unary-node (token-next rest))
               (setf node (make-ast-node :kind :div :lhs node :rhs node2))
               (setf rest rest2)))
            (t
             (return-from parse-multiplicative-node
               (values node rest)))))))

(defun parse-unary-node (tok)
  "unary-node ::== ( '+' | '-' ) unary | primary-node"
  (cond ((string= (token-val tok) "+")
         (parse-unary-node (token-next tok)))
        ((string= (token-val tok) "-")
         (multiple-value-bind (node rest)
             (parse-unary-node (token-next tok))
           ;; In case something like '--10' is encountered.
           (when (eq (token-val rest) #\-)
             (parse-unary-node (token-next rest)))
           (values (make-ast-node :kind :neg :lhs node)
                   rest)))
        (t
         (parse-primary-node tok))))

(defun parse-primary-node (tok)
  "primary-node ::== '(' expression-node ')' | number"
  (cond ((eq (token-kind tok) :num)
         (values (make-ast-node :kind :number :val (token-val tok))
                 (token-next tok)))
        ((string= (token-val tok) "(")
         (multiple-value-bind (node rest)
             (parse-expression-node (token-next tok))
           (values node (token-next (skip-to-token ")" rest)))))
        (t (error 'parser-error))))
