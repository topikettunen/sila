(defpackage #:sila/parser
  (:use #:cl)
  (:import-from #:sila/conditions
                #:parser-error)
  (:import-from #:sila/lexer
                #:tokenize
                #:token-kind
                #:token-val
                #:token-next)
  (:export #:expr))
(in-package #:sila/parser)

(deftype ast-node-kind ()
  "Sila AST node kind."
  '(member
    :add
    :sub
    :mul
    :div
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
        (t (search-token val (token-next tok)))))

(defun expr (tok)
  "expr ::== mul ( '+' mul | '-' mul ) *"
  (multiple-value-bind (node rest)
      (mul tok)
    (loop
      (cond ((eq (token-val rest) #\+)
             (multiple-value-bind (node2 rest2)
                 (mul (token-next rest))
               (setf node (make-ast-node :kind :add :lhs node :rhs node2))
               (setf rest rest2)))
            ((eq (token-val rest) #\-)
             (multiple-value-bind (node2 rest2)
                 (mul (token-next rest))
               (setf node (make-ast-node :kind :sub :lhs node :rhs node2))
               (setf rest rest2)))
            (t
             (return-from expr
               (values node rest)))))))

(defun mul (tok)
  "mul ::== primary ( '*' primary | '/' primary ) *"
  (multiple-value-bind (node rest)
      (primary tok)
    (loop
      (cond ((eq (token-val rest) #\*)
             (multiple-value-bind (node2 rest2)
                 (primary (token-next rest))
               (setf node (make-ast-node :kind :mul :lhs node :rhs node2))
               (setf rest rest2)))
            ((eq (token-val rest) #\/)
             (multiple-value-bind (node2 rest2)
                 (primary (token-next rest))
               (setf node (make-ast-node :kind :div :lhs node :rhs node2))
               (setf rest rest2)))
            (t
             (return-from mul
               (values node rest)))))))

(defun primary (tok)
  "primary ::== '(' expr ')' | number"
  (cond ((eq (token-kind tok) :num)
         (values (make-ast-node :kind :number :val (token-val tok))
                 (token-next tok)))
        ((eq (token-val tok) #\()
         (multiple-value-bind (node rest)
             (expr (token-next tok))
           (values node (token-next (skip-to-token #\) rest)))))
        (t (error 'parser-error))))
