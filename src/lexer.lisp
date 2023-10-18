(defpackage #:sila/lexer
  (:use #:cl)
  (:import-from #:alexandria
                #:appendf))
(in-package #:sila/lexer)

(deftype kind ()
  "Sila token kind"
  '(member
    :ident
    :punct
    :num
    :eof))

;; TODO(topi): Set types for these slots.
(defstruct token
  "Structure for Sila tokens."
  kind
  position
  length
  value
  next)

(defun whitespacep (c)
  "Predicate for whitespace."
  (member c '(#\Space #\Tab #\Return #\Newline)))

(defun trim-whitespace (str)
  (string-trim '(#\Space #\Tab #\Return #\Newline) str))

(defun punctuatorp (c)
  "Predicate for punctuators."
  (member c '(#\! #\" #\# #\$ #\% #\& #\' #\( #\) #\* #\+
              #\, #\- #\. #\/ #\: #\; #\< #\= #\> #\? #\@
              #\[ #\\ #\] #\^ #\_ #\` #\{ #\} #\~)))

(defun skip-to-punctuator (input start)
  "Skip to nearest punctuator."
  (position-if #'punctuatorp input :start start))

(defun punct-length (input pos)
  "Read punctuator and return its length. If token isn't a punctuator, return 0."
  (let* ((end (if (= pos (1- (length input)))
                  (length input)
                  (+ pos 2)))
         (punct (subseq input pos end)))
    (cond ((or (string= "==" punct)
               (string= "!=" punct)
               (string= "<=" punct)
               (string= ">=" punct)
               (string= "<-" punct))
           2)
          ((punctuatorp (char input pos))
           1)
          (t 0))))

(defun tokenize (src)
  "Generate tokens from the given source code."
  (let* ((head (make-token))
         (cur head)
         (src-pos 0))
    (loop :while (< src-pos (length src))
          :do (let ((punct-pos (skip-to-punctuator src src-pos)))
                (cond ((whitespacep (char src src-pos))
                       (incf src-pos))
                      ((digit-char-p (char src src-pos))
                       (let* ((token-len (if punct-pos
                                             (- punct-pos src-pos)
                                             ;; No more punctuators.
                                             (- (length src) src-pos)))
                              (token-val (trim-whitespace
                                          (subseq src src-pos (+ src-pos token-len)))))
                         ;; Idents starting with a letter will be catched with
                         ;; a diffenret conditional so if this is hit, ident
                         ;; starts with a number but contains letters, which
                         ;; isn't acceptable.
                         (unless (every #'digit-char-p token-val)
                           (error 'lexer-error
                                  :lexer-input src
                                  :error-msg "Ident can't start with a number."
                                  :token-pos src-pos))
                         (setf (token-next cur)
                               (make-token :kind :num
                                           :value token-val
                                           :length (length token-val)
                                           :position src-pos))
                         (setf cur (token-next cur))
                         (cond (punct-pos
                                (setf src-pos punct-pos)
                                (when (and (char= (char src src-pos) #\<)
                                           (ignore-errors
                                            (char= (char src (1+ src-pos)) #\-)))
                                  (error 'lexer-error
                                         :lexer-input src
                                         :error-msg "Can't assign to a number."
                                         :token-pos src-pos)))
                               (t
                                (setf src-pos (length src))))))
                      ((alpha-char-p (char src src-pos))
                       (let* ((token-len (if punct-pos
                                             (- punct-pos src-pos)
                                             ;; No more punctuators.
                                             (- (length src) src-pos)))
                              (token-val (trim-whitespace
                                          (subseq src src-pos (+ src-pos token-len)))))
                         (setf (token-next cur)
                               (make-token :kind :ident
                                           :value token-val
                                           :length (length token-val)
                                           :position src-pos))
                         (setf cur (token-next cur))
                         (setf src-pos (if punct-pos punct-pos (length src)))))
                      ((punctuatorp (char src src-pos))
                       (let* ((punct-len (punct-length src src-pos))
                              (val (subseq src src-pos (+ src-pos punct-len))))
                         (setf (token-next cur)
                               (make-token :kind :punct
                                           :value val
                                           :position src-pos
                                           :length punct-len))
                         (setf cur (token-next cur))
                         (incf src-pos punct-len)))
                      (t
                       (error 'lexer-error
                              :lexer-input src
                              :error-msg "Invalid token."
                              :token-pos src-pos)))))
    ;; No more tokens.
    (setf (token-next cur) (make-token :kind :eof :position src-pos))
    (setf cur (token-next cur))
    (token-next head)))

;;; Lexer error handling

(defun format-lexer-error (stream pos input msg)
  "Print lexer error in format like:

Lexer error:

1+a1
  ^ msg
"
  (format stream "Lexer error:~%~%~a~%~{~a~}^ ~a"
          input (make-list pos :initial-element #\Space) msg))

(define-condition lexer-error (error)
  ((token-pos :initarg :token-pos
              :initform nil
              :reader token-pos)
   (error-msg :initarg :error-msg
              :initform nil
              :reader error-msg)
   (lexer-input :initarg :lexer-input
                :initform nil
                :reader lexer-input))
  (:report (lambda (condition stream)
             (format-lexer-error stream
                                 (token-pos condition)
                                 (lexer-input condition)
                                 (error-msg condition))))
  (:documentation "Condition for when we encounter invalid token."))
