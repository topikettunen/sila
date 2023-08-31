(defpackage #:sila/lexer
  (:use #:cl)
  (:import-from #:sila/conditions
                #:lexer-error)
  (:export #:tokenize
           #:token-kind
           #:token-val
           #:token-next))
(in-package #:sila/lexer)

(deftype kind ()
  "Sila token kind"
  '(member
    :punct
    :num
    :eof))

(defstruct token
  "Structure for Sila tokens."
  kind
  pos
  len
  val
  next)

(defun whitespacep (c)
  "Predicate for whitespace."
  (member c '(#\Space #\Tab #\Return #\Newline)))

(defun trim-whitespace (str)
  (string-trim '(#\Space #\Tab #\Return #\Newline) str))

(defun punctuatorp (c)
  "Predicate for punctuators. All the punctuators are:

! \" # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~"
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
               (string= ">=" punct)) 2)
          ((punctuatorp (char input pos)) 1)
          (t 0))))

(defun tokenize (src)
  "Generate tokens from the given source code."
  (let* ((head (make-token))
         (cur head)
         (src-pos 0))
    (loop :while (< src-pos (length src))
          :do (let ((punct-pos (skip-to-punctuator src src-pos)))
                (cond (;; Whitespace
                       (whitespacep (char src src-pos))
                       (incf src-pos))
                      (;; Numeric literal
                       (digit-char-p (char src src-pos))
                       (let* ((token-len (if punct-pos
                                             (- punct-pos src-pos)
                                             ;; No more punctuators.
                                             (- (length src) src-pos)))
                              (token-val (trim-whitespace (subseq src src-pos (+ src-pos token-len)))))
                         (setf (token-next cur)
                               (make-token :kind :num
                                           :val token-val
                                           :len (length token-val)
                                           :pos src-pos))
                         (setf cur (token-next cur))
                         (if punct-pos
                             (setf src-pos punct-pos)
                             (setf src-pos (length src)))))
                      (;; Punctuator
                       (punctuatorp (char src src-pos))
                       (let ((punct-len (punct-length src src-pos)))
                         (setf (token-next cur)
                               (make-token :kind :punct
                                           :val (subseq src src-pos (+ src-pos punct-len))
                                           :pos src-pos
                                           :len punct-len))
                         (setf cur (token-next cur))
                         (setf src-pos (+ src-pos punct-len))))
                      (t
                       (error 'lexer-error
                              :lexer-input src
                              :error-msg "Invalid token."
                              :token-position src-pos)))))
    ;; No more tokens.
    (setf (token-next cur) (make-token :kind :eof
                                       :pos src-pos))
    (setf cur (token-next cur))
    (token-next head)))
