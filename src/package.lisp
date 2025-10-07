(in-package #:cl-user)

(defpackage #:sila
  (:use #:cl)
  (:local-nicknames
   (#:util #:serapeum/bundle))
  (:export #:sila-run
           #:sila-compile))
