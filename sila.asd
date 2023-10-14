(defsystem "sila"
  :version "0.0.1"
  :author "Topi Kettunen <topi@topikettunen.com>"
  :license "MIT"
  :depends-on ("alexandria")
  :components ((:module "src"
                :components
                ((:file "lexer")
                 (:file "parser")
                 (:file "codegen")
                 (:file "sila"))))
  :description "Sila programming language"
  :in-order-to ((test-op (test-op "sila/tests"))))

(defsystem "sila/tests"
  :author "Topi Kettunen <topi@topikettunen.com>"
  :license "MIT"
  :depends-on ("sila"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "package")
                 (:file "compiler-tests")
                 (:file "codegen-x86-64-tests"))))
  :description "Test system for sila"
  :perform (test-op (op c) (symbol-call :rove :run c)))
