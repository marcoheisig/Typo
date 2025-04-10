(defsystem "typo.test-suite"
  :description "A comprehensive test suite for Typo."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "MIT"

  :depends-on
  ("closer-mop"
   "typo")

  :perform
  (test-op (o c) (symbol-call '#:typo.test-suite '#:run-typo-test-suite))

  :serial t
  :components
  ((:file "packages")
   (:file "test-suite")
   (:file "test-objects")
   (:file "ntype-tests")
   (:file "fndb-tests")
   (:file "lambda-tests")))
