(defsystem "typo"
  :description "A portable type inference library for Common Lisp"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "AGPLv3"

  :depends-on
  ("alexandria"
   "closer-mop"
   "introspect-environment"
   "trivia"
   "trivial-arguments"
   "trivial-garbage")

  :in-order-to ((test-op (test-op "petalisp.test-suite")))

  :serial t
  :components
  ((:file "packages")
   (:module "ntype"
    :components
    ((:file "auxiliary-types")
     (:file "bits")
     (:file "class-subclasses")
     (:file "primitive-ntype-information")
     (:file "ntype")
     (:file "primitive-ntype")
     (:file "upgraded-array-element-ntype")
     (:file "upgraded-complex-part-ntype")
     (:file "make-eql-ntype")
     (:file "make-array-ntype")
     (:file "type-specifier-ntype")
     (:file "ntype-subtypep")
     (:file "ntype-subtypepc2")
     (:file "ntype=")
     (:file "ntype-union")
     (:file "ntype-intersection")
     (:file "ntype-subtypecase")
     (:file "ntype-contagion")
     (:file "array-element-ntype")
     (:file "complex-part-ntype")))
   (:module "fndb"
    :components
    ((:file "function-lambda-lists")
     (:file "conditions")
     (:file "wrap")
     (:file "fndb")
     (:file "define-fndb-record")
     (:file "macros")
     (:file "specialize")
     (:file "differentiate")))
   (:module "primitives"
    :components
    ((:file "default")))
   (:module "common-lisp"
    :components
    ((:file "auxiliary")
     (:file "predicates")
     (:file "type-checks")
     (:file "casts")
     (:file "data-and-control-flow")
     (:file "types-and-classes")
     (:file "abs")
     (:file "add")
     (:file "cmpeq")
     (:file "cmpneq")
     (:file "cmpx")
     (:file "complex")
     (:file "exp")
     (:file "log")
     (:file "cos")
     (:file "div")
     (:file "max")
     (:file "min")
     (:file "mul")
     (:file "signum")
     (:file "sin")
     (:file "sub")
     (:file "sqrt")
     (:file "tan")))))
