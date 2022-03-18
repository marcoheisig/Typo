(defsystem "typo.sb-simd"
  :description "sb-simd support for Typo."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "MIT"

  :depends-on
  ("alexandria"
   "closer-mop"
   "sb-simd"
   "trivia"
   "typo")

  :serial t
  :components
  ((:module "sb-simd"
    :components
    ((:file "packages")
     (:file "associatives")
     (:file "casts")
     (:file "comparisons")
     (:file "ifs")
     (:file "instructions")
     (:file "loads")
     (:file "reducers")
     (:file "stores")
     (:file "unequals")
     (:file "cl")))))
