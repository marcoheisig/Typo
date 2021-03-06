(cl:in-package #:common-lisp-user)

(progn
  (defpackage #:typo
    (:use #:closer-common-lisp)
    (:import-from #:trivia #:place #:<> #:access)
    ;; Handling of Types
    #1=
    (:export
     #:non-nil-symbol
     #:function-name
     #:variable-name
     #:function-designator
     #:extended-function-designator
     #:character-designator
     #:string-designator
     #:package-designator
     #:radix
     #:character-code
     #:arity
     #:argument-index
     #:byte-specifier
     #:complex-short-float
     #:complex-single-float
     #:complex-double-float
     #:complex-long-float
     #:generalized-boolean
     #:type-specifier
     #:zero
     #:+word-bits+
     #:+short-float-bits+
     #:+single-float-bits+
     #:+double-float-bits+
     #:+long-float-bits+
     #:ntype
     #:ntype-type-specifier
     #:ntype-bits
     #:ntype-subtypep
     #:ntype-subtypepc2
     #:ntype-union
     #:ntype-intersection
     #:ntype-contagion
     #:ntype=
     #:ntype-of
     #:eql-ntype-p
     #:eql-ntype-object
     #:type-specifier-ntype
     #:array-element-ntype
     #:complex-part-ntype
     #:upgraded-array-element-ntype
     #:upgraded-complex-part-ntype
     #:ntype-subtypecase
     #:universal-ntype
     #:empty-ntype)
    ;; The Function Database
    #2=
    (:export
     #:function-arity
     #:lambda-list-arity
     #:abort-specialization
     #:define-fnrecord
     #:fnrecord
     #:fnrecordp
     #:fnrecord-name
     #:fnrecord-function
     #:fnrecord-min-arguments
     #:fnrecord-max-arguments
     #:fnrecord-purep
     #:fnrecord-specializer
     #:fnrecord-differentiator
     #:find-fnrecord
     #:ensure-fnrecord
     #:wrap
     #:unwrap
     #:wrap-default
     #:wrap-default*
     #:wrapper-ntype
     #:wrap-constant
     #:wrap-function
     #:assert-wrapper-type
     #:define-instruction
     #:define-simple-instruction
     #:function-specializer
     #:function-differentiator
     #:specialize
     #:differentiate
     #:infer-ntypes)
    ;; Common Lisp Functions
    #3=
    (:export
     #:the-number
     #:the-real
     #:the-rational
     #:the-integer
     #:the-float
     #:the-short-float
     #:the-single-float
     #:the-double-float
     #:the-long-float
     #:the-complex
     #:the-complex-short-float
     #:the-complex-single-float
     #:the-complex-double-float
     #:the-complex-long-float
     #:the-function
     #:the-character
     #:the-symbol
     ;; abs.lisp
     #:short-float-abs
     #:single-float-abs
     #:double-float-abs
     #:long-float-abs
     #:complex-short-float-abs
     #:complex-single-float-abs
     #:complex-double-float-abs
     #:complex-long-float-abs
     ;; add.lisp
     #:integer+
     #:short-float+
     #:single-float+
     #:double-float+
     #:long-float+
     #:complex-short-float+
     #:complex-single-float+
     #:complex-double-float+
     #:complex-long-float+
     ;; array.lisp
     #:short-float-row-major-aref
     #:single-float-row-major-aref
     #:double-float-row-major-aref
     #:long-float-row-major-aref
     ;; auxiliary.lisp
     #:and-fn
     #:or-fn
     #:prog2-fn
     ;; casts.lisp
     #:short-float-from-single-float
     #:short-float-from-double-float
     #:short-float-from-long-float
     #:single-float-from-short-float
     #:single-float-from-double-float
     #:single-float-from-long-float
     #:double-float-from-short-float
     #:double-float-from-single-float
     #:double-float-from-long-float
     #:long-float-from-short-float
     #:long-float-from-single-float
     #:long-float-from-double-float
     #:complex-short-float-from-short-float
     #:complex-short-float-from-complex-single-float
     #:complex-short-float-from-complex-double-float
     #:complex-short-float-from-complex-long-float
     #:complex-single-float-from-single-float
     #:complex-single-float-from-complex-short-float
     #:complex-single-float-from-complex-double-float
     #:complex-single-float-from-complex-long-float
     #:complex-double-float-from-double-float
     #:complex-double-float-from-complex-short-float
     #:complex-double-float-from-complex-single-float
     #:complex-double-float-from-complex-long-float
     #:complex-long-float-from-long-float
     #:complex-long-float-from-complex-short-float
     #:complex-long-float-from-complex-single-float
     #:complex-long-float-from-complex-double-float
     ;; cmpeq.lisp
     #:short-float=
     #:single-float=
     #:double-float=
     #:long-float=
     #:complex-short-float=
     #:complex-single-float=
     #:complex-double-float=
     #:complex-long-float=
     ;; cmpneq.lisp
     #:short-float/=
     #:single-float/=
     #:double-float/=
     #:long-float/=
     #:complex-short-float/=
     #:complex-single-float/=
     #:complex-double-float/=
     #:complex-long-float/=
     ;; cmpx.lisp
     #:short-float<
     #:single-float<
     #:double-float<
     #:long-float<
     #:short-float>
     #:single-float>
     #:double-float>
     #:long-float>
     #:short-float<=
     #:single-float<=
     #:double-float<=
     #:long-float<=
     #:short-float>=
     #:single-float>=
     #:double-float>=
     #:long-float>=
     ;; complex.lisp
     #:short-float-complex
     #:single-float-complex
     #:double-float-complex
     #:long-float-complex
     #:short-float-cis
     #:single-float-cis
     #:double-float-cis
     #:long-float-cis
     #:complex-short-float-realpart
     #:complex-single-float-realpart
     #:complex-double-float-realpart
     #:complex-long-float-realpart
     #:complex-short-float-imagpart
     #:complex-single-float-imagpart
     #:complex-double-float-imagpart
     #:complex-long-float-imagpart
     ;; cos.lisp
     #:short-float-cos
     #:single-float-cos
     #:double-float-cos
     #:long-float-cos
     #:complex-short-float-cos
     #:complex-single-float-cos
     #:complex-double-float-cos
     #:complex-long-float-cos
     ;; div.lisp
     #:short-float/
     #:single-float/
     #:double-float/
     #:long-float/
     #:complex-short-float/
     #:complex-single-float/
     #:complex-double-float/
     #:complex-long-float/
     ;; max.lisp
     #:short-float-max
     #:single-float-max
     #:double-float-max
     #:long-float-max
     ;; min.lisp
     #:short-float-min
     #:single-float-min
     #:double-float-min
     #:long-float-min
     ;; mul.lisp
     #:integer*
     #:short-float*
     #:single-float*
     #:double-float*
     #:long-float*
     #:complex-short-float*
     #:complex-single-float*
     #:complex-double-float*
     #:complex-long-float*
     ;; sin.lisp
     #:short-float-sin
     #:single-float-sin
     #:double-float-sin
     #:long-float-sin
     #:complex-short-float-sin
     #:complex-single-float-sin
     #:complex-double-float-sin
     #:complex-long-float-sin
     ;; sub.lisp
     #:integer-
     #:short-float-
     #:single-float-
     #:double-float-
     #:long-float-
     #:complex-short-float-
     #:complex-single-float-
     #:complex-double-float-
     #:complex-long-float-
     #:integer-unary-
     #:short-float-unary-
     #:single-float-unary-
     #:double-float-unary-
     #:long-float-unary-
     #:complex-short-float-unary-
     #:complex-single-float-unary-
     #:complex-double-float-unary-
     #:complex-long-float-unary-
     ;; tan.lisp
     #:short-float-tan
     #:single-float-tan
     #:double-float-tan
     #:long-float-tan
     #:complex-short-float-tan
     #:complex-single-float-tan
     #:complex-double-float-tan
     #:complex-long-float-tan))

  (defpackage #:typo.ntype
    (:use #:closer-common-lisp #:typo)
    (:import-from #:trivia #:place #:<> #:access)
    #1#)

  (defpackage #:typo.fndb
    (:use #:closer-common-lisp #:typo)
    (:import-from #:trivia #:place #:<> #:access)
    #2#)

  (defpackage #:typo.common-lisp
    (:use #:closer-common-lisp #:typo)
    (:import-from #:trivia #:place #:<> #:access)
    #3#))
