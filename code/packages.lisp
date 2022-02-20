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
     #:give-up-specialization
     #:define-fndb-record
     #:wrap
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
    ;; Primitives
    #3=
    (:export
     #:f32
     #:f32-if
     #:f32-and
     #:f32-or
     #:f32-xor
     #:f32-andc1
     #:f32-not
     #:f32-max
     #:f32-min
     #:f32+
     #:f32-
     #:f32*
     #:f32/
     #:f32=
     #:f32/=
     #:f32<
     #:f32<=
     #:f32>
     #:f32>=
     #:f32-incf
     #:f32-decf
     #:f32-aref
     #:f32-row-major-aref
     ;; f64
     #:f64
     #:f64-if
     #:f64-and
     #:f64-or
     #:f64-xor
     #:f64-andc1
     #:f64-not
     #:f64-max
     #:f64-min
     #:f64+
     #:f64-
     #:f64*
     #:f64/
     #:f64=
     #:f64/=
     #:f64<
     #:f64<=
     #:f64>
     #:f64>=
     #:f64-incf
     #:f64-decf
     #:f64-aref
     #:f64-row-major-aref
     ;; u1
     #:u1
     ;; u2
     #:u2
     ;; u4
     #:u4
     ;; u8
     #:u8
     #:u8-if
     #:u8-and
     #:u8-or
     #:u8-xor
     #:u8-andc1
     #:u8-not
     #:u8-max
     #:u8-min
     #:u8+
     #:u8-
     #:u8=
     #:u8/=
     #:u8<
     #:u8<=
     #:u8>
     #:u8>=
     #:u8-incf
     #:u8-decf
     #:u8-aref
     #:u8-row-major-aref
     ;; u16
     #:u16
     #:u16-if
     #:u16-and
     #:u16-or
     #:u16-xor
     #:u16-andc1
     #:u16-not
     #:u16-max
     #:u16-min
     #:u16+
     #:u16-
     #:u16=
     #:u16/=
     #:u16<
     #:u16<=
     #:u16>
     #:u16>=
     #:u16-incf
     #:u16-decf
     #:u16-aref
     #:u16-row-major-aref
     ;; u32
     #:u32
     #:u32-if
     #:u32-and
     #:u32-or
     #:u32-xor
     #:u32-andc1
     #:u32-not
     #:u32-max
     #:u32-min
     #:u32+
     #:u32-
     #:u32=
     #:u32/=
     #:u32<
     #:u32<=
     #:u32>
     #:u32>=
     #:u32-incf
     #:u32-decf
     #:u32-aref
     #:u32-row-major-aref
     ;; u64
     #:u64
     #:u64-if
     #:u64-and
     #:u64-or
     #:u64-xor
     #:u64-andc1
     #:u64-not
     #:u64-max
     #:u64-min
     #:u64+
     #:u64-
     #:u64=
     #:u64/=
     #:u64<
     #:u64<=
     #:u64>
     #:u64>=
     #:u64-incf
     #:u64-decf
     #:u64-aref
     #:u64-row-major-aref
     ;; s8
     #:s8
     #:s8-if
     #:s8-and
     #:s8-or
     #:s8-xor
     #:s8-andc1
     #:s8-not
     #:s8-max
     #:s8-min
     #:s8+
     #:s8-
     #:s8=
     #:s8/=
     #:s8<
     #:s8<=
     #:s8>
     #:s8>=
     #:s8-incf
     #:s8-decf
     #:s8-aref
     #:s8-row-major-aref
     ;; s16
     #:s16
     #:s16-if
     #:s16-and
     #:s16-or
     #:s16-xor
     #:s16-andc1
     #:s16-not
     #:s16-max
     #:s16-min
     #:s16+
     #:s16-
     #:s16=
     #:s16/=
     #:s16<
     #:s16<=
     #:s16>
     #:s16>=
     #:s16-incf
     #:s16-decf
     #:s16-aref
     #:s16-row-major-aref
     ;; s32
     #:s32
     #:s32-if
     #:s32-and
     #:s32-or
     #:s32-xor
     #:s32-andc1
     #:s32-not
     #:s32-max
     #:s32-min
     #:s32+
     #:s32-
     #:s32=
     #:s32/=
     #:s32<
     #:s32<=
     #:s32>
     #:s32>=
     #:s32-incf
     #:s32-decf
     #:s32-aref
     #:s32-row-major-aref
     ;; s64
     #:s64
     #:s64-if
     #:s64-and
     #:s64-or
     #:s64-xor
     #:s64-andc1
     #:s64-not
     #:s64-max
     #:s64-min
     #:s64+
     #:s64-
     #:s64=
     #:s64/=
     #:s64<
     #:s64<=
     #:s64>
     #:s64>=
     #:s64-incf
     #:s64-decf
     #:s64-aref
     #:s64-row-major-aref)
    ;; Common Lisp Functions
    #4=
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
     #:short-float+
     #:single-float+
     #:double-float+
     #:long-float+
     #:complex-short-float+
     #:complex-single-float+
     #:complex-double-float+
     #:complex-long-float+
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
     #:short-float-
     #:single-float-
     #:double-float-
     #:long-float-
     #:complex-short-float-
     #:complex-single-float-
     #:complex-double-float-
     #:complex-long-float-
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

  (defpackage #:typo.primitives
    (:use #:closer-common-lisp #:typo)
    (:import-from #:trivia #:place #:<> #:access)
    #3#)

  (defpackage #:typo.common-lisp
    (:use #:closer-common-lisp #:typo)
    (:import-from #:trivia #:place #:<> #:access)
    #4#))
