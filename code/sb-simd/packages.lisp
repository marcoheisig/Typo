(cl:in-package #:common-lisp-user)

(defpackage #:typo.sb-simd
  (:use #:closer-common-lisp
        #:typo.common-lisp
        #:typo.fndb
        #:sb-simd-internals))
