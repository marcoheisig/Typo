(in-package #:typo.common-lisp)

(define-fnrecord = (number &rest more-numbers)
  (:pure t)
  (:specializer
   (if (null more-numbers)
       (wrap
        (prog2-fn
         (the-number number)
         t))
       (apply
        (function-specializer 'and)
        (mapcar
         (lambda (other-number)
           (wrap (cmpeq number other-number)))
         more-numbers)))))

(define-simple-instruction (= short-float=) (generalized-boolean) (short-float short-float))
(define-simple-instruction (= single-float=) (generalized-boolean) (single-float single-float))
(define-simple-instruction (= double-float=) (generalized-boolean) (double-float double-float))
(define-simple-instruction (= long-float=) (generalized-boolean) (long-float long-float))
(define-simple-instruction (= complex-short-float=) (generalized-boolean) (complex-short-float complex-short-float))
(define-simple-instruction (= complex-single-float=) (generalized-boolean) (complex-single-float complex-single-float))
(define-simple-instruction (= complex-double-float=) (generalized-boolean) (complex-double-float complex-double-float))
(define-simple-instruction (= complex-long-float=) (generalized-boolean) (complex-long-float complex-long-float))

(define-instruction (= cmpeq) (generalized-boolean) (a b)
  (ntype-subtypecase
      (ntype-contagion
       (wrapper-ntype a)
       (wrapper-ntype b))
    ((not number) (abort-specialization))
    (short-float
     (wrap
      (short-float=
       (coerce-to-short-float a)
       (coerce-to-short-float b))))
    (single-float
     (wrap
      (single-float=
       (coerce-to-single-float a)
       (coerce-to-single-float b))))
    (double-float
     (wrap
      (double-float=
       (coerce-to-double-float a)
       (coerce-to-double-float b))))
    (long-float
     (wrap
      (long-float=
       (coerce-to-long-float a)
       (coerce-to-long-float b))))
    ((complex short-float)
     (wrap
      (complex-short-float=
       (coerce-to-complex-short-float a)
       (coerce-to-complex-short-float b))))
    ((complex single-float)
     (wrap
      (complex-single-float=
       (coerce-to-complex-single-float a)
       (coerce-to-complex-single-float b))))
    ((complex double-float)
     (wrap
      (complex-double-float=
       (coerce-to-complex-double-float a)
       (coerce-to-complex-double-float b))))
    ((complex long-float)
     (wrap
      (complex-long-float=
       (coerce-to-complex-long-float a)
       (coerce-to-complex-long-float b))))
    (t
     (wrap-default
      (type-specifier-ntype 'generalized-boolean)))))


