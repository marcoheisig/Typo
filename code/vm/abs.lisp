(in-package #:typo.vm)

(define-fnrecord abs (x)
  (:properties :foldable :movable)
  (:differentiator _ (wrap (choose (< 0 x) 1 -1)))
  (:specializer
   (ntype-subtypecase (wrapper-ntype x)
     ((not number)
      (abort-specialization))
     (short-float
      (wrap (short-float-abs x)))
     (single-float
      (wrap (single-float-abs x)))
     (double-float
      (wrap (double-float-abs x)))
     (long-float
      (wrap (long-float-abs x)))
     (complex-short-float
      (wrap (complex-short-float-abs x)))
     (complex-single-float
      (wrap (complex-single-float-abs x)))
     (complex-double-float
      (wrap (complex-double-float-abs x)))
     (complex-long-float
      (wrap (complex-long-float-abs x)))
     (integer
      (wrap (integer-abs x)))
     (real
      (wrap-default (type-specifier-ntype '(real 0 *))))
     (rational
      (wrap-default (type-specifier-ntype '(rational 0 *))))
     (t
      (wrap-default (type-specifier-ntype '(real 0 *)))))))

(define-simple-instruction (abs integer-abs) ((integer 0 *)) (integer))
(define-simple-instruction (abs short-float-abs) ((short-float 0S0 *)) (short-float))
(define-simple-instruction (abs single-float-abs) ((single-float 0F0 *)) (single-float))
(define-simple-instruction (abs double-float-abs) ((double-float 0D0 *)) (double-float))
(define-simple-instruction (abs long-float-abs) ((long-float 0L0 *)) (long-float))
(define-simple-instruction (abs complex-short-float-abs) ((short-float 0S0 *)) (complex-short-float))
(define-simple-instruction (abs complex-single-float-abs) ((single-float 0F0 *)) (complex-single-float))
(define-simple-instruction (abs complex-double-float-abs) ((double-float 0D0 *)) (complex-double-float))
(define-simple-instruction (abs complex-long-float-abs) ((long-float 0L0 *)) (complex-long-float))
