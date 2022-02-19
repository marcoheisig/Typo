(in-package #:typo.common-lisp)

(define-fndb-record exp (x)
  (:pure t)
  (:differentiator _ (wrap (exp x)))
  (:specializer
   (ntype-subtypecase (wrapper-ntype x)
     ((not number)
      (abort-specialization))
     (short-float
      (wrap (short-float-exp x)))
     (single-float
      (wrap (single-float-exp x)))
     (double-float
      (wrap (double-float-exp x)))
     (long-float
      (wrap (long-float-exp x)))
     (complex
      (wrap
       (* (exp (realpart x))
          (cis (imagpart x)))))
     (t
      (wrap-default (type-specifier-ntype 'number))))))

(define-simple-instruction (exp short-float-exp) ((short-float 0S0 *)) (short-float))
(define-simple-instruction (exp single-float-exp) ((single-float 0F0 *)) (single-float))
(define-simple-instruction (exp double-float-exp) ((double-float 0D0 *)) (double-float))
(define-simple-instruction (exp long-float-exp) ((long-float 0L0 *)) (long-float))

(define-fndb-record expt (base power)
  (:pure t)
  (:differentiator
   index
   (ecase index
     (0 (wrap (* (expt base (1- power)) power)))
     (1 (wrap (* (expt base power) (log base))))))
  (:specializer
   (let ((base-ntype (wrapper-ntype base))
         (power-ntype (wrapper-ntype power)))
     (ntype-subtypecase power-ntype
       ((eql 0)
        (ntype-subtypecase base-ntype
          ((not number) (abort-specialization))
          (integer (wrap 1))
          (float (wrap (float base)))
          ((complex float) (wrap (complex (float 1 base) 0)))
          (t (wrap-default (type-specifier-ntype 'number)))))
       ((eql 1) (wrap (the-number base)))
       ((eql 2) (wrap (* base base)))
       ((eql -2) (wrap (/ (* base base))))
       ((eql 3) (wrap (* base base base)))
       ((eql -3) (wrap (/ (* base base base))))
       ((eql 1/2) (wrap (sqrt base)))
       ((eql -1/2) (wrap (/ (sqrt base))))
       ((eql 4)
        (let ((tmp (wrap (* base base))))
          (wrap (* tmp tmp))))
       ((eql 5)
        (let ((tmp (wrap (* base base))))
          (wrap (* tmp (* tmp base)))))
       ((eql 6)
        (let ((tmp (wrap (* base base base))))
          (wrap (* tmp tmp))))
       (integer
        (ntype-subtypecase base-ntype
          ((not number) (abort-specialization))
          (rational (wrap-default (type-specifier-ntype 'rational)))
          (short-float (wrap-default (type-specifier-ntype 'short-float)))
          (single-float (wrap-default (type-specifier-ntype 'single-float)))
          (double-float (wrap-default (type-specifier-ntype 'double-float)))
          (long-float (wrap-default (type-specifier-ntype 'long-float)))
          (t (wrap-default (type-specifier-ntype 'number)))))
       (t
        (ntype-subtypecase base-ntype
          ((not number) (abort-specialization))
          (t (if (eql-ntype-p base-ntype)
                 (wrap (exp (* power (ln base))))
                 (wrap-default (type-specifier-ntype 'number))))))))))
