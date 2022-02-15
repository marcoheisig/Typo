(in-package #:typo.fndb)

(define-differentiator exp (x) _
  (wrap (exp x)))

(define-specializer exp (x)
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
     (wrap-default (type-specifier-ntype 'number)))))

(define-simple-instruction (exp short-float-exp) ((short-float 0S0 *)) (short-float))
(define-simple-instruction (exp single-float-exp) ((single-float 0F0 *)) (single-float))
(define-simple-instruction (exp double-float-exp) ((double-float 0D0 *)) (double-float))
(define-simple-instruction (exp long-float-exp) ((long-float 0L0 *)) (long-float))

(define-differentiator expt (base power) index
  (ecase index
    (0 (wrap (* (expt base (1- power)) power)))
    (1 (wrap (* (expt base power) (log base))))))

(define-specializer expt (base power)
  (let ((base-ntype (wrapper-ntype base))
        (power-ntype (wrapper-ntype power)))
    (case power-ntype
      (0
       (ntype-subtypecase base-ntype
         ((not number)
          (abort-specialization))
         (integer
          (wrap 1))
         (float
          (wrap (float base)))
         ((complex float)
          (wrap (complex (float 1 base) 0)))
         (t
          (wrap-default (type-specifier-ntype 'number)))))
      (2 (wrap (* base base)))
      (3 (wrap (* base base base)))
      (4 (let ((tmp (wrap (* base base))))
           (wrap (* tmp tmp))))
      (5 (let ((tmp (wrap (* base base))))
           (wrap (* tmp (* tmp base)))))
      (6 (let ((tmp (wrap (* base base base))))
           (wrap (* tmp tmp))))
      (-2 (wrap (/ (* base base))))
      (-3 (wrap (/ (* base base base))))
      (1/2 (wrap (sqrt base)))
      (-1/2 (wrap (/ (sqrt base))))
      (otherwise
       (if (eql-ntype-p base-ntype)
           (wrap (exp (* power (ln base))))
           (ntype-subtypecase power-ntype
             ((not number) (abort-specialization))
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
              (wrap-default (type-specifier-ntype 'number)))))))))