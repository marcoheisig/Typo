(in-package #:typo.vm)

(define-fnrecord ln (x)
  (:properties :foldable :movable)
  (:differentiator _ (wrap (/ x)))
  (:specializer
   (ntype-subtypecase (wrapper-ntype x)
     ((not number) (abort-specialization))
     (short-float (wrap (short-float-ln x)))
     (single-float (wrap (single-float-ln x)))
     (double-float (wrap (double-float-ln x)))
     (long-float (wrap (long-float-ln x)))
     (t (wrap-default (type-specifier-ntype 'number))))))

(define-simple-instruction (ln short-float-ln) ((or short-float (complex short-float))) (short-float))
(define-simple-instruction (ln single-float-ln) ((or single-float (complex single-float))) (single-float))
(define-simple-instruction (ln double-float-ln) ((or double-float (complex double-float))) (double-float))
(define-simple-instruction (ln long-float-ln) ((or long-float (complex long-float))) (long-float))

(define-fnrecord log (number &optional (base nil base-supplied-p))
  (:properties :foldable :movable)
  (:differentiator
   index
   (if (not base-supplied-p)
       (wrap (/ number))
       (ecase index
         (0 (wrap (/ (* (ln base) number))))
         (1 (wrap (- (/ (ln number) (* base (expt (ln base) 2)))))))))
  (:specializer
   (if (not base-supplied-p)
       (wrap (ln number))
       (wrap (/ (ln number) (ln base))))))
