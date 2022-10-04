(in-package #:typo.vm)

(define-fnrecord sqrt (x)
  (:properties :foldable :movable)
  (:differentiator _ (wrap (/ (* 2 (sqrt x)))))
  (:specializer
   (ntype-subtypecase (wrapper-ntype x)
     ((not number)
      (abort-specialization))
     (short-float
      (wrap (short-float-sqrt x)))
     (single-float
      (wrap (single-float-sqrt x)))
     (double-float
      (wrap (double-float-sqrt x)))
     (long-float
      (wrap (long-float-sqrt x)))
     (complex
      (wrap
       (exp (/ (log x) 2))))
     (t
      (wrap-default (type-specifier-ntype 'number))))))

(define-simple-instruction (sqrt short-float-sqrt) ((or short-float (complex short-float))) (short-float))
(define-simple-instruction (sqrt single-float-sqrt) ((or single-float (complex single-float))) (single-float))
(define-simple-instruction (sqrt double-float-sqrt) ((or double-float (complex double-float))) (double-float))
(define-simple-instruction (sqrt long-float-sqrt) ((or long-float (complex long-float))) (long-float))

