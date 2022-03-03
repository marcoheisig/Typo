(in-package #:typo.common-lisp)

(define-fnrecord sqrt (x)
  (:pure t)
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

(define-simple-instruction (sqrt short-float-sqrt) (short-float) ((or short-float (complex short-float))))
(define-simple-instruction (sqrt single-float-sqrt) (single-float) ((or single-float (complex single-float))))
(define-simple-instruction (sqrt double-float-sqrt) (double-float) ((or double-float (complex double-float))))
(define-simple-instruction (sqrt long-float-sqrt) (long-float) ((or long-float (complex long-float))))

