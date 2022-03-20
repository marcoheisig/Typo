(in-package #:typo.common-lisp)

(define-fnrecord * (&rest numbers)
  (:pure t)
  (:differentiator
   index
   (apply (function-specializer '*)
          (loop for number in numbers
                for position from 0
                unless (= position index)
                  collect number)))
  (:specializer
   (trivia:match numbers
     ((list) (wrap 1))
     ((list number) (wrap (the-number number)))
     ((list a b)
      (wrap (two-arg-* a b)))
     (numbers
      (let* ((mid (floor (length numbers) 2))
             (lo (subseq numbers 0 mid))
             (hi (subseq numbers mid))
             (specializer (function-specializer '*)))
        (funcall specializer (apply specializer lo) (apply specializer hi)))))))

(define-fnrecord two-arg-* (a b)
  (:pure t)
  (:parent *)
  (:specializer
   (let* ((ntype-of-a (wrapper-ntype a))
          (ntype-of-b (wrapper-ntype b))
          (result-ntype (ntype-contagion ntype-of-a ntype-of-b)))
     (cond
       ((or (and (eql-ntype-p ntype-of-a)
                 (eql 0 (eql-ntype-object ntype-of-a)))
            (and (eql-ntype-p ntype-of-b)
                 (eql 0 (eql-ntype-object ntype-of-b))))
        (wrap-constant (coerce 0 (ntype-type-specifier result-ntype))))
       ((and (eql-ntype-p ntype-of-a)
             (eql 1 (eql-ntype-object ntype-of-a)))
        (funcall (function-specializer 'coerce)
                 b
                 (wrap-constant (ntype-type-specifier result-ntype))))
       ((and (eql-ntype-p ntype-of-b)
             (eql 1 (eql-ntype-object ntype-of-b)))
        (funcall (function-specializer 'coerce)
                 a
                 (wrap-constant (ntype-type-specifier result-ntype))))
       (t
        (ntype-subtypecase
            (ntype-contagion
             (wrapper-ntype a)
             (wrapper-ntype b))
          ((not number) (abort-specialization))
          (integer
           (wrap
            (integer*
             (the-integer a)
             (the-integer b))))
          (short-float
           (wrap
            (short-float*
             (coerce-to-short-float a)
             (coerce-to-short-float b))))
          (single-float
           (wrap
            (single-float*
             (coerce-to-single-float a)
             (coerce-to-single-float b))))
          (double-float
           (wrap
            (double-float*
             (coerce-to-double-float a)
             (coerce-to-double-float b))))
          (long-float
           (wrap
            (long-float*
             (coerce-to-long-float a)
             (coerce-to-long-float b))))
          ((complex short-float)
           (wrap
            (complex-short-float*
             (coerce-to-complex-short-float a)
             (coerce-to-complex-short-float b))))
          ((complex single-float)
           (wrap
            (complex-single-float*
             (coerce-to-complex-single-float a)
             (coerce-to-complex-single-float b))))
          ((complex double-float)
           (wrap
            (complex-double-float*
             (coerce-to-complex-double-float a)
             (coerce-to-complex-double-float b))))
          ((complex long-float)
           (wrap
            (complex-long-float*
             (coerce-to-complex-long-float a)
             (coerce-to-complex-long-float b))))
          (t
           (wrap-default (type-specifier-ntype 'number)))))))))

(define-simple-instruction (* integer*) (integer) (integer integer))
(define-simple-instruction (* short-float*) (short-float) (short-float short-float))
(define-simple-instruction (* single-float*) (single-float) (single-float single-float))
(define-simple-instruction (* double-float*) (double-float) (double-float double-float))
(define-simple-instruction (* long-float*) (long-float) (long-float long-float))
(define-simple-instruction (* complex-short-float*) (complex-short-float) (complex-short-float complex-short-float))
(define-simple-instruction (* complex-single-float*) (complex-single-float) (complex-single-float complex-single-float))
(define-simple-instruction (* complex-double-float*) (complex-double-float) (complex-double-float complex-double-float))
(define-simple-instruction (* complex-long-float*) (complex-long-float) (complex-long-float complex-long-float))
