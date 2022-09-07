(in-package #:typo.vm)

(define-fnrecord + (&rest numbers)
  (:pure t)
  (:differentiator index (declare (ignore numbers index)) (wrap 1))
  (:specializer
   (trivia:match numbers
     ((list)
      (wrap 0))
     ((list number)
      (wrap (the-number number)))
     ((list a b)
      (wrap (two-arg+ a b)))
     (numbers
      (let* ((mid (floor (length numbers) 2))
             (lo (subseq numbers 0 mid))
             (hi (subseq numbers mid))
             (specializer (function-specializer '+)))
        (funcall specializer (apply specializer lo) (apply specializer hi)))))))

(define-fnrecord two-arg+ (a b)
  (:pure t)
  (:differentiator index (declare (ignore a b index)) (wrap 1))
  (:specializer
   (let* ((ntype-of-a (wrapper-ntype a))
          (ntype-of-b (wrapper-ntype b))
          (result-ntype (ntype-contagion ntype-of-a ntype-of-b)))
     (cond
       ((and (eql-ntype-p ntype-of-a)
             (eql 0 (eql-ntype-object ntype-of-a)))
        (funcall (function-specializer 'coerce)
                 b
                 (wrap-constant (ntype-type-specifier result-ntype))))
       ((and (eql-ntype-p ntype-of-b)
             (eql 0 (eql-ntype-object ntype-of-b)))
        (funcall (function-specializer 'coerce)
                 a
                 (wrap-constant (ntype-type-specifier result-ntype))))
       (t
        (ntype-subtypecase result-ntype
          (nil (abort-specialization))
          ((not number) (abort-specialization))
          (integer
           (wrap
            (two-arg-integer+
             (the-integer a)
             (the-integer b))))
          (short-float
           (wrap
            (two-arg-short-float+
             (coerce-to-short-float a)
             (coerce-to-short-float b))))
          (single-float
           (wrap
            (two-arg-single-float+
             (coerce-to-single-float a)
             (coerce-to-single-float b))))
          (double-float
           (wrap
            (two-arg-double-float+
             (coerce-to-double-float a)
             (coerce-to-double-float b))))
          (long-float
           (wrap
            (two-arg-long-float+
             (coerce-to-long-float a)
             (coerce-to-long-float b))))
          ((complex short-float)
           (wrap
            (two-arg-complex-short-float+
             (coerce-to-complex-short-float a)
             (coerce-to-complex-short-float b))))
          ((complex single-float)
           (wrap
            (two-arg-complex-single-float+
             (coerce-to-complex-single-float a)
             (coerce-to-complex-single-float b))))
          ((complex double-float)
           (wrap
            (two-arg-complex-double-float+
             (coerce-to-complex-double-float a)
             (coerce-to-complex-double-float b))))
          ((complex long-float)
           (wrap
            (two-arg-complex-long-float+
             (coerce-to-complex-long-float a)
             (coerce-to-complex-long-float b))))
          (t (wrap-default (type-specifier-ntype 'number)))))))))

(define-simple-instruction (+ two-arg-integer+) (integer) (integer integer))
(define-simple-instruction (+ two-arg-short-float+) (short-float) (short-float short-float))
(define-simple-instruction (+ two-arg-single-float+) (single-float) (single-float single-float))
(define-simple-instruction (+ two-arg-double-float+) (double-float) (double-float double-float))
(define-simple-instruction (+ two-arg-long-float+) (long-float) (long-float long-float))
(define-simple-instruction (+ two-arg-complex-short-float+) (complex-short-float) (complex-short-float complex-short-float))
(define-simple-instruction (+ two-arg-complex-single-float+) (complex-single-float) (complex-single-float complex-single-float))
(define-simple-instruction (+ two-arg-complex-double-float+) (complex-double-float) (complex-double-float complex-double-float))
(define-simple-instruction (+ two-arg-complex-long-float+) (complex-long-float) (complex-long-float complex-long-float))

(define-fnrecord 1+ (number)
  (:pure t)
  (:differentiator index (declare (ignore index number)) (wrap 1))
  (:specializer (wrap (+ number 1))))
