(in-package #:typo.vm)

(define-fnrecord - (number &rest more-numbers)
  (:properties :foldable :movable)
  (:differentiator
   index
   (declare (ignore number))
   (if (and (zerop index)
            (not (null more-numbers)))
       (wrap 1)
       (wrap -1)))
  (:specializer
   (cond ((null more-numbers)
          (let ((x number))
            (ntype-subtypecase (wrapper-ntype x)
              ((not number) (abort-specialization))
              (integer (wrap (one-arg-integer- x)))
              (short-float (wrap (one-arg-short-float- x)))
              (single-float (wrap (one-arg-single-float- x)))
              (double-float (wrap (one-arg-double-float- x)))
              (long-float (wrap (one-arg-long-float- x)))
              (complex-short-float (wrap (one-arg-complex-short-float- x)))
              (complex-single-float (wrap (one-arg-complex-single-float- x)))
              (complex-double-float (wrap (one-arg-complex-double-float- x)))
              (complex-long-float (wrap (one-arg-complex-long-float- x)))
              (rational (wrap-default (type-specifier-ntype 'rational)))
              (real (wrap-default (type-specifier-ntype 'real)))
              (t (wrap-default (type-specifier-ntype 'number))))))
         (t
          (reduce
           (lambda (a b)
             (let* ((ntype-of-a (wrapper-ntype a))
                    (ntype-of-b (wrapper-ntype b))
                    (result-ntype (ntype-contagion ntype-of-a ntype-of-b)))
               (if (and (eql-ntype-p ntype-of-b)
                        (= (eql-ntype-object ntype-of-b) 0))
                   (funcall (function-specializer 'coerce)
                            a
                            (wrap-constant
                             (ntype-type-specifier result-ntype)))
                   (ntype-subtypecase result-ntype
                     ((not number) (abort-specialization))
                     (integer
                      (wrap
                       (two-arg-integer-
                        (the-integer a)
                        (the-integer b))))
                     (short-float
                      (wrap
                       (two-arg-short-float-
                        (coerce-to-short-float a)
                        (coerce-to-short-float b))))
                     (single-float
                      (wrap
                       (two-arg-single-float-
                        (coerce-to-single-float a)
                        (coerce-to-single-float b))))
                     (double-float
                      (wrap
                       (two-arg-double-float-
                        (coerce-to-double-float a)
                        (coerce-to-double-float b))))
                     (long-float
                      (wrap
                       (two-arg-long-float-
                        (coerce-to-long-float a)
                        (coerce-to-long-float b))))
                     ((complex short-float)
                      (wrap
                       (two-arg-complex-short-float-
                        (coerce-to-complex-short-float a)
                        (coerce-to-complex-short-float b))))
                     ((complex single-float)
                      (wrap
                       (two-arg-complex-single-float-
                        (coerce-to-complex-single-float a)
                        (coerce-to-complex-single-float b))))
                     ((complex double-float)
                      (wrap
                       (two-arg-complex-double-float-
                        (coerce-to-complex-double-float a)
                        (coerce-to-complex-double-float b))))
                     ((complex long-float)
                      (wrap
                       (two-arg-complex-long-float-
                        (coerce-to-complex-long-float a)
                        (coerce-to-complex-long-float b))))
                     (integer
                      (wrap-default (type-specifier-ntype 'integer)))
                     (rational
                      (wrap-default (type-specifier-ntype 'rational)))
                     (real
                      (wrap-default (type-specifier-ntype 'real)))
                     (t
                      (wrap-default (type-specifier-ntype 'number)))))))
           more-numbers
           :initial-value number)))))

(define-simple-instruction (- two-arg-integer-) (integer) (integer integer))
(define-simple-instruction (- two-arg-short-float-) (short-float) (short-float short-float))
(define-simple-instruction (- two-arg-single-float-) (single-float) (single-float single-float))
(define-simple-instruction (- two-arg-double-float-) (double-float) (double-float double-float))
(define-simple-instruction (- two-arg-long-float-) (long-float) (long-float long-float))
(define-simple-instruction (- two-arg-complex-short-float-) (complex-short-float) (complex-short-float complex-short-float))
(define-simple-instruction (- two-arg-complex-single-float-) (complex-single-float) (complex-single-float complex-single-float))
(define-simple-instruction (- two-arg-complex-double-float-) (complex-double-float) (complex-double-float complex-double-float))
(define-simple-instruction (- two-arg-complex-long-float-) (complex-long-float) (complex-long-float complex-long-float))

(define-simple-instruction (- one-arg-integer-) (integer) (integer integer))
(define-simple-instruction (- one-arg-short-float-) (short-float) (short-float))
(define-simple-instruction (- one-arg-single-float-) (single-float) (single-float))
(define-simple-instruction (- one-arg-double-float-) (double-float) (double-float))
(define-simple-instruction (- one-arg-long-float-) (long-float) (long-float))
(define-simple-instruction (- one-arg-complex-short-float-) (complex-short-float) (complex-short-float))
(define-simple-instruction (- one-arg-complex-single-float-) (complex-single-float) (complex-single-float))
(define-simple-instruction (- one-arg-complex-double-float-) (complex-double-float) (complex-double-float))
(define-simple-instruction (- one-arg-complex-long-float-) (complex-long-float) (complex-long-float))

(define-fnrecord 1- (number)
  (:properties :foldable :movable)
  (:differentiator _ (declare (ignore number)) (wrap 1))
  (:specializer (wrap (- number 1))))
