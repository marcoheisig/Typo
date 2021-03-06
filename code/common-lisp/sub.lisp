(in-package #:typo.common-lisp)

(define-fnrecord - (number &rest more-numbers)
  (:pure t)
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
              (integer (wrap (integer-unary- x)))
              (short-float (wrap (short-float-unary- x)))
              (single-float (wrap (single-float-unary- x)))
              (double-float (wrap (double-float-unary- x)))
              (long-float (wrap (long-float-unary- x)))
              (complex-short-float (wrap (complex-short-float-unary- x)))
              (complex-single-float (wrap (complex-single-float-unary- x)))
              (complex-double-float (wrap (complex-double-float-unary- x)))
              (complex-long-float (wrap (complex-long-float-unary- x)))
              (integer (wrap-default (type-specifier-ntype 'integer)))
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
                       (integer-
                        (the-integer a)
                        (the-integer b))))
                     (short-float
                      (wrap
                       (short-float-
                        (coerce-to-short-float a)
                        (coerce-to-short-float b))))
                     (single-float
                      (wrap
                       (single-float-
                        (coerce-to-single-float a)
                        (coerce-to-single-float b))))
                     (double-float
                      (wrap
                       (double-float-
                        (coerce-to-double-float a)
                        (coerce-to-double-float b))))
                     (long-float
                      (wrap
                       (long-float-
                        (coerce-to-long-float a)
                        (coerce-to-long-float b))))
                     ((complex short-float)
                      (wrap
                       (complex-short-float-
                        (coerce-to-complex-short-float a)
                        (coerce-to-complex-short-float b))))
                     ((complex single-float)
                      (wrap
                       (complex-single-float-
                        (coerce-to-complex-single-float a)
                        (coerce-to-complex-single-float b))))
                     ((complex double-float)
                      (wrap
                       (complex-double-float-
                        (coerce-to-complex-double-float a)
                        (coerce-to-complex-double-float b))))
                     ((complex long-float)
                      (wrap
                       (complex-long-float-
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

(define-simple-instruction (- integer-) (integer) (integer integer))
(define-simple-instruction (- short-float-) (short-float) (short-float short-float))
(define-simple-instruction (- single-float-) (single-float) (single-float single-float))
(define-simple-instruction (- double-float-) (double-float) (double-float double-float))
(define-simple-instruction (- long-float-) (long-float) (long-float long-float))
(define-simple-instruction (- complex-short-float-) (complex-short-float) (complex-short-float complex-short-float))
(define-simple-instruction (- complex-single-float-) (complex-single-float) (complex-single-float complex-single-float))
(define-simple-instruction (- complex-double-float-) (complex-double-float) (complex-double-float complex-double-float))
(define-simple-instruction (- complex-long-float-) (complex-long-float) (complex-long-float complex-long-float))

(define-simple-instruction (- integer-unary-) (integer) (integer integer))
(define-simple-instruction (- short-float-unary-) (short-float) (short-float))
(define-simple-instruction (- single-float-unary-) (single-float) (single-float))
(define-simple-instruction (- double-float-unary-) (double-float) (double-float))
(define-simple-instruction (- long-float-unary-) (long-float) (long-float))
(define-simple-instruction (- complex-short-float-unary-) (complex-short-float) (complex-short-float))
(define-simple-instruction (- complex-single-float-unary-) (complex-single-float) (complex-single-float))
(define-simple-instruction (- complex-double-float-unary-) (complex-double-float) (complex-double-float))
(define-simple-instruction (- complex-long-float-unary-) (complex-long-float) (complex-long-float))

(define-fnrecord 1- (number)
  (:pure t)
  (:differentiator _ (declare (ignore number)) (wrap 1))
  (:specializer (wrap (- number 1))))
