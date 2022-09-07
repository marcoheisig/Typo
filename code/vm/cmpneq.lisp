(in-package #:typo.vm)

(define-fnrecord /= (number &rest more-numbers)
  (:properties :foldable :movable)
  (:specializer
   (if (null more-numbers)
       (wrap
        (prog2-fn
         (the-number number)
         t))
       ;; This code produces (N^2-N)/2 comparisons for N supplied numbers.
       ;; But whoever calls /= with more than two arguments deserves it.
       (let ((value (wrap t)))
         (map-unique-pairs
          (lambda (a b)
            (setf value
                  (wrap
                   (and-fn value (two-arg/= a b)))))
          (list* number more-numbers))
         value))))

(defun map-unique-pairs (fn list)
  (loop for sublist on list
        for a = (first sublist) do
          (loop for b in (rest sublist) do
            (funcall fn a b))))

(define-simple-instruction (/= two-arg-short-float/=) (generalized-boolean) (short-float short-float))
(define-simple-instruction (/= two-arg-single-float/=) (generalized-boolean) (single-float single-float))
(define-simple-instruction (/= two-arg-double-float/=) (generalized-boolean) (double-float double-float))
(define-simple-instruction (/= two-arg-long-float/=) (generalized-boolean) (long-float long-float))
(define-simple-instruction (/= two-arg-complex-short-float/=) (generalized-boolean) (complex-short-float complex-short-float))
(define-simple-instruction (/= two-arg-complex-single-float/=) (generalized-boolean) (complex-single-float complex-single-float))
(define-simple-instruction (/= two-arg-complex-double-float/=) (generalized-boolean) (complex-double-float complex-double-float))
(define-simple-instruction (/= two-arg-complex-long-float/=) (generalized-boolean) (complex-long-float complex-long-float))
(define-instruction (= two-arg/=) (generalized-boolean) (a b)
  (ntype-subtypecase
      (ntype-contagion
       (wrapper-ntype a)
       (wrapper-ntype b))
    ((not number) (abort-specialization))
    (short-float
     (wrap
      (two-arg-short-float/=
       (coerce-to-short-float a)
       (coerce-to-short-float b))))
    (single-float
     (wrap
      (two-arg-single-float/=
       (coerce-to-single-float a)
       (coerce-to-single-float b))))
    (double-float
     (wrap
      (two-arg-double-float/=
       (coerce-to-double-float a)
       (coerce-to-double-float b))))
    (long-float
     (wrap
      (two-arg-long-float/=
       (coerce-to-long-float a)
       (coerce-to-long-float b))))
    ((complex short-float)
     (wrap
      (two-arg-complex-short-float/=
       (coerce-to-complex-short-float a)
       (coerce-to-complex-short-float b))))
    ((complex single-float)
     (wrap
      (two-arg-complex-single-float/=
       (coerce-to-complex-single-float a)
       (coerce-to-complex-single-float b))))
    ((complex double-float)
     (wrap
      (two-arg-complex-double-float/=
       (coerce-to-complex-double-float a)
       (coerce-to-complex-double-float b))))
    ((complex long-float)
     (wrap
      (two-arg-complex-long-float/=
       (coerce-to-complex-long-float a)
       (coerce-to-complex-long-float b))))
    (t
     (wrap-default
      (type-specifier-ntype 'generalized-boolean)))))




