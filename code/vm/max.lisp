(in-package #:typo.vm)

(define-fnrecord max (real &rest more-reals)
  (:properties :foldable :movable)
  (:differentiator
   index
   (funcall (function-specializer 'if)
            (funcall (function-specializer '=)
                     (wrap-constant index)
                     (apply (function-specializer 'argmax) real more-reals))
            (wrap-constant 1)
            (wrap-constant 0)))
  (:specializer
   (cond ((null more-reals)
          (wrap (the-real real)))
         (t
          (reduce
           (lambda (a b)
             (let ((ntype-of-a (wrapper-ntype a))
                   (ntype-of-b (wrapper-ntype b)))
               (ntype-subtypecase ntype-of-a
                 ((not real) (abort-specialization))
                 (short-float
                  (ntype-subtypecase ntype-of-b
                    ((not real) (abort-specialization))
                    (short-float (wrap (two-arg-short-float-max a b)))
                    (t (wrap-default (ntype-union ntype-of-a ntype-of-b)))))
                 (single-float
                  (ntype-subtypecase ntype-of-b
                    ((not real) (abort-specialization))
                    (single-float (wrap (two-arg-single-float-max a b)))
                    (t (wrap-default (ntype-union ntype-of-a ntype-of-b)))))
                 (double-float
                  (ntype-subtypecase ntype-of-b
                    ((not real) (abort-specialization))
                    (double-float (wrap (two-arg-double-float-max a b)))
                    (t (wrap-default (ntype-union ntype-of-a ntype-of-b)))))
                 (long-float
                  (ntype-subtypecase ntype-of-b
                    ((not real) (abort-specialization))
                    (long-float (wrap (two-arg-long-float-max a b)))
                    (t (wrap-default (ntype-union ntype-of-a ntype-of-b)))))
                 (t
                  (ntype-subtypecase ntype-of-b
                    ((not real) (abort-specialization))
                    (t (wrap-default (ntype-union ntype-of-a ntype-of-b))))))))
           more-reals
           :initial-value real)))))

(define-simple-instruction (max two-arg-short-float-max) (short-float) (short-float short-float))
(define-simple-instruction (max two-arg-single-float-max) (single-float) (single-float single-float))
(define-simple-instruction (max two-arg-double-float-max) (double-float) (double-float double-float))
(define-simple-instruction (max two-arg-long-float-max) (long-float) (long-float long-float))

(define-fnrecord argmax (real &rest more-reals)
  (:properties :foldable :movable)
  (:specializer (wrap-default (type-specifier-ntype 'argument-index))))
