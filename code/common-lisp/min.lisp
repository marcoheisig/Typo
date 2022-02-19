(in-package #:typo.common-lisp)

(define-fndb-record min (real &rest more-reals)
  (:pure t)
  (:differentiator
   index
   (funcall (function-specializer 'if)
            (funcall (function-specializer '=)
                     (wrap-constant index)
                     (apply (function-specializer 'argmin) real more-reals))
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
                    (short-float (wrap (short-float-min a b)))
                    (t (wrap-default (ntype-union ntype-of-a ntype-of-b)))))
                 (single-float
                  (ntype-subtypecase ntype-of-b
                    ((not real) (abort-specialization))
                    (single-float (wrap (single-float-min a b)))
                    (t (wrap-default (ntype-union ntype-of-a ntype-of-b)))))
                 (double-float
                  (ntype-subtypecase ntype-of-b
                    ((not real) (abort-specialization))
                    (double-float (wrap (double-float-min a b)))
                    (t (wrap-default (ntype-union ntype-of-a ntype-of-b)))))
                 (long-float
                  (ntype-subtypecase ntype-of-b
                    ((not real) (abort-specialization))
                    (long-float (wrap (long-float-min a b)))
                    (t (wrap-default (ntype-union ntype-of-a ntype-of-b)))))
                 (t (wrap-default (type-specifier-ntype 'real))))))
           more-reals
           :initial-value real)))))

(define-simple-instruction (min short-float-min) (short-float) (short-float short-float))
(define-simple-instruction (min single-float-min) (single-float) (single-float single-float))
(define-simple-instruction (min double-float-min) (double-float) (double-float double-float))
(define-simple-instruction (min long-float-min) (long-float) (long-float long-float))
