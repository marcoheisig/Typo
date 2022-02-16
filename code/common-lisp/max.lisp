(in-package #:typo.fndb)

(define-fndb-record max (real &rest more-reals)
  (:pure t)
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
                    (short-float (wrap (short-float-max a b)))
                    (t (wrap-default (ntype-union ntype-of-a ntype-of-b)))))
                 (single-float
                  (ntype-subtypecase ntype-of-b
                    ((not real) (abort-specialization))
                    (single-float (wrap (single-float-max a b)))
                    (t (wrap-default (ntype-union ntype-of-a ntype-of-b)))))
                 (double-float
                  (ntype-subtypecase ntype-of-b
                    ((not real) (abort-specialization))
                    (double-float (wrap (double-float-max a b)))
                    (t (wrap-default (ntype-union ntype-of-a ntype-of-b)))))
                 (long-float
                  (ntype-subtypecase ntype-of-b
                    ((not real) (abort-specialization))
                    (long-float (wrap (long-float-max a b)))
                    (t (wrap-default (ntype-union ntype-of-a ntype-of-b)))))
                 (t
                  (ntype-subtypecase ntype-of-b
                    ((not real) (abort-specialization))
                    (t (wrap-default (ntype-union ntype-of-a ntype-of-b))))))))
           more-reals
           :initial-value real)))))

(define-simple-instruction (max short-float-max) (short-float) (short-float short-float))
(define-simple-instruction (max single-float-max) (single-float) (single-float single-float))
(define-simple-instruction (max double-float-max) (double-float) (double-float double-float))
(define-simple-instruction (max long-float-max) (long-float) (long-float long-float))

(define-fndb-record argmax (real &rest more-reals)
  (:pure t)
  (:specializer (wrap-default (type-specifier-ntype 'argument-index))))
