(in-package #:typo.vm)

(defmacro cmpx (name)
  (flet ((mkname (type)
           (intern (format nil "~:@(two-arg-~S~S~)" type name) #.*package*)))
    `(progn
       (define-fnrecord ,name (real &rest more-reals)
         (:properties :foldable :movable)
         (:specializer
          (if (null more-reals)
              (wrap (prog2-fn (the-real real) t))
              (labels ((cmp (a b)
                         (ntype-subtypecase
                             (ntype-contagion
                              (wrapper-ntype a)
                              (wrapper-ntype b))
                           ((not real) (abort-specialization))
                           (short-float
                            (wrap
                             (,(mkname 'short-float)
                              (coerce-to-short-float a)
                              (coerce-to-short-float b))))
                           (single-float
                            (wrap
                             (,(mkname 'single-float)
                              (coerce-to-single-float a)
                              (coerce-to-single-float b))))
                           (double-float
                            (wrap
                             (,(mkname 'double-float)
                              (coerce-to-double-float a)
                              (coerce-to-double-float b))))
                           (long-float
                            (wrap
                             (,(mkname 'long-float)
                              (coerce-to-long-float a)
                              (coerce-to-long-float b))))
                           (t
                            (wrap-default
                             (type-specifier-ntype 'generalized-boolean))))))
                (let ((a real)
                      (value (wrap t)))
                  (loop for b in more-reals for boolean = (cmp a b) do
                    (setf value (wrap (and-fn value boolean)))
                    (setf a b))
                  value)))))
       (define-simple-instruction (,name ,(mkname 'short-float)) (generalized-boolean) (short-float short-float))
       (define-simple-instruction (,name ,(mkname 'single-float)) (generalized-boolean) (single-float single-float))
       (define-simple-instruction (,name ,(mkname 'double-float)) (generalized-boolean) (double-float double-float))
       (define-simple-instruction (,name ,(mkname 'long-float)) (generalized-boolean) (long-float long-float)))))

(cmpx <)
(cmpx >)
(cmpx <=)
(cmpx >=)
