(in-package #:typo.vm)

(define-fnrecord subtypep (type-1 type-2 &optional environment)
  (:properties :foldable)
  (:specializer
   (wrap-default
    (type-specifier-ntype 'generalized-boolean)
    (type-specifier-ntype 'generalized-boolean))))

(define-fnrecord type-of (object)
  (:properties :foldable)
  (:specializer
   (wrap-default (type-specifier-ntype 'type-specifier))))

(define-fnrecord typep (object type-specifier &optional (environment nil environment-p))
  (:properties :foldable)
  (:specializer
   (let ((object-ntype (wrapper-ntype object))
         (type-specifier-ntype (wrapper-ntype type-specifier))
         (environment-ntype (if environment-p
                                (wrapper-ntype environment)
                                (type-specifier-ntype 'null))))
     (declare (ignore environment-ntype))
     (if (not (eql-ntype-p type-specifier-ntype))
         (wrap-default (type-specifier-ntype 'generalized-boolean))
         (multiple-value-bind (ntype precise-p)
             (type-specifier-ntype (eql-ntype-object type-specifier-ntype))
           (cond ((and precise-p (ntype-subtypep object-ntype ntype))
                  (wrap-default (true-ntype)))
                 ((ntype-subtypepc2 object-ntype ntype)
                  (wrap-constant nil))
                 (t (wrap-default (type-specifier-ntype 'generalized-boolean)))))))))
