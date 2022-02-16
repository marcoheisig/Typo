(in-package #:typo.fndb)

(define-fndb-record subtypep (type-1 type-2 &optional environment)
  (:pure t)
  (:specializer
   (wrap-default
    (type-specifier-ntype 'generalized-boolean)
    (type-specifier-ntype 'generalized-boolean))))

(define-fndb-record type-of (object)
  (:pure t)
  (:specializer
   (wrap-default (type-specifier-ntype 'type-specifier))))

(define-fndb-record typep (object type-specifier &optional (environment nil environment-p))
  (:pure t)
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
                  (wrap-default (type-specifier-ntype '(not null))))
                 ((ntype-subtypepc2 object-ntype ntype)
                  (wrap-constant nil))
                 (t (wrap-default (type-specifier-ntype 'generalized-boolean)))))))))
