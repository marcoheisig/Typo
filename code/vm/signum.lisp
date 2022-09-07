(in-package #:typo.vm)

(define-fnrecord signum (number)
  (:pure t)
  ;; Should I care about the discontinuity at zero?  Nah.
  (:differentiator _ (declare (ignore number)) 0)
  (:specializer
   (let ((ntype (wrapper-ntype number)))
     (ntype-subtypecase ntype
       (rational (wrap-default (type-specifier-ntype 'integer)))
       ((or float (complex float)) (wrap-default ntype))
       (complex (wrap-default (type-specifier-ntype 'complex)))
       (t (wrap-default 'number))))))
