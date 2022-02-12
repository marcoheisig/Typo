(in-package #:typo)

(define-specializer signum (number)
  (let ((ntype (wrapper-ntype number)))
    (with-constant-folding (signum (ntype number))
      (ntype-subtypecase ntype
        (rational (wrap-default (type-specifier-ntype 'integer)))
        ((or float (complex float)) (wrap-default ntype))
        (complex (wrap-default (type-specifier-ntype 'complex)))
        (t (wrap-default 'number))))))

(define-differentiator signum (number) index
  (declare (ignore number))
  ;; Should I care about the discontinuity at zero?  Nah.
  0)
