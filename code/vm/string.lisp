(in-package #:typo.vm)

(define-fnrecord char (string index)
  (:properties :foldable)
  (:specializer
   (assert-wrapper-type index unsigned-byte)
   (etypecase (wrapper-ntype string)
     ((not string) (abort-specialization))
     (t (wrap-default (type-specifier-ntype 'character))))))

(define-fnrecord (setf char) (value string index)
  (:properties :foldable)
  (:specializer
   (assert-wrapper-type value character)
   (assert-wrapper-type index unsigned-byte)
   (etypecase (wrapper-ntype string)
     ((not string) (abort-specialization))
     (t (wrap-default (wrapper-ntype value))))))

(define-fnrecord schar (simple-string index)
  (:properties :foldable)
  (:specializer
   (assert-wrapper-type index unsigned-byte)
   (etypecase (wrapper-ntype simple-string)
     ((not simple-string) (abort-specialization))
     (t (wrap-default (type-specifier-ntype 'character))))))

(define-fnrecord (setf char) (value simple-string index)
  (:properties :foldable)
  (:specializer
   (assert-wrapper-type value character)
   (assert-wrapper-type index unsigned-byte)
   (etypecase (wrapper-ntype simple-string)
     ((not simple-string) (abort-specialization))
     (t (wrap-default (wrapper-ntype value))))))
