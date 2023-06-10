(in-package #:typo.vm)

(defmacro define-predicate-fnrecord (predicate type-specifier)
  `(define-fnrecord ,predicate (object)
     (:properties :foldable :movable)
     (:specializer
      (let ((ntype (wrapper-ntype object)))
        (ntype-subtypecase ntype
          ((not ,type-specifier) (wrap nil))
          (,type-specifier (wrap-default (true-ntype)))
          (t (wrap-default (type-specifier-ntype 'generalized-boolean))))))))

(define-predicate-fnrecord arrayp array)
(define-predicate-fnrecord bit-vector-p bit-vector)
(define-predicate-fnrecord characterp character)
(define-predicate-fnrecord compiled-function-p compiled-function)
(define-predicate-fnrecord complexp complex)
(define-predicate-fnrecord consp cons)
(define-predicate-fnrecord floatp float)
(define-predicate-fnrecord functionp function)
(define-predicate-fnrecord hash-table-p hash-table)
(define-predicate-fnrecord integerp integer)
(define-predicate-fnrecord keywordp keyword)
(define-predicate-fnrecord listp list)
(define-predicate-fnrecord numberp number)
(define-predicate-fnrecord packagep package)
(define-predicate-fnrecord random-state-p random-state)
(define-predicate-fnrecord rationalp rational)
(define-predicate-fnrecord realp real)
(define-predicate-fnrecord streamp stream)

;;; The remaining rules cannot be handled by DEFINE-PREDICATE-FNRECORD,
;;; because the domain of these functions is limited to a certain type.

(define-fnrecord minusp (real)
  (:properties :foldable :movable)
  (:specializer
   (ntype-subtypecase (wrapper-ntype real)
     ((not real) (abort-specialization))
     ((real * (0)) (wrap-default (true-ntype)))
     ((real 0 *) (wrap nil))
     (t (wrap-default (type-specifier-ntype 'generalized-boolean))))))

(define-fnrecord plusp (real)
  (:properties :foldable :movable)
  (:specializer
   (ntype-subtypecase (wrapper-ntype real)
     ((not real) (abort-specialization))
     ((real (0) *) (wrap-default (true-ntype)))
     ((real * 0) (wrap nil))
     (t (wrap-default (type-specifier-ntype 'generalized-boolean))))))

(define-fnrecord zerop (number)
  (:properties :foldable :movable)
  (:specializer
   (ntype-subtypecase (wrapper-ntype number)
     ((not real) (abort-specialization))
     (zero (wrap-default (true-ntype)))
     ((not zero) (wrap nil))
     (t (wrap-default (type-specifier-ntype 'generalized-boolean))))))

(define-fnrecord evenp (integer)
  (:properties :foldable :movable)
  (:specializer
   (ntype-subtypecase (wrapper-ntype integer)
     ((not real) (abort-specialization))
     (t (wrap-default (type-specifier-ntype 'generalized-boolean))))))

(define-fnrecord oddp (integer)
  (:properties :foldable :movable)
  (:specializer
   (ntype-subtypecase (wrapper-ntype integer)
     ((not real) (abort-specialization))
     (t (wrap-default (type-specifier-ntype 'generalized-boolean))))))

(define-fnrecord alpha-char-p (char)
  (:properties :foldable :movable)
  (:specializer
   (ntype-subtypecase (wrapper-ntype char)
     ((not character) (abort-specialization))
     (t (wrap-default (type-specifier-ntype 'generalized-boolean))))))

(define-fnrecord digit-char-p (char &optional (radix nil radix-supplied-p))
  (:properties :foldable :movable)
  (:specializer
   (when radix-supplied-p
     (ntype-subtypecase radix
       ((not radix) (abort-specialization))))
   (ntype-subtypecase (wrapper-ntype char)
     ((not character) (abort-specialization))
     (t (wrap-default (type-specifier-ntype 'generalized-boolean))))))

(define-fnrecord graphic-char-p (char)
  (:properties :foldable :movable)
  (:specializer
   (ntype-subtypecase (wrapper-ntype char)
     ((not character) (abort-specialization))
     (t (wrap-default (type-specifier-ntype 'generalized-boolean))))))

(define-fnrecord standard-char-p (char)
  (:properties :foldable :movable)
  (:specializer
   (ntype-subtypecase (wrapper-ntype char)
     ((not character) (abort-specialization))
     (t (wrap-default (type-specifier-ntype 'generalized-boolean))))))

(define-fnrecord upper-case-p (char)
  (:properties :foldable :movable)
  (:specializer
   (ntype-subtypecase (wrapper-ntype char)
     ((not character) (abort-specialization))
     (t (wrap-default (type-specifier-ntype 'generalized-boolean))))))

(define-fnrecord lower-case-p (char)
  (:properties :foldable :movable)
  (:specializer
   (ntype-subtypecase (wrapper-ntype char)
     ((not character) (abort-specialization))
     (t (wrap-default (type-specifier-ntype 'generalized-boolean))))))

(define-fnrecord both-case-p (char)
  (:properties :foldable :movable)
  (:specializer
   (ntype-subtypecase (wrapper-ntype char)
     ((not character) (abort-specialization))
     (t (wrap-default (type-specifier-ntype 'generalized-boolean))))))

