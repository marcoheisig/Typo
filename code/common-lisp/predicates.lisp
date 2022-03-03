(in-package #:typo.common-lisp)

(defmacro define-predicate-fnrecord (predicate type-specifier)
  `(define-fnrecord ,predicate (object)
     (:pure t)
     (:specializer
      (let ((ntype (wrapper-ntype object)))
        (ntype-subtypecase ntype
          ((not ,type-specifier) (wrap nil))
          (,type-specifier (wrap-default (type-specifier-ntype '(not null))))
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
;;; because the domain of these functions is limited to certain numbers.

(define-fnrecord minusp (real)
  (:pure t)
  (:specializer
   (ntype-subtypecase (wrapper-ntype real)
     ((not real) (abort-specialization))
     ((real * (0)) (wrap-default (type-specifier-ntype '(not null))))
     ((real 0 *) (wrap nil))
     (t (wrap-default (type-specifier-ntype 'generalized-boolean))))))

(define-fnrecord plusp (real)
  (:pure t)
  (:specializer
   (ntype-subtypecase (wrapper-ntype real)
     ((not real) (abort-specialization))
     ((real (0) *) (wrap-default (type-specifier-ntype '(not null))))
     ((real * 0) (wrap nil))
     (t (wrap-default (type-specifier-ntype 'generalized-boolean))))))

(define-fnrecord zerop (number)
  (:pure t)
  (:specializer
   (ntype-subtypecase (wrapper-ntype number)
     ((not real) (abort-specialization))
     (zero (wrap-default (type-specifier-ntype '(not null))))
     ((not zero) (wrap nil))
     (t (wrap-default (type-specifier-ntype 'generalized-boolean))))))

(define-fnrecord evenp (integer)
  (:pure t)
  (:specializer
   (ntype-subtypecase (wrapper-ntype integer)
     ((not real) (abort-specialization))
     (t (wrap-default (type-specifier-ntype 'generalized-boolean))))))

(define-fnrecord oddp (integer)
  (:pure t)
  (:specializer
   (ntype-subtypecase (wrapper-ntype integer)
     ((not real) (abort-specialization))
     (t (wrap-default (type-specifier-ntype 'generalized-boolean))))))
