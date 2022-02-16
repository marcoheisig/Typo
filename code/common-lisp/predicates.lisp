(in-package #:typo.fndb)

(defmacro define-predicate-fndb-record (predicate type-specifier)
  `(define-fndb-record ,predicate (object)
     (:pure t)
     (:specializer
      (let ((ntype (wrapper-ntype object)))
        (ntype-subtypecase ntype
          ((not ,type-specifier) (wrap nil))
          (,type-specifier (wrap-default (type-specifier-ntype '(not null))))
          (t (wrap-default (type-specifier-ntype 'generalized-boolean))))))))

(define-predicate-fndb-record arrayp array)
(define-predicate-fndb-record bit-vector-p bit-vector)
(define-predicate-fndb-record characterp character)
(define-predicate-fndb-record compiled-function-p compiled-function)
(define-predicate-fndb-record complexp complex)
(define-predicate-fndb-record consp cons)
(define-predicate-fndb-record floatp float)
(define-predicate-fndb-record functionp function)
(define-predicate-fndb-record hash-table-p hash-table)
(define-predicate-fndb-record integerp integer)
(define-predicate-fndb-record keywordp keyword)
(define-predicate-fndb-record listp list)
(define-predicate-fndb-record numberp number)
(define-predicate-fndb-record packagep package)
(define-predicate-fndb-record random-state-p random-state)
(define-predicate-fndb-record rationalp rational)
(define-predicate-fndb-record realp real)
(define-predicate-fndb-record streamp stream)

;;; The remaining rules cannot be handled by DEFINE-PREDICATE-FNDB-RECORD,
;;; because the domain of these functions is limited to certain numbers.

(define-fndb-record minusp (real)
  (:pure t)
  (:specializer
   (ntype-subtypecase (wrapper-ntype real)
     ((not real) (abort-specialization))
     ((real * (0)) (wrap-default (type-specifier-ntype '(not null))))
     ((real 0 *) (wrap nil))
     (t (wrap-default (type-specifier-ntype 'generalized-boolean))))))

(define-fndb-record plusp (real)
  (:pure t)
  (:specializer
   (ntype-subtypecase (wrapper-ntype real)
     ((not real) (abort-specialization))
     ((real (0) *) (wrap-default (type-specifier-ntype '(not null))))
     ((real * 0) (wrap nil))
     (t (wrap-default (type-specifier-ntype 'generalized-boolean))))))

(define-fndb-record zerop (number)
  (:pure t)
  (:specializer
   (ntype-subtypecase (wrapper-ntype number)
     ((not real) (abort-specialization))
     (zero (wrap-default (type-specifier-ntype '(not null))))
     ((not zero) (wrap nil))
     (t (wrap-default (type-specifier-ntype 'generalized-boolean))))))

(define-fndb-record evenp (integer)
  (:pure t)
  (:specializer
   (ntype-subtypecase (wrapper-ntype integer)
     ((not real) (abort-specialization))
     (t (wrap-default (type-specifier-ntype 'generalized-boolean))))))

(define-fndb-record oddp (integer)
  (:pure t)
  (:specializer
   (ntype-subtypecase (wrapper-ntype integer)
     ((not real) (abort-specialization))
     (t (wrap-default (type-specifier-ntype 'generalized-boolean))))))
