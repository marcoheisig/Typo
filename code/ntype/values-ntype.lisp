(in-package #:typo.ntype)

(defgeneric values-ntype-nth-value-ntype (n values-ntype)
  (:documentation
   "Returns the ntype corresponding to the Nth argument of the supplied
VALUES-NTYPE."))

(defgeneric values-ntype-minimum-number-of-values (values-ntype)
  (:documentation
   "Returns an integer that is the mininum number of values returned by the
supplied VALUES-NTYPE."))

(defgeneric values-ntype-number-of-optional-values (values-ntype)
  (:documentation
   "Returns an integer that is the number of optional values returned by the
supplied VALUES-NTYPE."))

(defgeneric values-ntype-number-of-non-rest-values (values-ntype)
  (:documentation
   "Returns an integer that is the sum of the minimum number of values and the
number of optional values returned by the supplied VALUES-NTYPE."))

(defgeneric values-ntype-rest-ntype (values-ntype)
  (:documentation
   "Returns the ntype of all rest values returned by the supplied
VALUES-NTYPE, or NIL, if there are no rest values."))

(defgeneric values-ntype-type-specifier (values-ntype)
  (:documentation
   "Returns the type specifier corresponding to the supplied VALUES-NTYPE."))

(defgeneric values-ntype-union (vn1 vn2)
  (:documentation
   "Returns the values ntype that is the union of the two supplied ones."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; VALUES-NTYPE

(defstruct (values-ntype
            (:predicate values-ntype-p)
            (:copier nil)
            (:constructor nil)))

(defmethod values-ntype-nth-value-ntype ((n integer) (values-ntype values-ntype))
  (if (< n 0)
      (error "Index must be non-negative, got ~D." n)
      (ntype-of nil)))

(defmethod values-ntype-rest-ntype ((values-ntype values-ntype))
  nil)

(defmethod values-ntype-type-specifier ((values-ntype values-ntype))
  (let* ((n-required (values-ntype-minimum-number-of-values values-ntype))
         (n-optional (values-ntype-number-of-optional-values values-ntype))
         (n-non-rest (values-ntype-number-of-non-rest-values values-ntype))
         (required
           (loop for n from 0 below n-required
                 collect
                 (ntype-type-specifier
                  (values-ntype-nth-value-ntype n values-ntype))))
         (optional
           (loop for n from n-required below n-non-rest
                 collect
                 (ntype-type-specifier
                  (values-ntype-nth-value-ntype n values-ntype))))
         (rest
           (if (values-ntype-rest-ntype values-ntype)
               (ntype-type-specifier
                (values-ntype-rest-ntype values-ntype))
               nil)))
    (if (not rest)
        (if (zerop n-optional)
            `(values ,@required &optional)
            `(values ,@required &optional ,@optional))
        (if (zerop n-optional)
            `(values ,@required &rest ,rest)
            `(values ,@required &optional ,@optional &rest ,rest)))))

(defmethod print-object ((values-ntype values-ntype) stream)
  (flet ((nth-value-type (n)
           (ntype-type-specifier
            (values-ntype-nth-value-ntype n values-ntype))))
    (let* ((n-required (values-ntype-minimum-number-of-values values-ntype))
           (n-optional (values-ntype-number-of-optional-values values-ntype))
           (n-non-rest (values-ntype-number-of-non-rest-values values-ntype))
           (required (loop for n from 0 below n-required collect (nth-value-type n)))
           (optional (loop for n from n-required below n-non-rest collect (nth-value-type n)))
           (rest (values-ntype-rest-ntype values-ntype))
           (print-spec
             (if (not rest)
                 (if (zerop n-optional)
                     `((values-ntype ,@required))
                     `((values-ntype ,@required)
                       (&optional ,@optional)))
                 (if (zerop n-optional)
                     `((values-ntype ,@required)
                       (&rest ,(ntype-type-specifier rest)))
                     `((values-ntype ,@required)
                       (&optional ,@optional)
                       (&rest ,(ntype-type-specifier rest)))))))
      (print-unreadable-object (values-ntype stream)
        (pprint-logical-block (stream print-spec)
          (loop
            (pprint-logical-block (stream (pprint-pop))
              (loop
                (write (pprint-pop) :stream stream)
                (pprint-exit-if-list-exhausted)
                (write-char #\space stream)
                (pprint-newline :fill stream)))
            (pprint-exit-if-list-exhausted)
            (write-char #\space stream)
            (pprint-newline :fill stream)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SINGLE-VALUE-VALUES-NTYPE

(defstruct (single-value-values-ntype
            (:include values-ntype)
            (:constructor %make-single-value-values-ntype (ntype)))
  (ntype nil :type ntype :read-only t))

(let ((table (trivial-garbage:make-weak-hash-table :weakness :value :test #'eq)))
  (defun make-single-value-values-ntype (ntype)
    (declare (ntype ntype))
    (values
     (alexandria:ensure-gethash ntype table (%make-single-value-values-ntype ntype)))))

(defmethod values-ntype-nth-value-ntype ((n (eql 0)) (single-value-values-ntype single-value-values-ntype))
  (single-value-values-ntype-ntype single-value-values-ntype))

(defmethod values-ntype-minimum-number-of-values ((single-value-values-ntype single-value-values-ntype))
  1)

(defmethod values-ntype-number-of-optional-values ((single-value-values-ntype single-value-values-ntype))
  0)

(defmethod values-ntype-number-of-non-rest-values ((single-value-values-ntype single-value-values-ntype))
  1)

(defmethod values-ntype-union ((svvn1 single-value-values-ntype)
                               (svvn2 single-value-values-ntype))
  (make-single-value-values-ntype
   (ntype-union
    (single-value-values-ntype-ntype svvn1)
    (single-value-values-ntype-ntype svvn2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FULL-VALUES-NTYPE

(defstruct (full-values-ntype
            (:include values-ntype)
            (:constructor %make-full-values-ntype))
  (ntypes nil :type simple-vector :read-only t)
  (nmin nil :type unsigned-byte :read-only t)
  (rest nil :type (or null ntype) :read-only t))

(let ((rest-table (make-hash-table :test #'eq)))
  (defun make-values-ntype (required optional rest)
    (declare (sequence required)
             (sequence optional)
             (type (or null ntype) rest))
    (let ((n-required (length required))
          (n-optional (length optional)))
      (if (and (= 1 n-required)
               (= 0 n-optional)
               (not rest))
          (make-single-value-values-ntype (elt required 0))
          (let* ((nmin-table
                   (alexandria:ensure-gethash
                    rest rest-table
                    (make-hash-table :test #'eql)))
                 (ntypes-table
                   (alexandria:ensure-gethash
                    n-required nmin-table
                    (trivial-garbage:make-weak-hash-table :weakness :value :test #'equalp)))
                 (ntypes (make-array (+ n-required n-optional))))
            (replace ntypes required)
            (replace ntypes optional :start1 n-required)
            (values
             (alexandria:ensure-gethash
              ntypes
              ntypes-table
              (%make-full-values-ntype
               :ntypes ntypes
               :nmin n-required
               :rest rest))))))))

(defmethod values-ntype-nth-value-ntype ((n integer) (full-values-ntype full-values-ntype))
  (with-accessors ((ntypes full-values-ntype-ntypes)
                   (nmin full-values-ntype-nmin)
                   (rest full-values-ntype-rest)) full-values-ntype
    (if (< n (length ntypes))
        (if (minusp n)
            (call-next-method)
            (svref ntypes n))
        (etypecase rest
          (null (call-next-method))
          (ntype rest)))))

(defmethod values-ntype-minimum-number-of-values ((full-values-ntype full-values-ntype))
  (full-values-ntype-nmin full-values-ntype))

(defmethod values-ntype-number-of-optional-values ((full-values-ntype full-values-ntype))
  (- (length (full-values-ntype-ntypes full-values-ntype))
     (full-values-ntype-nmin full-values-ntype)))

(defmethod values-ntype-number-of-non-rest-values ((full-values-ntype full-values-ntype))
  (length (full-values-ntype-ntypes full-values-ntype)))

(defmethod values-ntype-rest-ntype ((full-values-ntype full-values-ntype))
  (full-values-ntype-rest full-values-ntype))

(defmethod values-ntype-union ((vn1 values-ntype)
                               (vn2 values-ntype))
  (let* ((minimum (min (values-ntype-minimum-number-of-values vn1)
                       (values-ntype-minimum-number-of-values vn2)))
         (nonrest1 (values-ntype-number-of-non-rest-values vn1))
         (nonrest2 (values-ntype-number-of-non-rest-values vn2))
         (rest1 (values-ntype-rest-ntype vn1))
         (rest2 (values-ntype-rest-ntype vn1)))
    (make-values-ntype
     (loop for index below minimum
           collect (ntype-union (values-ntype-nth-value-ntype index vn1)
                                (values-ntype-nth-value-ntype index vn2)))
     (loop for index from minimum below (max nonrest1 nonrest2)
           collect (ntype-union
                    (values-ntype-nth-value-ntype index vn1)
                    (values-ntype-nth-value-ntype index vn2)))
     (cond ((and (ntypep rest1)
                 (ntypep rest2))
            (ntype-union rest1 rest2))
           ((ntypep rest1) rest1)
           ((ntypep rest2) rest2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Constructors

(defun values-type-specifier-values-ntype (type-specifier)
  (trivia:match type-specifier
    ((list 'values type-specifier '&optional)
     (make-single-value-values-ntype
      (type-specifier-ntype type-specifier)))
    ((list* 'values rest)
     (labels ((fail () (trivia.fail:fail))
              (parse-required (x required)
                (trivia:ematch x
                  ((list)
                   (make-result required '() (universal-ntype)))
                  ((list* '&optional rest)
                   (parse-optional rest required '()))
                  ((list* '&rest rest)
                   (parse-rest rest required '()))
                  ((list* type-specifier rest)
                   (parse-required
                    rest
                    (cons (type-specifier-ntype type-specifier) required)))))
              (parse-optional (x required optional)
                (trivia:ematch x
                  ((list)
                   (make-result required optional nil))
                  ((list* '&rest rest)
                   (parse-rest rest required optional))
                  ((list* type-specifier rest)
                   (parse-optional
                    rest
                    required
                    (cons (type-specifier-ntype type-specifier) optional)))))
              (parse-rest (x required optional)
                (trivia:ematch x
                  ((or (list type-specifier)
                       (list type-specifier '&allow-other-keys))
                   (make-result required optional (type-specifier-ntype type-specifier)))
                  (_ (fail))))
              (make-result (required optional rest)
                (make-values-ntype
                 (nreverse required)
                 (nreverse optional)
                 rest)))
       (parse-required rest '())))
    (_
     (error "Not a valid values type specifier: ~S" type-specifier))))

(define-compiler-macro values-type-specifier-values-ntype (&whole whole type-specifier)
  (if (not (constantp type-specifier))
      whole
      `(load-time-value
        (locally (declare (notinline values-type-specifier-ntype))
          (values-type-specifier-ntype ,type-specifier)))))
