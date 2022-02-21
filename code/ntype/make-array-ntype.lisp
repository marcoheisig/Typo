(in-package #:typo.ntype)

;;; A hash table whose keys are lists of the form (element-ntype dimensions
;;; simplep), and whose values are the corresponding array ntypes.
(defvar *array-ntype-table*
  (trivial-garbage:make-weak-hash-table
   :test #'equal
   :weakness :value))

(defmethod make-array-ntype (&key (element-type '*) (dimensions '*) (simplep nil))
  (check-type simplep boolean)
  (multiple-value-bind (element-ntype precise-p)
      (canonicalize-array-element-type element-type)
    (let* ((dimensions (canonicalize-array-dimension-specifier dimensions))
           (key #1=(list element-ntype dimensions simplep)))
      (declare (dynamic-extent key))
      (values
       (or (gethash key *array-ntype-table*)
           (setf (gethash #1# *array-ntype-table*)
                 (%make-array-ntype
                  :element-ntype element-ntype
                  :dimensions dimensions
                  :simplep simplep
                  :index
                  (primitive-ntype-index
                   (find-primitive-ntype 'array)))))
       precise-p))))

(define-compiler-macro make-array-ntype (&whole form &rest args)
  (if (and (evenp (length args))
           (every #'constantp args))
      `(load-time-value
        (locally (declare (notinline make-array-ntype))
          (make-array-ntype ,@args)))
      form))

(defun canonicalize-array-dimension-specifier (dimensions)
  (typecase dimensions
    ((eql *) dimensions)
    ((integer 0 #.array-rank-limit) dimensions)
    (list
     (loop with *-only = t
           for length from 0
           for dimension in dimensions do
             (unless (eql dimension '*)
               (setf *-only nil)
               (unless (typep dimension '(integer 0 #.array-rank-limit))
                 (error "Invalid array dimension specifier: ~S"
                        dimension)))
           finally (return (if *-only length dimensions))))
    (otherwise
     (error "Invalid array dimensions specifier: ~S"
            dimensions))))

(defun canonicalize-array-element-type (element-type)
  (if (eql element-type '*)
      (values '* t)
      (multiple-value-bind (aet aet-precise-p)
          (type-specifier-ntype element-type)
        (multiple-value-bind (uaet uaet-precise-p)
            (upgraded-array-element-ntype aet)
          (values uaet (and aet-precise-p uaet-precise-p))))))

(defmethod make-load-form ((array-ntype array-ntype) &optional env)
  (declare (ignore env))
  `(make-array-ntype
    :element-type ',(array-ntype-element-type array-ntype)
    :dimensions ',(array-ntype-dimensions array-ntype)
    :simplep ',(array-ntype-simplep array-ntype)))

(defmethod ntype-primitive-ntype
    ((ntype (eql (make-array-ntype))))
  (find-primitive-ntype 'array))
