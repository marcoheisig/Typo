(in-package #:typo)

;;; A hash table whose keys are lists of the form (element-ntype dimensions
;;; simplep), and whose values are the corresponding array ntypes.
(defvar *array-ntype-table*
  (trivial-garbage:make-weak-hash-table
   :test #'equal
   :weakness :value))

;;; Populate the array ntype table with the one array that has an
;;; equivalent primitive type.
(setf (gethash (list '* '* nil) *array-ntype-table*)
      (find-primitive-ntype 'array))

(defmethod make-array-ntype (&key (element-type '*) (dimensions '*) (simplep nil))
  (check-type simplep boolean)
  (multiple-value-bind (element-ntype precise-p)
      (canonicalize-array-element-type element-type)
    (multiple-value-bind (dimensions vectorp)
        (canonicalize-array-dimension-specifier dimensions)
      (let ((key #1=(list element-ntype dimensions simplep)))
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
         precise-p)))))

(defun canonicalize-array-dimension-specifier (dimensions)
  (typecase dimensions
    ((eql *)
     (values '* nil))
    ((integer 0 #.array-rank-limit)
     (values dimensions (= dimensions 1)))
    (list
     (loop with simple = t
           for dimension in dimensions
           for length from 0 do
             (unless (eql dimension '*)
               (if (typep dimension '(integer 0 #.array-rank-limit))
                   (setf simple nil)
                   (error "Invalid array dimension specifier: ~S"
                          dimension)))
           finally
              (return
                (values (if simple length dimensions)
                        (= length 1)))))
    (otherwise
     (error "Invalid array dimensions specifier: ~S"
            dimensions))))

(defun canonicalize-array-element-type (element-type)
  (if (eql element-type '*)
      (values '* t)
      (multiple-value-bind (aet aet-precise-p)
          (declaim (notinline type-specifier-ntype))
          (type-specifier-ntype element-type)
        (multiple-value-bind (uaet uaet-precise-p)
            (upgraded-array-element-ntype aet)
          (values uaet (and aet-precise-p uaet-precise-p))))))

(defmethod make-load-form ((array-ntype array-ntype) &optional env)
  (declare (ignore env))
  `(load-time-value
    (make-array-ntype
     :element-type ',(array-ntype-element-ntype array-ntype)
     :dimensions ',(array-ntype-dimensions array-ntype)
     :simplep ',(array-ntype-simplep array-ntype))))
