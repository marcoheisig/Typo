(in-package #:typo)

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
                     (if (not vectorp)
                         (find-primitive-ntype 'array)
                         (cond ((eql element-ntype '*)
                                (find-primitive-ntype 'vector))
                               ((ntype= element-ntype (find-primitive-ntype 't))
                                (if simplep
                                    (find-primitive-ntype 'simple-vector)
                                    (find-primitive-ntype 'vector)))
                               ((ntype= element-type (find-primitive-ntype 'bit))
                                (if simplep
                                    (find-primitive-ntype 'simple-bit-vector)
                                    (find-primitive-ntype 'bit-vector)))
                               ((ntype= element-type (find-primitive-ntype 'character))
                                (if simplep
                                    (find-primitive-ntype 'simple-string)
                                    (find-primitive-ntype 'string)))
                               (t
                                (find-primitive-ntype 'vector))))))))
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

;;; Populate the table with entries that have an equivalent primitive ntype
;;; instead of an array ntype.

(setf (gethash (list '* '* nil) *array-ntype-table*)
      (find-primitive-ntype 'array))

(setf (gethash (list '* '* t) *array-ntype-table*)
      (find-primitive-ntype 'simple-array))

(setf (gethash (list '* 1 nil) *array-ntype-table*)
      (find-primitive-ntype 'vector))

(setf (gethash (list (find-primitive-ntype 't) 1 t) *array-ntype-table*)
      (find-primitive-ntype 'simple-vector))

(setf (gethash (list (find-primitive-ntype 'bit) 1 nil) *array-ntype-table*)
      (find-primitive-ntype 'bit-vector))

(setf (gethash (list (find-primitive-ntype 'bit) 1 t) *array-ntype-table*)
      (find-primitive-ntype 'simple-bit-vector))

(setf (gethash (list (find-primitive-ntype 'character) 1 nil) *array-ntype-table*)
      (find-primitive-ntype 'string))

(setf (gethash (list (find-primitive-ntype 'character) 1 t) *array-ntype-table*)
      (find-primitive-ntype 'simple-string))

(defmethod make-load-form ((array-ntype array-ntype) &optional env)
  (declare (ignore env))
  `(load-time-value
    (make-array-ntype
     :element-type ',(array-ntype-element-ntype array-ntype)
     :dimensions ',(array-ntype-dimensions array-ntype)
     :simplep ',(array-ntype-simplep array-ntype))))
