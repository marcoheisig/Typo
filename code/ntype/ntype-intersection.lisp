(in-package #:typo)

(defmethod ntype-intersection
    ((ntype1 ntype)
     (ntype2 ntype))
  (multiple-value-bind (p1 p1-precise-p)
      (ntype-primitive-ntype ntype1)
    (multiple-value-bind (p2 p2-precise-p)
        (ntype-primitive-ntype ntype2)
      (multiple-value-bind (intersection intersection-precise-p)
          (primitive-ntype-intersection p1 p2)
        (values intersection (and p1-precise-p p2-precise-p intersection-precise-p))))))

(defmethod ntype-intersection
    ((ntype1 primitive-ntype)
     (ntype2 primitive-ntype))
  (primitive-ntype-intersection ntype1 ntype2))

(let ((v1-cache (make-array (list +primitive-ntype-limit+ +primitive-ntype-limit+)
                            :element-type 'ntype
                            :initial-element (universal-ntype)))
      (v2-cache (make-array (list +primitive-ntype-limit+ +primitive-ntype-limit+)
                            :element-type 'bit
                            :initial-element 0)))
  (loop for p1 across *primitive-ntypes* do
    (loop for p2 across *primitive-ntypes* do
      (multiple-value-bind (intersection intersection-precise-p)
          (find-primitive-ntype
           `(and ,(ntype-type-specifier p1)
                 ,(ntype-type-specifier p2)))
        (setf (aref v1-cache (ntype-index p1) (ntype-index p2))
              intersection)
        (setf (aref v2-cache (ntype-index p1) (ntype-index p2))
              (if intersection-precise-p 1 0)))))
  (defun primitive-ntype-intersection (p1 p2)
    (declare (primitive-ntype p1 p2))
    (values
     (aref v1-cache (ntype-index p1) (ntype-index p2))
     (plusp (aref v2-cache (ntype-index p1) (ntype-index p2))))))

(defmethod ntype-intersection
    ((ntype1 eql-ntype)
     (ntype2 eql-ntype))
  (if (eql (eql-ntype-object ntype1)
           (eql-ntype-object ntype2))
      (values ntype1 t)
      (values (empty-ntype) t)))

;;; TODO handle array ntypes.
