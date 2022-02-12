(in-package #:typo)

(defmethod ntype-union
    ((ntype1 ntype)
     (ntype2 ntype))
  (primitive-ntype-union
   (ntype-primitive-ntype ntype1)
   (ntype-primitive-ntype ntype2)))

(defmethod ntype-union
    ((ntype1 primitive-ntype)
     (ntype2 primitive-ntype))
  (primitive-ntype-union ntype1 ntype2))

(let ((v1-cache (make-array (list +primitive-ntype-limit+ +primitive-ntype-limit+)
                            :element-type 'ntype
                            :initial-element (universal-ntype)))
      (v2-cache (make-array (list +primitive-ntype-limit+ +primitive-ntype-limit+)
                            :element-type 'bit
                            :initial-element 0)))
  (loop for p1 across *primitive-ntypes* do
    (loop for p2 across *primitive-ntypes* do
      (multiple-value-bind (union union-precise-p)
          (find-primitive-ntype
           `(or ,(ntype-type-specifier p1)
                ,(ntype-type-specifier p2)))
        (setf (aref v1-cache (ntype-index p1) (ntype-index p2))
              union)
        (setf (aref v2-cache (ntype-index p1) (ntype-index p2))
              (if union-precise-p 1 0)))))
  (defun primitive-ntype-union (p1 p2)
    (declare (primitive-ntype p1 p2))
    (values
     (aref v1-cache (ntype-index p1) (ntype-index p2))
     (plusp (aref v2-cache (ntype-index p1) (ntype-index p2))))))

(defmethod ntype-union
    ((ntype1 eql-ntype)
     (ntype2 eql-ntype))
  (if (eql (eql-ntype-object ntype1)
           (eql-ntype-object ntype2))
      (values ntype1 t)
      (call-next-method)))

;;; TODO handle array ntypes.
