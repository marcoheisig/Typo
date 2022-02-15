(in-package #:typo.ntype)

;;; Subtyping with Primitive Ntypes

(defmethod ntype-subtypep
    ((ntype1 ntype)
     (ntype2 primitive-ntype))
  (primitive-ntype-subtypep (ntype-primitive-ntype ntype1) ntype2))

(let ((cache (make-array (list +primitive-ntype-limit+ +primitive-ntype-limit+)
                         :element-type 'bit)))
  (loop for p1 across *primitive-ntypes* do
    (loop for p2 across *primitive-ntypes* do
      (setf (aref cache (ntype-index p1) (ntype-index p2))
            (if (subtypep
                 (primitive-ntype-type-specifier p1)
                 (primitive-ntype-type-specifier p2))
                1 0))))
  (defun primitive-ntype-subtypep (p1 p2)
    (declare (primitive-ntype p1 p2))
    (plusp (aref cache (ntype-index p1) (ntype-index p2)))))

;;; Subtyping with EQL Ntypes

(defmethod ntype-subtypep
    ((ntype1 eql-ntype)
     (ntype2 eql-ntype))
  (eql (eql-ntype-object ntype1)
       (eql-ntype-object ntype2)))

(defmethod ntype-subtypep
    ((ntype1 eql-ntype)
     (ntype2 ntype))
  (typep (eql-ntype-object ntype1)
         (ntype-type-specifier ntype2)))

(defmethod ntype-subtypep
    ((ntype1 ntype)
     (ntype2 eql-ntype))
  ;; The only case where a non-EQL ntype can denote a subtype of an EQL is
  ;; that of the class NULL.
  (and (eql ntype1 (type-specifier-ntype 'null))
       (null (eql-ntype-object ntype2))))

;;; Subtyping with Array Ntypes

;;; TODO
