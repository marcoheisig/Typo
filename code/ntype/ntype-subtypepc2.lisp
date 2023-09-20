(in-package #:typo.ntype)

(defmethod ntype-subtypepc2
    ((ntype1 ntype)
     (ntype2 ntype))
  (values nil nil))

(defmethod ntype-subtypepc2
    ((ntype1 ntype)
     (ntype2 primitive-ntype))
  (multiple-value-bind (subtypepc2 precise-p)
      (primitive-ntype-subtypepc2 (ntype-primitive-ntype ntype1) ntype2)
    (if (not precise-p)
        (values nil nil)
        (values subtypepc2 t))))

(let ((v1-cache (make-array (list +primitive-ntype-limit+ +primitive-ntype-limit+)
                            :element-type 'bit))
      (v2-cache (make-array (list +primitive-ntype-limit+ +primitive-ntype-limit+)
                            :element-type 'bit)))
  (loop for p1 across *primitive-ntypes* do
    (loop for p2 across *primitive-ntypes* do
      (multiple-value-bind (v1 v2)
          (subtypep
           (primitive-ntype-type-specifier p1)
           `(not ,(primitive-ntype-type-specifier p2)))
        (setf (aref v1-cache (ntype-index p1) (ntype-index p2))
              (if v1 1 0))
        (setf (aref v2-cache (ntype-index p1) (ntype-index p2))
              (if v2 1 0)))))
  (defun primitive-ntype-subtypepc2 (p1 p2)
    (declare (primitive-ntype p1 p2))
    (values
     (plusp (aref v1-cache (ntype-index p1) (ntype-index p2)))
     (plusp (aref v2-cache (ntype-index p1) (ntype-index p2))))))

;;; Subtyping with EQL Ntypes

(defmethod ntype-subtypepc2
    ((ntype1 eql-ntype)
     (ntype2 eql-ntype))
  (values
   (not (eql (eql-ntype-object ntype1)
             (eql-ntype-object ntype2)))
   t))

(defmethod ntype-subtypepc2
    ((ntype1 eql-ntype)
     (ntype2 ntype))
  (if (typep (eql-ntype-object ntype1)
             (ntype-type-specifier ntype2))
      (values nil t)
      (values t t)))

(defmethod ntype-subtypepc2
    ((ntype1 ntype)
     (ntype2 eql-ntype))
  (if (typep (eql-ntype-object ntype2)
             (ntype-type-specifier ntype1))
      (values nil t)
      (values t t)))
