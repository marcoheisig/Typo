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

;;; The only case where a non-EQL ntype can denote a subtype of an EQL is
;;; that of the class NULL.
(defmethod ntype-subtypep
    ((ntype1 (eql (find-primitive-ntype 'null)))
     (ntype2 (eql (make-eql-ntype nil))))
  t)

;;; Subtyping with Array Ntypes

(defmethod ntype-subtypep
    ((ntype1 array-ntype)
     (ntype2 (eql (find-primitive-ntype 'array))))
  t)

(defmethod ntype-subtypep
    ((ntype1 (eql (find-primitive-ntype 'array)))
     (ntype2 array-ntype))
  (ntype-subtypep (make-array-ntype) ntype2))

(defmethod ntype-subtypep
    ((ntype1 array-ntype)
     (ntype2 array-ntype))
  (and (array-dimensions-subtypep
        (array-ntype-dimensions ntype1)
        (array-ntype-dimensions ntype2))
       (array-element-ntype-subtypep
        (array-ntype-element-ntype ntype1)
        (array-ntype-element-ntype ntype2))
       (array-simplep-subtypep
        (array-ntype-simplep ntype1)
        (array-ntype-simplep ntype2))))

(defun array-dimensions-subtypep (d1 d2)
  (etypecase d2
    ((eql *) t)
    (integer
     (etypecase d1
       ((eql *) nil)
       (integer (= d2 d1))
       (list (= d2 (length d1)))))
    (list
     ;; We rely on the fact that make-array-ntype normalizes lists of all
     ;; asterisks into integers.
     (and (listp d1)
          (= (length d1) (length d2))
          (loop for a in d1
                for b in d2
                always (or (eql a *) (eql a b)))))))

(defun array-element-ntype-subtypep (e1 e2)
  (cond ((eq e1 e2) t)
        ((eq e2 '*) t)
        ((eq e1 '*) nil)
        ((and (ntypep e1) (ntypep e2)) (ntype= e1 e2))
        (t nil)))

(defun array-simplep-subtypep (b1 b2)
  (if b2 b1 t))
