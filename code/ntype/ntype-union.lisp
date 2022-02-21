(in-package #:typo.ntype)

(defmethod ntype-union
    ((ntype1 ntype)
     (ntype2 ntype))
  (multiple-value-bind (p1 p1-precise-p)
      (ntype-primitive-ntype ntype1)
    (multiple-value-bind (p2 p2-precise-p)
        (ntype-primitive-ntype ntype2)
      (multiple-value-bind (union union-precise-p)
          (primitive-ntype-union p1 p2)
        (values union (and p1-precise-p p2-precise-p union-precise-p))))))

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

;;; EQL Ntype Union

(defmethod ntype-union
    ((ntype1 eql-ntype)
     (ntype2 eql-ntype))
  (if (eql (eql-ntype-object ntype1)
           (eql-ntype-object ntype2))
      (values ntype1 t)
      (call-next-method)))

(defmethod ntype-union
    ((ntype1 (eql (find-primitive-ntype 'null)))
     (ntype2 (eql (make-eql-ntype nil))))
  (values ntype2 t))

(defmethod ntype-union
    ((ntype1 (eql (make-eql-ntype nil)))
     (ntype2 (eql (find-primitive-ntype 'null))))
  (values ntype1 t))

;;; Array Ntype Union

(defmethod ntype-union
    ((ntype1 array-ntype)
     (ntype2 (eql (find-primitive-ntype 'array))))
  (values (make-array-ntype) nil))

(defmethod ntype-union
    ((ntype1 (eql (find-primitive-ntype 'array)))
     (ntype2 array-ntype))
  (values (make-array-ntype) nil))

(defmethod ntype-union
    ((ntype1 (eql (make-array-ntype)))
     (ntype2 (eql (find-primitive-ntype 'array))))
  (values ntype1 t))

(defmethod ntype-union
    ((ntype1 (eql (find-primitive-ntype 'array)))
     (ntype2 (eql (make-array-ntype))))
  (values ntype2 t))

(defmethod ntype-union
    ((ntype1 array-ntype)
     (ntype2 array-ntype))
  (multiple-value-bind (element-ntype element-ntype-precise-p)
      (array-element-ntype-union
       (array-ntype-element-ntype ntype1)
       (array-ntype-element-ntype ntype2))
    (multiple-value-bind (dimensions dimensions-precise-p)
        (array-dimensions-union
         (array-ntype-dimensions ntype1)
         (array-ntype-dimensions ntype2))
      (multiple-value-bind (simplep simplep-precise-p)
          (array-simplep-union
           (array-ntype-simplep ntype1)
           (array-ntype-simplep ntype2))
        (multiple-value-bind (result result-precise-p)
            (make-array-ntype
             :element-type (ntype-type-specifier element-ntype)
             :dimensions dimensions
             :simplep simplep)
          (values
           result
           (and element-ntype-precise-p
                dimensions-precise-p
                simplep-precise-p
                result-precise-p)))))))

(defun array-element-ntype-union (e1 e2)
  (if (eq e1 e2)
      (values e1 t)
      (values '* nil)))

(defun array-dimensions-union (d1 d2)
  (cond ((equal d1 d2)
         (values d1 t))
        ((and (listp d1)
              (listp d2))
         (values
          (loop for a in d1
                for b in d2
                collect
                (if (eql a b) a '*))
          nil))
        (t (values '* nil))))

(defun array-simplep-union (b1 b2)
  (if (eq b1 b2)
      (values b1 t)
      (values nil nil)))
