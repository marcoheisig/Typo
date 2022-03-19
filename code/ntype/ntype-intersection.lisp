(in-package #:typo.ntype)

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

;;; EQL Ntype Intersection

(defmethod ntype-intersection
    ((ntype1 eql-ntype)
     (ntype2 eql-ntype))
  (if (eql (eql-ntype-object ntype1)
           (eql-ntype-object ntype2))
      (values ntype1 t)
      (values (empty-ntype) t)))

(defmethod ntype-intersection
    ((ntype1 (eql (find-primitive-ntype 'null)))
     (ntype2 (eql (make-eql-ntype nil))))
  (values ntype2 t))

(defmethod ntype-intersection
    ((ntype1 (eql (make-eql-ntype nil)))
     (ntype2 (eql (find-primitive-ntype 'null))))
  (values ntype1 t))

(defmethod ntype-intersection
    ((ntype1 eql-ntype)
     (ntype2 ntype))
  (if (ntype-subtypep ntype1 ntype2)
      (values ntype1 t)
      (values (empty-ntype) t)))

(defmethod ntype-intersection
    ((ntype1 ntype)
     (ntype2 eql-ntype))
  (if (ntype-subtypep ntype2 ntype1)
      (values ntype2 t)
      (values (empty-ntype) t)))

;;; Array Ntype Intersection

(defmethod ntype-intersection
    ((ntype1 array-ntype)
     (ntype2 (eql (find-primitive-ntype 'array))))
  (values ntype1 t))

(defmethod ntype-intersection
    ((ntype1 array-ntype)
     (ntype2 (eql (find-primitive-ntype 't))))
  (values ntype1 t))

(defmethod ntype-intersection
    ((ntype1 (eql (find-primitive-ntype 'array)))
     (ntype2 array-ntype))
  (values ntype2 t))

(defmethod ntype-intersection
    ((ntype1 (eql (find-primitive-ntype 't)))
     (ntype2 array-ntype))
  (values ntype2 t))

(defmethod ntype-intersection
    ((ntype1 array-ntype)
     (ntype2 array-ntype))
  (flet ((fail ()
           (return-from ntype-intersection
             (values (empty-ntype) t))))
    (let* ((e1 (array-ntype-element-ntype ntype1))
           (e2 (array-ntype-element-ntype ntype2))
           (element-type
             (cond ((eq e1 e2) e1)
                   ((eq e1 '*) e2)
                   ((eq e2 '*) e1)
                   (t (fail))))
           (d1 (array-ntype-dimensions ntype1))
           (d2 (array-ntype-dimensions ntype2))
           (dimensions
             (etypecase d1
               ((eql *) d2)
               (integer
                (etypecase d2
                  ((eql *) d1)
                  (integer (if (= d1 d2) d1 (fail)))
                  (list (if (= d1 (length d2)) d2 (fail)))))
               (list
                (etypecase d2
                  ((eql *) d1)
                  (integer (if (= (length d1) d2) d1 (fail)))
                  (list
                   (if (/= (length d1) (length d2))
                       (fail)
                       (loop for a in d1
                             for b in d2
                             collect (if (eq a '*) b a))))))))
           (simplep (or (array-ntype-simplep ntype1)
                        (array-ntype-simplep ntype2))))
      (make-array-ntype
       :element-type element-type
       :dimensions dimensions
       :simplep simplep))))
