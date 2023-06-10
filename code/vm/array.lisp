(in-package #:typo.vm)

(define-fnrecord array-dimensions (array)
  (:properties :foldable)
  (:specializer
   (assert-wrapper-type array array)
   (wrap-default (type-specifier-ntype 'list))))

(define-fnrecord array-dimension (array axis-number)
  (:properties :foldable)
  (:specializer
   (assert-wrapper-type array array)
   (assert-wrapper-type axis-number unsigned-byte)
   (wrap-default
    (type-specifier-ntype 'unsigned-byte))))

;;; row—major—aref

(define-fnrecord row-major-aref (array index)
  (:properties :foldable)
  (:specializer
   (assert-wrapper-type index unsigned-byte)
   (ntype-subtypecase (wrapper-ntype array)
     ((not array)
      (abort-specialization))
     ((array short-float)
      (wrap (short-float-row-major-aref array index)))
     ((array single-float)
      (wrap (single-float-row-major-aref array index)))
     ((array double-float)
      (wrap (double-float-row-major-aref array index)))
     ((array long-float)
      (wrap (long-float-row-major-aref array index)))
     (t
      (wrap-default (universal-ntype))))))

(define-simple-instruction (row-major-aref short-float-row-major-aref) (single-float) ((array short-float) unsigned-byte))
(define-simple-instruction (row-major-aref single-float-row-major-aref) (single-float) ((array single-float) unsigned-byte))
(define-simple-instruction (row-major-aref double-float-row-major-aref) (double-float) ((array double-float) unsigned-byte))
(define-simple-instruction (row-major-aref long-float-row-major-aref) (long-float) ((array long-float) unsigned-byte))

(define-fnrecord (setf row-major-aref) (value array index)
  (:specializer
   (assert-wrapper-type index unsigned-byte)
   (ntype-subtypecase (wrapper-ntype array)
     ((not array)
      (abort-specialization))
     ((array short-float)
      (assert-wrapper-type value short-float)
      (wrap (setf (short-float-row-major-aref array index) value)))
     ((array single-float)
      (assert-wrapper-type value single-float)
      (wrap (setf (single-float-row-major-aref array index) value)))
     ((array double-float)
      (assert-wrapper-type value double-float)
      (wrap (setf (double-float-row-major-aref array index) value)))
     ((array long-float)
      (assert-wrapper-type value long-float)
      (wrap (setf (long-float-row-major-aref array index) value)))
     (t
      (wrap-default (wrapper-ntype value))))))

(define-simple-instruction ((setf row-major-aref) (setf short-float-row-major-aref)) (single-float) (short-float (array short-float) unsigned-byte))
(define-simple-instruction ((setf row-major-aref) (setf single-float-row-major-aref)) (single-float) (single-float (array single-float) unsigned-byte))
(define-simple-instruction ((setf row-major-aref) (setf double-float-row-major-aref)) (double-float) (double-float (array double-float) unsigned-byte))
(define-simple-instruction ((setf row-major-aref) (setf long-float-row-major-aref)) (long-float) (long-float (array long-float) unsigned-byte))

;;; aref

(define-fnrecord aref (array &rest indices)
  (:properties :foldable)
  (:specializer
   (let* ((array-ntype (wrapper-ntype array))
          (rank (length indices))
          (rank-ntype (type-specifier-ntype `(array * ,rank))))
     (cond
       ((ntype-subtypepc2 array-ntype (type-specifier-ntype 'array))
        (abort-specialization))
       ((ntype-subtypepc2 array-ntype rank-ntype)
        (error "Invalid number of array subscripts for array of type ~S."
               (ntype-type-specifier array-ntype)))
       ((ntype-subtypep array-ntype rank-ntype)
        (let* ((dimensions
                 (loop for axis below rank
                       for axis-wrapper = (wrap-constant axis)
                       collect (wrap (array-dimension array axis-wrapper))))
               (strides
                 (nreverse
                  (loop for axis from (1- rank) downto 0
                        for stride = (wrap-constant 1)
                          then (wrap (* stride (unwrap (nth (1+ axis) dimensions))))
                        collect stride)))
               (row-major-index
                 (wrap-constant 0)))
          (loop for stride in strides
                for index in indices do
                  (setf row-major-index
                        (wrap (+ row-major-index (* stride index)))))
          (wrap (row-major-aref array row-major-index))))
       (t
        (wrap-default (universal-ntype)))))))

(define-fnrecord (setf aref) (value array &rest indices)
  (:specializer
   (let* ((array-ntype (wrapper-ntype array))
          (rank (length indices))
          (rank-ntype (type-specifier-ntype `(array * ,rank))))
     (cond
       ((ntype-subtypepc2 array-ntype (type-specifier-ntype 'array))
        (abort-specialization))
       ((ntype-subtypepc2 array-ntype rank-ntype)
        (error "Invalid number of array subscripts for array of type ~S."
               (ntype-type-specifier array-ntype)))
       ((ntype-subtypep array-ntype rank-ntype)
        (let* ((dimensions
                 (loop for axis below rank
                       collect (wrap (array-dimension array (unwrap (wrap-constant axis))))))
               (strides
                 (nreverse
                  (loop for axis from (1- rank) downto 0
                        for stride = (wrap-constant 1)
                          then (wrap (* stride (unwrap (elt dimensions (1+ axis)))))
                        collect stride)))
               (row-major-index
                 (wrap-constant 0)))
          (loop for stride in strides
                for index in indices do
                  (setf row-major-index
                        (wrap (+ row-major-index (* stride index)))))
          (wrap (setf (row-major-aref array row-major-index) value))))
       (t
        (wrap-default (wrapper-ntype value)))))))

;;; svref

(define-fnrecord svref (simple-vector index)
  (:properties :foldable)
  (:specializer
   (assert-wrapper-type index unsigned-byte)
   (etypecase (wrapper-ntype simple-vector)
     ((not simple-vector) (abort-specialization))
     (t (wrap-default (universal-ntype))))))

(define-fnrecord (setf svref) (value simple-vector index)
  (:properties :foldable)
  (:specializer
   (assert-wrapper-type index unsigned-byte)
   (etypecase (wrapper-ntype simple-vector)
     ((not simple-vector) (abort-specialization))
     (t (wrap-default (wrapper-ntype value))))))
