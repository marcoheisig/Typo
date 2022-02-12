(in-package #:typo)

(defmethod upgraded-array-element-ntype ((primitive-ntype primitive-ntype))
  (primitive-ntype-upgraded-array-element-ntype primitive-ntype))

(defmethod upgraded-array-element-ntype ((ntype ntype))
  (primitive-ntype-upgraded-array-element-ntype
   (ntype-primitive-ntype ntype)))

(let ((cache (make-array (list +primitive-ntype-limit+)
                         :element-type '(cons ntype (cons boolean null))
                         :initial-element (list (universal-ntype) nil))))
  (loop for p across *primitive-ntypes* do
    (setf (aref cache (ntype-index p))
          (multiple-value-list
           (find-primitive-ntype
            (upgraded-array-element-type
             (primitive-ntype-type-specifier p))))))
  (defun primitive-ntype-upgraded-array-element-ntype (primitive-ntype)
    (declare (primitive-ntype primitive-ntype))
    (values-list
     (aref cache (ntype-index primitive-ntype)))))
