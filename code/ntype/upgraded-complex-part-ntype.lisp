(in-package #:typo)

(defmethod upgraded-complex-part-ntype ((primitive-ntype primitive-ntype))
  (primitive-ntype-upgraded-complex-part-ntype primitive-ntype))

(defmethod upgraded-complex-part-ntype ((ntype ntype))
  (primitive-ntype-upgraded-complex-part-ntype
   (ntype-primitive-ntype ntype)))

(let ((cache (make-array (list +primitive-ntype-limit+)
                         :element-type '(cons ntype (cons boolean null))
                         :initial-element (list (universal-ntype) nil))))
  (loop for p across *primitive-ntypes* do
    (when (subtypep (primitive-ntype-type-specifier p) 'real)
      (setf (aref cache (ntype-index p))
            (multiple-value-list
             (find-primitive-ntype
              (upgraded-complex-part-type
               (primitive-ntype-type-specifier p)))))))
  (defun primitive-ntype-upgraded-complex-part-ntype (primitive-ntype)
    (declare (primitive-ntype primitive-ntype))
    (values-list
     (aref cache (ntype-index primitive-ntype)))))
