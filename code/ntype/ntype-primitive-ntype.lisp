(in-package #:typo)

(declaim (inline ntype-primitive-ntype))
(defun ntype-primitive-ntype (ntype)
  (declare (ntype ntype))
  (cond ((primitive-ntype-p ntype)
         (values ntype t))
        ((and (eql-ntype-p ntype)
              (null (eql-ntype-object ntype)))
         (find-primitive-ntype 'null))
        (t (values (primitive-ntype-from-index (ntype-index ntype)) nil))))

(defmethod ntype-bits ((ntype ntype))
  (primitive-ntype-bits
   (ntype-primitive-ntype ntype)))
