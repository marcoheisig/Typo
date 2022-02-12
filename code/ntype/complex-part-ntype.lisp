(in-package #:typo)

(defmacro define-complex-part-ntype-methods ()
  (let ((complex-classes (class-subclasses (find-class 'complex))))
    `(progn
       ,@(loop for complex-class in complex-classes
               for class-name = (class-name complex-class)
               collect
               `(defmethod complex-part-ntype ((complex ,class-name))
                  (typecase complex
                    ,@(loop for primitive-ntype in *upgraded-complex-part-primitive-ntypes*
                            for element-type = (primitive-ntype-type-specifier primitive-ntype)
                            unless (subtypep `(and (complex ,element-type) ,class-name) nil)
                              collect
                            `((complex ,element-type)
                              (values ,primitive-ntype t)))
                    (otherwise (values (universal-ntype) nil))))))))

(define-complex-part-ntype-methods)

