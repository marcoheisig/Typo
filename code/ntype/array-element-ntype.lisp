(in-package #:typo.ntype)

(defmacro define-array-element-ntype-methods ()
  (let ((array-classes (class-subclasses (find-class 'array))))
    `(progn
       ,@(loop for array-class in array-classes
               for class-name = (class-name array-class)
               collect
               `(defmethod array-element-ntype ((array ,class-name))
                  (typecase array
                    ,@(loop for primitive-ntype in *upgraded-array-element-primitive-ntypes*
                            for element-type = (primitive-ntype-type-specifier primitive-ntype)
                            unless (subtypep `(and (array ,element-type) ,class-name) nil)
                              collect
                            `((array ,element-type)
                              (values ,primitive-ntype t)))
                    (otherwise (values (universal-ntype) nil))))))))

(define-array-element-ntype-methods)
