(in-package #:typo.ntype)

(defvar *eql-ntype-table*
  (trivial-garbage:make-weak-hash-table
   :test #'eql
   :weakness :value))

(defmethod make-eql-ntype :around (object)
  (values
   (alexandria:ensure-gethash
    object
    *eql-ntype-table*
    (call-next-method))))

(defmacro define-make-eql-ntype-methods ()
  (let* ((class-ntypes (remove-if-not #'primitive-ntype-class *primitive-ntypes*))
         (non-class-ntypes (remove-if #'primitive-ntype-class *primitive-ntypes*))
         (alist (map 'list #'list class-ntypes)))
    (loop for non-class-ntype across (reverse non-class-ntypes) do
      (let ((entry (find non-class-ntype alist
                         :key #'first
                         :test #'ntype-subtypep)))
        (push non-class-ntype (cdr entry))))
    `(progn
       ,@(loop for (ntype . other-ntypes) in alist
               for class = (class-name (primitive-ntype-class ntype))
               collect
               `(defmethod make-eql-ntype ((object ,class))
                  (typecase object
                    ,@(loop for other-ntype in other-ntypes
                            collect
                            `(,(ntype-type-specifier other-ntype)
                              (%make-eql-ntype
                               :index ,(ntype-index other-ntype)
                               :object object)))
                    (otherwise
                     (%make-eql-ntype
                      :index ,(ntype-index ntype)
                      :object object))))))))

(define-make-eql-ntype-methods)

(defmethod make-load-form ((eql-ntype eql-ntype) &optional env)
  (declare (ignore env))
  `(make-eql-ntype
    ,(eql-ntype-object eql-ntype)))

(defmethod ntype-primitive-ntype
    ((ntype (eql (make-eql-ntype nil))))
  (find-primitive-ntype 'null))
