(in-package #:typo.fndb)

(defun block-name (function-name)
  (trivia:ematch function-name
    ((list 'setf name) name)
    ((type function-name) function-name)))

(defmacro specializer-lambda (function-name lambda-list &body body)
  (multiple-value-bind (required optional rest keyword)
      (alexandria:parse-ordinary-lambda-list lambda-list)
    (unless (null keyword)
      "Specializer lambda lists must not contain keywords.~@
       The specializer for ~S violates this constraint."
      function-name)
    (setf optional (loop for (name initform suppliedp) in optional
                         collect `(,name (wrap-constant ,initform) ,(or suppliedp (gensym)))))
    (multiple-value-bind (remaining-forms declarations)
        (alexandria:parse-body body)
      (alexandria:with-gensyms (fnrecord)
        `(let ((,fnrecord (ensure-fnrecord ',function-name)))
           (lambda (,@required
                    ,@(when optional `(&optional ,@optional))
                    ,@(when rest `(&rest ,rest)))
             ,@declarations
             (labels
                 ((list-of-arguments ()
                    ,(cond ((and (not optional) (not rest))
                            `(list ,@required))
                           ((and (not optional))
                            `(list* ,@required ,rest))
                           (t
                            (let ((result (gensym "RESULT")))
                              `(let ((,result ,rest))
                                 ,@(loop for (arg nil suppliedp) in (reverse optional)
                                         collect
                                         `(when ,suppliedp (push ,arg ,result)))
                                 ,@(loop for arg in (reverse required)
                                         collect `(push ,arg ,result))
                                 ,result)))))
                  (abort-specialization ()
                    (%abort-specialization
                     (fnrecord-name ,fnrecord)
                     (list* ,@required ,@(mapcar #'first optional) ,rest)))
                  (wrap-default* (required optional rest)
                    (declare (list required optional) (type (or ntype null) rest))
                    (wrap-function ,fnrecord (list-of-arguments) required optional rest))
                  (wrap-default (&rest ntypes)
                    (wrap-default* ntypes '() nil)))
               (declare (ignorable #'abort-specialization #'wrap-default #'wrap-default*))
               (block ,(block-name function-name)
                 ;; Fold calls to foldable functions with known arguments.
                 (when (and (member :foldable (fnrecord-properties ,fnrecord))
                            ,@(loop for wrapper in required
                                    collect `(eql-ntype-p (wrapper-ntype ,wrapper)))
                            ,@(loop for (wrapper nil nil) in optional
                                    collect `(eql-ntype-p (wrapper-ntype ,wrapper)))
                            ,@(when rest
                                `((loop for arg in ,rest
                                        always (eql-ntype-p (wrapper-ntype arg))))))
                   (return-from ,(block-name function-name)
                     (let ((values
                             (multiple-value-list
                              (apply
                               (fnrecord-function ,fnrecord)
                               (loop for wrapper in (list-of-arguments)
                                     collect (eql-ntype-object (wrapper-ntype wrapper)))))))
                       (if (= 1 (length values))
                           (wrap-constant (first values))
                           (wrap-function
                            (ensure-fnrecord 'values)
                            (mapcar #'wrap-constant values)
                            (mapcar #'ntype-of values) '() nil)))))
                 ,@remaining-forms))))))))

(declaim (notinline %abort-specialization))
(defun %abort-specialization (function arguments)
  (error 'invalid-arguments
         :function function
         :argument-types
         (mapcar (alexandria:compose #'ntype-type-specifier #'wrapper-ntype) arguments)))

(defmacro assert-wrapper-type (wrapper type)
  `(ntype-subtypecase (wrapper-ntype ,wrapper)
     ((not ,type) (abort-specialization))))

(defmacro differentiator-lambda (function-name lambda-list index &body body)
  (multiple-value-bind (required optional rest keyword)
      (alexandria:parse-ordinary-lambda-list lambda-list)
    (unless (null keyword)
      (error "Differentiator lambda lists must not contain keywords. ~
              The differentiator for the function ~S violates this constraint."
             function-name))
    (multiple-value-bind (remaining-forms declarations)
        (alexandria:parse-body body)
      `(lambda ,(list* index lambda-list)
         ,@declarations
         ;; For convenience, add an ignorable declaration to the
         ;; index if it can only be zero.
         ,@(when (and (= 1 (length required))
                      (null optional)
                      (null rest))
             `((declare (ignorable ,index))))
         (block ,(block-name function-name) ,@remaining-forms)))))

(defmacro define-instruction ((parent-name instruction-name)
                              result-types arguments
                              &body body)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (declaim (inline ,instruction-name))
       (defun ,instruction-name ,arguments
         (the (values ,@result-types &optional)
              ,(trivia:match parent-name
                 ((list 'setf (and function-name (type non-nil-symbol)))
                  `(setf (,function-name ,@(rest arguments)) ,(first arguments)))
                 ((and function-name (type non-nil-symbol))
                  `(,function-name ,@arguments))))))
     (define-fnrecord ,instruction-name ,arguments
       (:parent ,parent-name)
       (:specializer ,@body))))

(defmacro define-simple-instruction ((parent-name instruction-name)
                                     result-types argument-types)
  (let ((arguments (mapcar #'gensymify argument-types)))
    `(define-instruction (,parent-name ,instruction-name) ,result-types ,arguments
       ,@(loop for argument in arguments
               for argument-type in argument-types
               collect
               `(ntype-subtypecase (wrapper-ntype ,argument)
                  ((not ,argument-type) (abort-specialization))))
       (wrap-default
        ,@(loop for type in result-types
                collect
                `(type-specifier-ntype ',type))))))

(defun gensymify (x)
  (if (symbolp x)
      (gensym (symbol-name x))
      (gensym)))
