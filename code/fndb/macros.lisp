(in-package #:typo.fndb)

(defmacro define-fndb-record (function-name lambda-list &body options)
  `(ensure-fndb-record
    ',function-name ',lambda-list
    ,@(loop for option in options
            append
            (trivia:ematch option
              ((list :parent parent)
               (list :parent `(find-fndb-record ',parent)))
              ((list :pure purep)
               (list :purep `(the boolean ,purep)))
              ((list* :differentiator index body)
               (list :differentiator `(define-differentiator ,function-name ,lambda-list ,index ,@body)))
              ((list* :specializer body)
               (list :specializer `(define-specializer ,function-name ,lambda-list ,@body)))))))

(defun block-name (function-name)
  (trivia:ematch function-name
    ((list 'setf name) name)
    ((type function-name) function-name)))

(defmacro define-specializer (function-name lambda-list &body body)
  (multiple-value-bind (required optional rest keyword)
      (alexandria:parse-ordinary-lambda-list lambda-list)
    (unless (null keyword)
      "Specializer lambda lists must not contain keywords.~@
       The specializer for ~S violates this constraint."
      function-name)
    (multiple-value-bind (remaining-forms declarations)
        (alexandria:parse-body body)
      `(lambda ,lambda-list
         ,@declarations
         (flet ((abort-specialization ()
                  (%abort-specialization
                   ',function-name
                   (list* ,@required ,@(mapcar #'first optional) ,rest)))
                (wrap-default (&rest ntypes)
                  (wrap-function
                   ntypes
                   ',function-name
                   (list* ,@required ,@(mapcar #'first optional) ,rest))))
           (declare (ignorable #'abort-specialization #'wrap-default))
           (block ,(block-name function-name) ,@remaining-forms))))))

(defmacro check-ntype (object ntype)
  `(ntype-subtypecase (wrapper-ntype ,object)
     ((not ,ntype) (abort-specialization))
     (t (values))))

(declaim (notinline %abort-specialization))
(defun %abort-specialization (function arguments)
  (error 'invalid-arguments
         :function function
         :argument-types
         (mapcar (alexandria:compose #'ntype-type-specifier #'wrapper-ntype) arguments)))

(defmacro define-differentiator (function-name lambda-list index &body body)
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

(defmacro wrap (form)
  (expand-wrap form))

(defun expand-wrap (form)
  (cond ((consp form)
         `(funcall
           (fndb-record-specializer
            (find-fndb-record ',(first form) nil))
           ,@(mapcar #'expand-wrap (rest form))))
        ((member form '(nil t))
         `(wrap-constant ,form))
        ((symbolp form)
         form)
        (t `(wrap-constant ,form))))

(defmacro define-instruction ((parent-name instruction-name)
                              result-types arguments
                              &body body)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (declaim (inline ,instruction-name))
       (defun ,instruction-name ,arguments
         (the (values ,@result-types &optional)
              (,parent-name ,@arguments))))
     (define-fndb-record ,instruction-name ,arguments
       (:pure t)
       (:parent ,parent-name)
       (:specializer ,@body))))

(defmacro function-specializer (function)
  `(fndb-record-specializer
    (find-fndb-record ,function nil)))

(defmacro define-simple-instruction ((parent-name instruction-name)
                                     result-types argument-types)
  (let ((arguments (mapcar #'gensymify argument-types)))
    `(define-instruction (,parent-name ,instruction-name) ,result-types ,arguments
       (wrap-default
        ,@(loop for type in result-types
                collect
                `(type-specifier-ntype ',type))))))

(defun gensymify (x)
  (if (symbolp x)
      (gensym (symbol-name x))
      (gensym)))
