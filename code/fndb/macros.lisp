(in-package #:typo.fndb)

(defun block-name (function-name)
  (trivia:ematch function-name
    ((list 'setf name) name)
    ((type function-name) function-name)))

(defmacro define-specializer (function-name lambda-list purep &body body)
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
                   ',function-name
                   (list* ,@required ,@(mapcar #'first optional) ,rest)
                   ntypes
                   '()
                   nil))
                (wrap-default* (required optional rest)
                  (declare (list required optional) (ntype rest))
                  (wrap-function
                   ',function-name
                   (list* ,@required ,@(mapcar #'first optional) ,rest)
                   required
                   optional
                   rest)))
           (declare (ignorable #'abort-specialization #'wrap-default #'wrap-default*))
           (block ,(block-name function-name) ,@remaining-forms))))))

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
