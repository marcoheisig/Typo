(in-package #:typo.fndb)

(defun differentiate
    (function wrappers index
     &key
       wrap-constant
       wrap-function
       wrapper-ntype
       wrapper-nth-value)
  (check-type index argument-index)
  (let ((*wrap-constant* wrap-constant)
        (*wrap-function* wrap-function)
        (*wrapper-nth-value* wrapper-nth-value)
        (*wrapper-ntype* wrapper-ntype)
        (fnrecord (find-fnrecord function nil)))
    (unless (< -1 index (length wrappers))
      (error 'invalid-differentiation-index
             :function function
             :index index
             :arguments wrappers))
    (ntype-subtypecase (wrapper-ntype (nth index wrappers))
      ((not number)
       (error 'non-numeric-differentiation-argument
              :function function
              :index index
              :arguments wrappers)))
    (if (not fnrecord)
        (error "Don't know how to differentiate ~S" function)
        (apply (fnrecord-differentiator fnrecord) index wrappers))))

(defun diff (function arguments n)
  ;; We represent wrappers as list of the form (expression values-ntype)
  (flet ((wrap-constant (object)
           (list object
                 (make-values-ntype (list (ntype-of object)) '() nil)))
         (wrap-function (function arguments required optional rest)
           (list `(,function ,@(mapcar #'first arguments))
                 (make-values-ntype required optional rest)))
         (wrapper-ntype (wrapper)
           (values-ntype-nth-value-ntype 0 (second wrapper)))
         (wrapper-nth-value (index wrapper)
           (list `(nth-value ,index ,(first wrapper))
                 (make-values-ntype
                  (list (values-ntype-nth-value-ntype index (second wrapper)))
                  '()
                  nil))))
    (destructuring-bind (expression values-ntype)
        (differentiate
         function
         (loop for (expr ntype) in arguments
               collect
               (list expr (make-values-ntype (list ntype) '() nil)))
         n
         :wrap-constant #'wrap-constant
         :wrap-function #'wrap-function
         :wrapper-nth-value #'wrapper-nth-value)
      (values expression values-ntype))))
