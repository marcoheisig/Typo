(in-package #:typo.fndb)

(defun differentiate
    (function wrappers index
     &key
       wrap-constant wrap-function wrapper-nth-value-ntype
       (wrapper-ntype (lambda (w) (funcall wrapper-nth-value-ntype 0 w))))
  (check-type index argument-index)
  (let ((*wrap-constant* wrap-constant)
        (*wrap-function* wrap-function)
        (*wrapper-nth-value-ntype* wrapper-ntype)
        (*wrapper-ntype* wrapper-ntype)
        (fndb-record (find-fndb-record function nil)))
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
    (if (not fndb-record)
        (error "Don't know how to differentiate ~S" function)
        (apply (fndb-record-differentiator fndb-record) index wrappers))))

(defun diff (function arguments n)
  (flet ((wrap-constant (object)
           (list object (list (ntype-of object)) '() nil))
         (wrap-function (function arguments required optional rest)
           (list (list* function (mapcar #'first arguments)) required optional rest))
         (wrapper-nth-value-ntype (index wrapper)
           (trivia:ematch wrapper
             ((list _ required optional rest)
              (let ((n-required (length required)))
                (if (< index n-required)
                    (nth index required)
                    (let ((n-optional (length optional)))
                      (if (< index (+ n-required n-optional))
                          (nth (- index n-required) optional)
                          (if (null rest)
                              (type-specifier-ntype 'null)
                              rest)))))))))
    (destructuring-bind (expression required optional rest)
        (differentiate
         function
         (loop for (expr . ntype) in arguments
               collect
               (list expr (list ntype) '() nil))
         n
         :wrap-constant #'wrap-constant
         :wrap-function #'wrap-function
         :wrapper-nth-value-ntype #'wrapper-nth-value-ntype)
      (values expression required optional rest))))
