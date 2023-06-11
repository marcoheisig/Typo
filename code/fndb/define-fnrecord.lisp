(in-package #:typo.fndb)

(defmacro define-fnrecord (function-name lambda-list &body options)
  `(update-fnrecord
    ',function-name
    :lambda-list ',lambda-list
    ,@(loop for processed = '()
            for option in options
            append
            (trivia:ematch option
              ((list* keyword)
               (if (member keyword processed)
                   (error "Duplicate fnrecord option: ~S"
                          keyword)
                   (push keyword processed))
               (trivia.fail:fail))
              ((list* :properties properties)
               (list :properties `',properties))
              ((list :parent parent)
               (list :parent `',parent))
              ((list* :differentiator index body)
               (list
                :differentiator
                `(differentiator-lambda ,function-name ,lambda-list ,index ,@body)))
              ((list* :specializer body)
               (list
                :specializer
                `(specializer-lambda ,function-name ,lambda-list ,@body)))))))
