(in-package #:typo.fndb)

(defmacro define-fndb-record (function-name lambda-list &body options)
  (multiple-value-bind (min-arguments max-arguments)
      (lambda-list-arity lambda-list)
    (let ((purep (second (find :pure options :key #'first))))
      (check-type purep boolean)
      `(reinitialize-instance (ensure-fndb-record ',function-name)
         :function-name ',function-name
         :min-arguments ',min-arguments
         :max-arguments ',max-arguments
         ,@(loop for processed = '()
                 for option in options
                 append
                 (trivia:ematch option
                   ((list :pure boolean)
                    (list :purep `',boolean))
                   ((list* keyword)
                    (if (member keyword processed)
                        (error "Duplicate fndb record option: ~S"
                               keyword)
                        (push keyword processed))
                    (trivia.fail:fail))
                   ((list :parent parent)
                    (list
                     :parent
                     `(find-fndb-record ',parent)))
                   ((list* :differentiator index body)
                    (list
                     :differentiator
                     `(define-differentiator ,function-name ,lambda-list ,index ,@body)))
                   ((list* :specializer body)
                    (list
                     :specializer
                     `(define-specializer ,function-name ,lambda-list ,purep ,@body)))))))))
