(in-package #:typo.fndb)

(defmacro define-fnrecord (function-name lambda-list &body options)
  (let ((purep (second (find :pure options :key #'first))))
    (check-type purep boolean)
    `(ensure-fnrecord
      ',function-name
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
                ((list :pure boolean)
                 (list :purep `',boolean))
                ((list :parent parent)
                 (list :parent `',parent))
                ((list* :differentiator index body)
                 (list
                  :differentiator
                  `(define-differentiator ,function-name ,lambda-list ,index ,@body)))
                ((list* :specializer body)
                 (list
                  :specializer
                  `(define-specializer ,function-name ,lambda-list ,purep ,@body))))))))
