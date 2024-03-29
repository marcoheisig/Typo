(in-package #:typo.vm)

(defmacro define-type-check (type)
  (check-type type symbol)
  (let ((name (intern (format nil "~@:(the-~A~)" type) #.*package*)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (declaim (inline ,name))
         (defun ,name (object)
           (check-type object ,type)
           object))
       (define-fnrecord ,name (object)
         ;; Of course type checks are only pure in their respective
         ;; domains.  But this is fine for us, since we do not consider
         ;; signaled conditions when it comes to types, only returned
         ;; values.
         (:properties :foldable :movable)
         ,@(when (subtypep type 'number)
             `((:differentiator _ (declare (ignore object)) ,(coerce 1 type))))
         (:specializer
          (ntype-subtypecase (wrapper-ntype object)
            ((not ,type) (abort-specialization))
            (,type object)
            (t (wrap-default (type-specifier-ntype ',type)))))))))

(define-type-check number)
(define-type-check real)
(define-type-check rational)
(define-type-check integer)
(define-type-check float)
(define-type-check short-float)
(define-type-check single-float)
(define-type-check double-float)
(define-type-check long-float)
(define-type-check complex)
(define-type-check complex-short-float)
(define-type-check complex-single-float)
(define-type-check complex-double-float)
(define-type-check complex-long-float)
(define-type-check function)
(define-type-check character)
(define-type-check symbol)
