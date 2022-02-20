(in-package #:typo.fndb)

(defun unbound-special-function (name)
  (lambda (&rest args)
    (declare (ignore args))
    (error "The special function ~S is not bound." name)))

(defmacro define-special-function ((function variable) lambda-list)
  (check-type variable variable-name)
  (check-type function function-name)
  (unless (null (intersection lambda-list lambda-list-keywords))
    (error "Only mandatory arguments are supported."))
  `(progn
     (declaim (function ,variable))
     (defvar ,variable (unbound-special-function ',function))
     (declaim (inline ,function))
     (defun ,function ,lambda-list
       (funcall ,variable ,@lambda-list))))

(define-special-function (wrap-constant *wrap-constant*)
    (constant))

(define-special-function (wrap-function *wrap-function*)
    (function-designator wrappers required-ntypes optional-ntypes rest-ntype))

(define-special-function (wrapper-nth-value-ntype *wrapper-nth-value-ntype*)
  (n wrapper))

(define-special-function (wrapper-ntype *wrapper-ntype*)
  (wrapper))

(defmacro wrap (form)
  (expand-wrap form))

(defun expand-wrap (form)
  (cond ((consp form)
         `(funcall
           (function-specializer ',(first form))
           ,@(mapcar #'expand-wrap (rest form))))
        ((constantp form)
         `(wrap-constant ,form))
        ((symbolp form)
         form)
        (t (error "Don't know how to wrap ~S." form))))

(defmacro assert-wrapper-type (wrapper type)
  `(ntype-subtypecase (wrapper-ntype ,wrapper)
     ((not ,type) (abort-specialization))))
