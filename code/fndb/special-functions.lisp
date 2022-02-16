(in-package #:typo.fndb)

(defun unbound-special-function (name)
  (lambda (&rest args)
    (declare (ignore args))
    (error "The special function ~S is not bound." name)))

(defmacro define-special-function (function variable)
  (check-type variable variable-name)
  (check-type function function-name)
  `(progn
     (declaim (function ,variable))
     (defvar ,variable (unbound-special-function ',function))
     (defun ,function (&rest args)
       (apply ,variable args))
     (define-compiler-macro ,function (&rest args)
       `(funcall ,',variable ,@args))))

(define-special-function wrap-constant *wrap-constant*)
(define-special-function wrap-function *wrap-function*)
(define-special-function wrapper-nth-value-ntype *wrapper-nth-value-ntype*)
(define-special-function wrapper-ntype *wrapper-ntype*)
