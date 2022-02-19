(in-package #:typo.common-lisp)

(defconstant short-float-e (exp 1S0))
(defconstant single-float-e (exp 1F0))
(defconstant double-float-e (exp 1D0))
(defconstant long-float-e (exp 1L0))
(defconstant short-float-pi (float pi 1S0))
(defconstant single-float-pi (float pi 1F0))
(defconstant double-float-pi (float pi 1D0))
(defconstant long-float-pi (float pi 1L0))

(declaim (inline coerce-to-short-float))
(defun coerce-to-short-float (number)
  (coerce number 'short-float))

(declaim (inline coerce-to-single-float))
(defun coerce-to-single-float (number)
  (coerce number 'single-float))

(declaim (inline coerce-to-double-float))
(defun coerce-to-double-float (number)
  (coerce number 'double-float))

(declaim (inline coerce-to-long-float))
(defun coerce-to-long-float (number)
  (coerce number 'long-float))

(declaim (inline coerce-to-complex-short-float))
(defun coerce-to-complex-short-float (number)
  (coerce number 'complex-short-float))

(declaim (inline coerce-to-complex-single-float))
(defun coerce-to-complex-single-float (number)
  (coerce number 'complex-single-float))

(declaim (inline coerce-to-complex-double-float))
(defun coerce-to-complex-double-float (number)
  (coerce number 'complex-double-float))

(declaim (inline coerce-to-complex-long-float))
(defun coerce-to-complex-long-float (number)
  (coerce number 'complex-long-float))

(declaim (inline two-arg-+))
(defun two-arg-+ (a b)
  (+ a b))

(declaim (inline two-arg--))
(defun two-arg-- (a b)
  (- a b))

(declaim (inline two-arg-*))
(defun two-arg-* (a b)
  (* a b))

(declaim (inline two-arg-/))
(defun two-arg-/ (a b)
  (/ a b))

(declaim (inline ln))
(defun ln (number)
  (log number))

(defun argmax (real &rest more-reals)
  (labels ((argmax-aux (max max-index index reals)
             (declare (real max) ((and fixnum unsigned-byte) max-index index))
             (cond ((null reals)
                    max-index)
                   ((>= max (first reals))
                    (argmax-aux max max-index (1+ index) (rest reals)))
                   (t
                    (argmax-aux (first reals) (1+ index) (1+ index) (rest reals))))))
    (check-type real real)
    (argmax-aux real 0 0 more-reals)))

(defun argmin (real &rest more-reals)
  (labels ((argmin-aux (min min-index index reals)
             (declare (real min) ((and fixnum unsigned-byte) min-index index))
             (cond ((null reals)
                    min-index)
                   ((< min (first reals))
                    (argmin-aux min min-index (1+ index) (rest reals)))
                   (t
                    (argmin-aux (first reals) (1+ index) (1+ index) (rest reals))))))
    (check-type real real)
    (argmin-aux real 0 0 more-reals)))

(declaim (inline choose))
(defun choose (boolean a b)
  (the (values generalized-boolean &optional)
       (if boolean
           a
           b)))

(declaim (inline prog2-fn))
(defun prog2-fn (a b)
  (prog2 a b))

(declaim (inline and-fn))
(defun and-fn (&rest args)
  (loop for arg in args always arg))

(declaim (inline or-fn))
(defun or-fn (&rest args)
  (loop for arg in args thereis arg))
