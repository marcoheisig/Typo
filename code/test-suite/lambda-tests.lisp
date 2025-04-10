(in-package #:typo.test-suite)

(declaim (inline delta))
(defun delta (i j)
  (if (= i j)
      1
      0))

(declaim (inline square))
(defun square (x)
  (* x x))

(declaim (inline quadruple))
(defun quadruple (x)
  (let ((y (* x x)))
    (* y y)))

(define-test lambda-tests
  (dolist (i *test-integers*)
    (dolist (j *test-integers*)
      (eval-and-test `(delta ,i ,j))))
  (loop for number in *test-numbers* do
    (eval-and-test `(square ,number))
    (eval-and-test `(quadruple ,number))))
