(in-package #:typo.test-suite)

(defun eval-and-test (expr)
  "Evaluate EXPRESSION, while checking that all values returned by
subexpressions match those predicted by Typo."
  (handler-case (eval-and-test-aux expr '())
    ;; We usually test numeric operations with all available numbers, with
    ;; no regard for floating-point overflows and the like.  Instead, we
    ;; catch these problems here.
    (arithmetic-error () (values))))

(defun lax-typep (object type)
  "Almost like TYPEP, but don't distinguish positive and negative floating
  point zero."
  (trivia:match type
    ((list 'eql thing)
     (if (numberp thing)
         (= object thing)
         (eql object thing)))
    (_
     (typep object type))))

(defun eval-and-test-aux (expr env)
  (if (atom expr)
      (if (symbolp expr)
          (let ((entry (assoc expr env)))
            (if (not entry)
                (error "Unbound variable: ~S" expr)))
          expr)
      (case (first expr)
        (quote (second expr))
        (otherwise
         (let* ((first (first expr))
                (rest (rest expr))
                (args (loop for arg in rest collect (eval-and-test-aux arg env)))
                (values (multiple-value-list (apply first args)))
                (n-values (length values)))
           (apply
            #'alexandria:map-product
            ;; Check that each value matches its predicted type.
            (lambda (&rest ntypes)
              (multiple-value-bind (required optional rest)
                  (infer-ntypes first ntypes)
                (let ((n-required (length required))
                      (n-optional (length optional))
                      (restp (not (null rest))))
                  (is (<= n-required n-values))
                  (when (not restp)
                    (is (<= n-values (+ n-required n-optional))))
                  (loop for index from 0 below n-required do
                    (let ((value (nth index values)))
                      (unless (and (numberp value) (zerop value))
                        (is (lax-typep (nth index values)
                                       (ntype-type-specifier (nth index required)))))))
                  (loop for index from 0 below n-optional do
                    (is (lax-typep (nth (+ n-required index) values)
                                   (ntype-type-specifier (nth index optional)))))
                  (when restp
                    (loop for value in (nthcdr (+ n-required n-optional) values) do
                      (is (lax-typep value (ntype-type-specifier rest))))))))
            (loop for ntype in (mapcar #'ntype-of args)
                  collect
                  (if (eql-ntype-p ntype)
                      (list ntype (typo.ntype::ntype-primitive-ntype ntype))
                      (list ntype))))
           (values-list values))))))

(define-test apply-test
  (eval-and-test '(apply '+ 7 8 '(9 10 11)))
  (eval-and-test '(apply 'coerce 7 '(double-float))))

(define-test cast-test
  (loop for number-type in '(short-float single-float double-float long-float integer rational) do
    (loop for integer in *test-integers* do
      (eval-and-test `(coerce ,integer ',number-type))
      (eval-and-test `(coerce ,integer '(complex ,number-type))))))

(define-test numeric-tests
  (loop for number in *test-numbers* do
    (eval-and-test `(abs ,number))
    (eval-and-test `(signum ,number)))
  (loop for fn in '(= /= + - * /) do
    (loop for number-1 in *test-numbers* do
      (eval-and-test `(,fn ,number-1))
      (loop for number-2 in *test-numbers* do
        (eval-and-test `(,fn ,number-1 ,number-2))
        (eval-and-test `(,fn ,number-1 ,number-2 ,number-1)))))
  (loop for fn in '(< > <= >= min max) do
    (loop for number-1 in *test-reals* do
      (eval-and-test `(,fn ,number-1))
      (loop for number-2 in *test-reals* do
        (eval-and-test `(,fn ,number-1 ,number-2))
        (eval-and-test `(,fn ,number-1 ,number-2 ,number-1)))))
  ;; Rounding.
  (loop for fn in '(floor ceiling truncate round ffloor fceiling ftruncate fround) do
    (loop for number-1 in *test-reals* do
      (eval-and-test `(,fn ,number-1))
      (loop for number-2 in *test-reals* do
        (eval-and-test `(,fn ,number-1 ,number-2)))))
  ;; Trigonometric functions.
  (loop for fn in '(sin cos tan asin acos atan log exp sqrt) do
    (loop for number-1 in *test-numbers* do
      (eval-and-test `(,fn ,number-1)))))
