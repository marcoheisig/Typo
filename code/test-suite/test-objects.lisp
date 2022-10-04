(in-package #:typo.test-suite)

(defparameter *test-integers*
  (let ((integers '()))
    (loop for integer in (list -1338 -1337 -19 -3 0 3 19 1337 1338) do
      (push integer integers))
    (loop for bits = 1 then (* bits 2) until (>= bits 64) do
      (push (1+ (+ (expt 2 bits))) integers)
      (push (1- (+ (expt 2 bits))) integers)
      (push (+ (expt 2 bits)) integers)
      (push (- (expt 2 bits)) integers)
      (push (1- (- (expt 2 bits))) integers)
      (push (1+ (- (expt 2 bits))) integers))
    (remove-duplicates integers)))

(defparameter *test-floats*
  (let ((floats '()))
    (push most-positive-long-float floats)
    (push most-positive-double-float floats)
    (push most-positive-single-float floats)
    (push most-positive-short-float floats)
    (push most-negative-short-float floats)
    (push most-negative-single-float floats)
    (push most-negative-double-float floats)
    (push most-negative-long-float floats)
    (loop for base in (list -0.7L0 -0.1L0 -0.0L0 +0.0L0 +0.1L0 +0.7L0) do
      (loop for fp-type in '(short-float single-float double-float long-float) do
        (loop for exponent in '(1 2 3 5) do
          (push (scale-float (coerce base fp-type) exponent) floats))))
    (remove-duplicates floats)))

(defparameter *test-reals*
  (append *test-integers* *test-floats*))

(defparameter *test-complex-numbers*
  (let ((complex-numbers '()))
    (loop for float in *test-floats* do
      (push (complex float float) complex-numbers)
      (push (complex float (- float)) complex-numbers))
    (remove-duplicates complex-numbers)))

(defparameter *test-numbers*
  (append *test-integers* *test-floats* *test-complex-numbers*))

(defparameter *test-arrays*
  (loop for dimensions in '(() (1) (10) (2 2) (2 0 3) (2 3 4) (4 3 2 1))
        append
        (loop for (element-type initial-element)
                in '(((unsigned-byte 1) 1)
                     ((unsigned-byte 2) 1)
                     ((unsigned-byte 4) 1)
                     ((unsigned-byte  8) 42)
                     ((unsigned-byte 16) 42)
                     ((unsigned-byte 32) 42)
                     ((unsigned-byte 64) 42)
                     ((signed-byte  8) -42)
                     ((signed-byte 16) -42)
                     ((signed-byte 32) -42)
                     ((signed-byte 64) -42)
                     (short-float  42s0)
                     (single-float 42f0)
                     (double-float 42d0)
                     (long-float   42l0)
                     (character    #\X))
              append
              (loop for adjustable in '(nil t)
                    collect (make-array dimensions
                                        :element-type element-type
                                        :initial-element initial-element
                                        :adjustable adjustable)))))

(defparameter *test-objects*
  (append *test-numbers* *test-arrays*))
