(in-package #:typo.ntype)

;;; A list whose entry are of the form (type-specifier bits).
(defparameter *primitive-ntype-information*
  (remove-duplicates
   (append
    `((nil 0)
      (single-float ,+single-float-bits+)
      (double-float ,+double-float-bits+)
      (short-float ,+short-float-bits+)
      (long-float ,+long-float-bits+)
      (float ,(max +short-float-bits+ +single-float-bits+ +double-float-bits+ +long-float-bits+))
      (bit 1)
      ((unsigned-byte 2) 8)
      ((unsigned-byte 4) 8)
      ((signed-byte 8) 8)
      ((unsigned-byte 8) 8)
      ((signed-byte 16) 16)
      ((unsigned-byte 16) 16)
      ((signed-byte 32) 32)
      ((unsigned-byte 32) 32)
      (fixnum ,+fixnum-bits+)
      ((signed-byte 64) 64)
      ((unsigned-byte 64) 64)
      (integer ,+word-bits+)
      (ratio ,+word-bits+)
      (rational ,+word-bits+)
      (real ,+word-bits+)
      ((complex single-float) ,(* 2 +single-float-bits+))
      ((complex double-float) ,(* 2 +double-float-bits+))
      ((complex short-float) ,(* 2 +short-float-bits+))
      ((complex long-float) ,(* 2 +long-float-bits+))
      (complex ,(* 2 +word-bits+))
      (number ,+word-bits+)
      (array ,+word-bits+)
      (function ,+word-bits+)
      (non-nil-symbol ,+word-bits+)
      (null ,+word-bits+)
      (symbol ,+word-bits+)
      (list ,+word-bits+)
      (character ,(integer-length (1- char-code-limit)))
      (logical-pathname ,+word-bits+)
      (pathname ,+word-bits+)
      (class ,+word-bits+)
      ((not null) ,+word-bits+)
      (t ,+word-bits+)))
   :key #'first
   :test #'alexandria:type=
   :from-end t))

;;; The primitive ntype information list should be ordered such that for
;;; any two types A and B, A can only be a subtype of B if it occurs
;;; earlier in the list.
(loop for ((A . nil) . rest) on *primitive-ntype-information* do
  (loop for (B . nil) in rest do
    (assert (not (subtypep B A)))))

(deftype ntype-index ()
  `(integer 0 ,(length *primitive-ntype-information*)))
