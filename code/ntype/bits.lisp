(in-package #:typo.ntype)

(defconstant +word-bits+
  #+32-bit 32
  #+64-bit 64
  #-(or 32-bit 64-bit) 64)

(defconstant +fixnum-bits+
  (max (integer-length most-positive-fixnum)
       (integer-length most-negative-fixnum)))

;;; A Common Lisp implementation may provide up to four distinct floating
;;; point types - short-float, single-float, double-float and long-float.
;;; This code figures out how many bits an implementation uses to represent
;;; each of these types.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun float-exponent-bits (float)
    (1+ (integer-length (1- (nth-value 1 (decode-float float))))))

  (defun float-bits (float)
    (+ (float-digits float)
       (float-exponent-bits float))))

(defconstant +short-float-bits+ (float-bits most-positive-short-float))

(defconstant +single-float-bits+ (float-bits most-positive-single-float))

(defconstant +double-float-bits+ (float-bits most-positive-double-float))

(defconstant +long-float-bits+ (float-bits most-positive-long-float))

(defconstant +character-bits+ (integer-length (1- char-code-limit)))

(defconstant base-char-code-limit
  (if (alexandria:type= 'base-char 'character)
      char-code-limit
      (loop for code from 0 below char-code-limit do
        (etypecase (code-char code)
          (base-char)
          (null)
          (character (return code))))))

(defconstant +base-char-bits+
  (if (subtypep 'character 'base-char)
      +character-bits+
      (integer-length (1- base-char-code-limit))))
