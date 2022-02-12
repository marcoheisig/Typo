(in-package #:typo)

(defparameter *built-in-classes*
  (let ((built-in-classes '())
        (table (make-hash-table)))
    (labels ((scan-class (class)
               (alexandria:ensure-gethash
                class
                table
                (progn
                  (when (typep class 'built-in-class)
                    (push class built-in-classes))
                  (mapc #'scan-class (class-direct-subclasses class))))))
      (mapc #'scan-class (class-direct-subclasses (find-class 't))))
    built-in-classes))

(defun relevant-built-in-class-p (class)
  (and
   ;; Relevant classes must have a class name that is exported from its
   ;; home package, or the CL package.
   (symbolp (class-name class))
   (let* ((symbol (class-name class))
          (name (symbol-name symbol))
          (package (symbol-package symbol)))
     (or (eq package (find-package "CL"))
         (eq (nth-value 1 (find-symbol name package))
             :external)))
   ;; Array classes are handled elsewhere.
   (not (subtypep class 'array))))

(defparameter *relevant-built-in-classes*
  (remove-if-not #'relevant-built-in-class-p *built-in-classes*))

(defun built-in-class-bits (built-in-class)
  (cond ((subtypep built-in-class 'nil)
         0)
        ((subtypep built-in-class 'cons)
         (* 2 +word-bits+))
        ((subtypep built-in-class 'fixnum)
         +fixnum-bits+)
        ((subtypep built-in-class 'short-float)
         +short-float-bits+)
        ((subtypep built-in-class 'single-float)
         +single-float-bits+)
        ((subtypep built-in-class 'double-float)
         +double-float-bits+)
        ((subtypep built-in-class 'long-float)
         +long-float-bits+)
        ((subtypep built-in-class 'character)
         (integer-length (1- char-code-limit)))
        #+sbcl
        ((subtypep built-in-class 'sb-ext:simd-pack)
         128)
        #+sbcl
        ((subtypep built-in-class 'sb-ext:simd-pack-256)
         256)
        (t +word-bits+)))

;;; A list whose entry are of the form (type-specifier bits).
(defparameter *primitive-ntype-information*
  (sort
   (remove-duplicates
    (append
     ;; Fundamental types.
     `((t ,+word-bits+)
       (nil 0)
       (null ,+word-bits+)
       (list ,+word-bits+))
     ;; Number types.
     `((short-float ,+short-float-bits+)
       (single-float ,+single-float-bits+)
       (double-float ,+double-float-bits+)
       (long-float ,+long-float-bits+)
       ((complex short-float) ,(* 2 +short-float-bits+))
       ((complex single-float) ,(* 2 +single-float-bits+))
       ((complex double-float) ,(* 2 +double-float-bits+))
       ((complex long-float) ,(* 2 +long-float-bits+))
       (fixnum ,+fixnum-bits+)
       ((signed-byte 8) 8)
       ((signed-byte 16) 16)
       ((signed-byte 32) 32)
       ((signed-byte 64) 64)
       (bit 1)
       ((unsigned-byte 1) 1)
       ((unsigned-byte 2) 8)
       ((unsigned-byte 4) 8)
       ((unsigned-byte 8) 8)
       ((unsigned-byte 16) 16)
       ((unsigned-byte 32) 32)
       ((unsigned-byte 64) 64))
     ;; Array types.
     `((vector 64)
       (simple-vector 64)
       (bit-vector 64)
       (simple-bit-vector 64)
       (string 64)
       (simple-string 64)
       (array 64))
     ;; Relevant built-in classes.
     (loop for built-in-class in *relevant-built-in-classes*
           collect (list (class-name built-in-class)
                         (built-in-class-bits built-in-class)))
     ;; Truth.
     `(((not null) ,+word-bits+)))
    :key #'first
    :test #'alexandria:type=
    :from-end t)
   #'subtypep
   :key #'first))

(deftype ntype-index ()
  `(integer 0 ,(length *primitive-ntype-information*)))
