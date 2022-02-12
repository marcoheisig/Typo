(in-package #:typo)

;;; The rules of numeric contagion are as follows:
;;;
;;; CLHS 12.1.4.1: When rationals and floats are combined by a numerical
;;; function, the rational is first converted to a float of the same
;;; format.
;;;
;;; CLHS 12.1.4.4: The result of a numerical function is a float of the
;;; largest format among all the floating-point arguments to the function.
;;;
;;; CLHS 12.1.5.2: When a real and a complex are both part of a
;;; computation, the real is first converted to a complex by providing an
;;; imaginary part of 0.

(defmethod ntype-contagion ((ntype1 ntype) (ntype2 ntype))
  (primitive-ntype-contagion
   (ntype-primitive-ntype ntype1)
   (ntype-primitive-ntype ntype2)))

(defun ntype-contagion/slow (ntype1 ntype2)
  (declare (ntype ntype1 ntype2))
  (flet ((fail () (return-from ntype-contagion/slow (empty-ntype))))
    (ntype-subtypecase ntype1
      ((not number) (fail))
      (complex
       (ntype-subtypecase ntype2
         ((not number) (fail))
         (complex (complex-contagion ntype1 ntype2))
         (float (complex-contagion ntype1 (complex-ntype-from-real-ntype ntype2)))
         (real (complex-contagion ntype1 (complex-ntype-from-real-ntype ntype2)))
         (t (type-specifier-ntype 'complex))))
      (float
       (ntype-subtypecase ntype2
         ((not number) (fail))
         (complex (complex-contagion (complex-ntype-from-real-ntype ntype1) ntype2))
         (float (float-contagion ntype1 ntype2))
         (real (ntype-primitive-ntype ntype1))
         (t (type-specifier-ntype 'number))))
      (real
       (ntype-subtypecase ntype2
         ((not number) (fail))
         (complex (complex-contagion (complex-ntype-from-real-ntype ntype1) ntype2))
         (float (ntype-primitive-ntype ntype2))
         (real (values (ntype-union ntype1 ntype2)))
         (t (type-specifier-ntype 'number))))
      (t
       (ntype-subtypecase ntype2
         ((not number) (fail))
         (t (type-specifier-ntype 'number)))))))

(defun complex-ntype-from-real-ntype (real-ntype)
  (assert (ntype-subtypep real-ntype (type-specifier-ntype 'real)))
  (find-primitive-ntype `(complex ,(ntype-type-specifier real-ntype))))

(defun float-contagion (ntype1 ntype2)
  (assert (ntype-subtypep ntype1 (type-specifier-ntype 'float)))
  (assert (ntype-subtypep ntype2 (type-specifier-ntype 'float)))
  (macrolet ((body ()
               `(ntype-subtypecase ntype1
                  ,@(loop for fp1 in *float-primitive-ntypes*
                          collect
                          `(,(primitive-ntype-type-specifier fp1)
                            (ntype-subtypecase ntype2
                              ,@(loop for fp2 in *float-primitive-ntypes*
                                      collect
                                      `(,(primitive-ntype-type-specifier fp2)
                                        ,(if (> (primitive-ntype-bits fp1)
                                                (primitive-ntype-bits fp2))
                                             fp1 fp2)))
                              (t (type-specifier-ntype 'float)))))
                  (t (type-specifier-ntype 'float)))))
    (body)))

(defun complex-contagion (ntype1 ntype2)
  (assert (ntype-subtypep ntype1 (type-specifier-ntype 'complex)))
  (assert (ntype-subtypep ntype2 (type-specifier-ntype 'complex)))
  (macrolet ((body ()
               `(ntype-subtypecase ntype1
                  ,@(loop for cp1 in *complex-primitive-ntypes*
                          collect
                          `(,(primitive-ntype-type-specifier cp1)
                            (ntype-subtypecase ntype2
                              ,@(loop for cp2 in *complex-primitive-ntypes*
                                      collect
                                      `(,(primitive-ntype-type-specifier cp2)
                                        ,(if (> (primitive-ntype-bits cp1)
                                                (primitive-ntype-bits cp2))
                                             cp1 cp2)))
                              (t (type-specifier-ntype 'complex)))))
                  (t (type-specifier-ntype 'complex)))))
    (body)))

(let ((cache (make-array (list +primitive-ntype-limit+ +primitive-ntype-limit+)
                         :element-type 'ntype
                         :initial-element (universal-ntype))))
  (loop for p1 across *primitive-ntypes* do
    (loop for p2 across *primitive-ntypes* do
      (setf (aref cache (ntype-index p1) (ntype-index p2))
            (ntype-contagion/slow p1 p2))))
  (defun primitive-ntype-contagion (p1 p2)
    (declare (primitive-ntype p1 p2))
    (aref cache (ntype-index p1) (ntype-index p2))))
