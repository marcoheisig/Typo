(in-package #:typo.vm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Floating-Point Casts

(define-fnrecord coerce-to-short-float (x)
  (:properties :foldable :movable)
  (:differentiator _ (declare (ignore x)) (wrap 1S0))
  (:specializer
   (ntype-subtypecase (wrapper-ntype x)
     (short-float (wrap x))
     (single-float (wrap (short-float-from-single-float x)))
     (double-float (wrap (short-float-from-double-float x)))
     (long-float (wrap (short-float-from-long-float x)))
     (t (wrap-default (type-specifier-ntype 'short-float))))))

(define-simple-instruction (coerce-to-short-float short-float-from-single-float) (short-float) (single-float))
(define-simple-instruction (coerce-to-short-float short-float-from-double-float) (short-float) (double-float))
(define-simple-instruction (coerce-to-short-float short-float-from-long-float) (short-float) (long-float))

(define-fnrecord coerce-to-single-float (x)
  (:properties :foldable :movable)
  (:differentiator _ (declare (ignore x)) (wrap 1F0))
  (:specializer
   (ntype-subtypecase (wrapper-ntype x)
     (single-float (wrap x))
     (short-float (wrap (single-float-from-short-float x)))
     (double-float (wrap (single-float-from-double-float x)))
     (long-float (wrap (single-float-from-long-float x)))
     (t (wrap-default (type-specifier-ntype 'single-float))))))

(define-simple-instruction (coerce-to-single-float single-float-from-short-float) (single-float) (short-float))
(define-simple-instruction (coerce-to-single-float single-float-from-double-float) (single-float) (double-float))
(define-simple-instruction (coerce-to-single-float single-float-from-long-float) (single-float) (long-float))

(define-fnrecord coerce-to-double-float (x)
  (:properties :foldable :movable)
  (:differentiator _ (declare (ignore x)) (wrap 1D0))
  (:specializer
   (ntype-subtypecase (wrapper-ntype x)
     (double-float (wrap x))
     (short-float (wrap (double-float-from-short-float x)))
     (single-float (wrap (double-float-from-single-float x)))
     (long-float (wrap (double-float-from-long-float x)))
     (t (wrap-default (type-specifier-ntype 'double-float))))))

(define-simple-instruction (coerce-to-double-float double-float-from-short-float) (double-float) (short-float))
(define-simple-instruction (coerce-to-double-float double-float-from-single-float) (double-float) (single-float))
(define-simple-instruction (coerce-to-double-float double-float-from-long-float) (double-float) (long-float))

(define-fnrecord coerce-to-long-float (x)
  (:properties :foldable :movable)
  (:differentiator _ (declare (ignore x)) (wrap 1L0))
  (:specializer
   (ntype-subtypecase (wrapper-ntype x)
     (long-float (wrap x))
     (short-float (wrap (long-float-from-short-float x)))
     (single-float (wrap (long-float-from-single-float x)))
     (double-float (wrap (long-float-from-double-float x)))
     (t (wrap-default (type-specifier-ntype 'long-float))))))

(define-simple-instruction (coerce-to-long-float long-float-from-short-float) (long-float) (short-float))
(define-simple-instruction (coerce-to-long-float long-float-from-single-float) (long-float) (single-float))
(define-simple-instruction (coerce-to-long-float long-float-from-double-float) (long-float) (double-float))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Complex Floating-Point Casts

(define-fnrecord coerce-to-complex-short-float (x)
  (:properties :foldable :movable)
  (:specializer
   (ntype-subtypecase (wrapper-ntype x)
     (short-float (wrap (complex-short-float-from-short-float x)))
     (single-float (wrap (complex-short-float-from-short-float (short-float-from-single-float x))))
     (double-float (wrap (complex-short-float-from-short-float (short-float-from-double-float x))))
     (long-float (wrap (complex-short-float-from-short-float (short-float-from-long-float x))))
     (complex-short-float (wrap x))
     (complex-single-float (wrap (complex-short-float-from-complex-single-float x)))
     (complex-double-float (wrap (complex-short-float-from-complex-double-float x)))
     (complex-long-float (wrap (complex-short-float-from-complex-long-float x)))
     (t (wrap-default (type-specifier-ntype 'complex-short-float))))))

(define-simple-instruction (coerce-to-complex-short-float complex-short-float-from-short-float) (complex-short-float) (short-float))
(define-simple-instruction (coerce-to-complex-short-float complex-short-float-from-complex-single-float) (complex-short-float) (complex-single-float))
(define-simple-instruction (coerce-to-complex-short-float complex-short-float-from-complex-double-float) (complex-short-float) (complex-double-float))
(define-simple-instruction (coerce-to-complex-short-float complex-short-float-from-complex-long-float) (complex-short-float) (complex-long-float))

(define-fnrecord coerce-to-complex-single-float (x)
  (:properties :foldable :movable)
  (:specializer
   (ntype-subtypecase (wrapper-ntype x)
     (short-float (wrap (complex-single-float-from-single-float (single-float-from-short-float x))))
     (single-float (wrap (complex-single-float-from-single-float x)))
     (double-float (wrap (complex-single-float-from-single-float (single-float-from-double-float x))))
     (long-float (wrap (complex-single-float-from-single-float (single-float-from-long-float x))))
     (complex-single-float (wrap x))
     (complex-short-float (wrap (complex-single-float-from-complex-short-float x)))
     (complex-double-float (wrap (complex-single-float-from-complex-double-float x)))
     (complex-long-float (wrap (complex-single-float-from-complex-long-float x)))
     (t (wrap-default (type-specifier-ntype 'complex-single-float))))))

(define-simple-instruction (coerce-to-complex-single-float complex-single-float-from-single-float) (complex-single-float) (single-float))
(define-simple-instruction (coerce-to-complex-single-float complex-single-float-from-complex-short-float) (complex-single-float) (complex-short-float))
(define-simple-instruction (coerce-to-complex-single-float complex-single-float-from-complex-double-float) (complex-single-float) (complex-double-float))
(define-simple-instruction (coerce-to-complex-single-float complex-single-float-from-complex-long-float) (complex-single-float) (complex-long-float))

(define-fnrecord coerce-to-complex-double-float (x)
  (:properties :foldable :movable)
  (:specializer
   (ntype-subtypecase (wrapper-ntype x)
     (short-float (wrap (complex-double-float-from-double-float (double-float-from-short-float x))))
     (single-float (wrap (complex-double-float-from-double-float (double-float-from-single-float x))))
     (double-float (wrap (complex-double-float-from-double-float x)))
     (long-float (wrap (complex-double-float-from-double-float (double-float-from-long-float x))))
     (complex-double-float (wrap x))
     (complex-short-float (wrap (complex-double-float-from-complex-short-float x)))
     (complex-single-float (wrap (complex-double-float-from-complex-single-float x)))
     (complex-long-float (wrap (complex-double-float-from-complex-long-float x)))
     (t (wrap-default (type-specifier-ntype 'complex-double-float))))))

(define-simple-instruction (coerce-to-complex-double-float complex-double-float-from-double-float) (complex-double-float) (double-float))
(define-simple-instruction (coerce-to-complex-double-float complex-double-float-from-complex-short-float) (complex-double-float) (complex-short-float))
(define-simple-instruction (coerce-to-complex-double-float complex-double-float-from-complex-single-float) (complex-double-float) (complex-single-float))
(define-simple-instruction (coerce-to-complex-double-float complex-double-float-from-complex-long-float) (complex-double-float) (complex-long-float))

(define-fnrecord coerce-to-complex-long-float (x)
  (:properties :foldable :movable)
  (:specializer
   (ntype-subtypecase (wrapper-ntype x)
     (short-float (wrap (complex-long-float-from-long-float (long-float-from-short-float x))))
     (single-float (wrap (complex-long-float-from-long-float (long-float-from-single-float x))))
     (double-float (wrap (complex-long-float-from-long-float (long-float-from-double-float x))))
     (long-float (wrap (complex-long-float-from-long-float x)))
     (complex-long-float (wrap x))
     (complex-short-float (wrap (complex-long-float-from-complex-short-float x)))
     (complex-single-float (wrap (complex-long-float-from-complex-single-float x)))
     (complex-double-float (wrap (complex-long-float-from-complex-double-float x)))
     (t (wrap-default (type-specifier-ntype 'complex-long-float))))))

(define-simple-instruction (coerce-to-complex-long-float complex-long-float-from-long-float) (complex-long-float) (long-float))
(define-simple-instruction (coerce-to-complex-long-float complex-long-float-from-complex-short-float) (complex-long-float) (complex-short-float))
(define-simple-instruction (coerce-to-complex-long-float complex-long-float-from-complex-single-float) (complex-long-float) (complex-single-float))
(define-simple-instruction (coerce-to-complex-long-float complex-long-float-from-complex-double-float) (complex-long-float) (complex-double-float))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FLOAT

(define-fnrecord float (number &optional (prototype nil prototype-supplied-p))
  (:properties :foldable :movable)
  (:specializer
   (if prototype-supplied-p
       (ntype-subtypecase (wrapper-ntype prototype)
         ((not float) (abort-specialization))
         (short-float (wrap (coerce-to-short-float number)))
         (single-float (wrap (coerce-to-single-float number)))
         (double-float (wrap (coerce-to-double-float number)))
         (long-float (wrap (coerce-to-long-float number)))
         (t (wrap-default (type-specifier-ntype 'float))))
       (ntype-subtypecase (wrapper-ntype number)
         ((not real) (abort-specialization))
         (float (wrap number))
         ((not float) (wrap (coerce-to-single-float number)))
         (t (wrap-default (type-specifier-ntype 'float)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COERCE

(define-fnrecord coerce (object type-specifier)
  (:properties :foldable :movable)
  (:specializer
   (let ((object-ntype (wrapper-ntype object))
         (type-specifier-ntype (wrapper-ntype type-specifier)))
     (if (not (eql-ntype-p type-specifier-ntype))
         (wrap-default (universal-ntype))
         (multiple-value-bind (result-ntype precise-p)
             (type-specifier-ntype (eql-ntype-object type-specifier-ntype))
           (cond ((not precise-p)
                  (ntype-subtypecase result-ntype
                    (real (wrap-default result-ntype))
                    (t (wrap-default (universal-ntype)))))
                 ((ntype-subtypep object-ntype result-ntype)
                  (wrap object))
                 (t
                  (ntype-subtypecase result-ntype
                    (nil (abort-specialization))
                    (short-float (wrap (coerce-to-short-float object)))
                    (single-float (wrap (coerce-to-single-float object)))
                    (double-float (wrap (coerce-to-double-float object)))
                    (long-float (wrap (coerce-to-long-float object)))
                    (complex-short-float (wrap (coerce-to-complex-short-float object)))
                    (complex-single-float (wrap (coerce-to-complex-single-float object)))
                    (complex-double-float (wrap (coerce-to-complex-double-float object)))
                    (complex-long-float (wrap (coerce-to-complex-long-float object)))
                    (float
                     (ntype-subtypecase object-ntype
                       ((not real) (abort-specialization))
                       (rational (wrap (coerce-to-single-float object)))
                       (float (wrap object))
                       (t (wrap-default (type-specifier-ntype 'float)))))
                    (complex
                     (ntype-subtypecase object-ntype
                       ((not number) (abort-specialization))
                       (rational (wrap object))
                       (short-float (wrap (coerce-to-complex-short-float object)))
                       (single-float (wrap (coerce-to-complex-single-float object)))
                       (double-float (wrap (coerce-to-complex-double-float object)))
                       (long-float (wrap (coerce-to-complex-long-float object)))
                       ((not rational) (wrap-default (type-specifier-ntype 'complex)))
                       (t (wrap-default (type-specifier-ntype 'number)))))
                    (t
                     (wrap-default result-ntype))))))))))
