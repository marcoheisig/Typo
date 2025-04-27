(in-package #:typo.vm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COMPLEX

(define-fnrecord complex
    (realpart &optional (imagpart 0))
  (:properties :foldable :movable)
  (:specializer
   (let* ((realpart-ntype (wrapper-ntype realpart))
          (imagpart-ntype (wrapper-ntype imagpart)))
     (if (and (eql-ntype-p imagpart)
              (eql (eql-ntype-object imagpart) 0)
              (ntype-subtypep realpart-ntype (type-specifier-ntype 'rational)))
         realpart
         (ntype-subtypecase (ntype-contagion realpart-ntype imagpart-ntype)
           (short-float
            (wrap
             (short-float-complex
              (coerce-to-short-float realpart)
              (coerce-to-short-float imagpart))))
           (single-float
            (wrap
             (single-float-complex
              (coerce-to-single-float realpart)
              (coerce-to-single-float imagpart))))
           (double-float
            (wrap
             (double-float-complex
              (coerce-to-double-float realpart)
              (coerce-to-double-float imagpart))))
           (long-float
            (wrap
             (long-float-complex
              (coerce-to-long-float realpart)
              (coerce-to-long-float imagpart))))
           (t
            (ntype-subtypecase imagpart-ntype
              ((not rational) (wrap-default 'complex))
              (t (wrap-default (type-specifier-ntype 'number))))))))))

(define-simple-instruction (complex short-float-complex) (complex-short-float) (short-float short-float))
(define-simple-instruction (complex single-float-complex) (complex-single-float) (single-float single-float))
(define-simple-instruction (complex double-float-complex) (complex-double-float) (double-float double-float))
(define-simple-instruction (complex long-float-complex) (complex-long-float) (long-float long-float))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; REALPART

(define-fnrecord realpart (number)
  (:properties :foldable :movable)
  (:specializer
   (ntype-subtypecase (wrapper-ntype number)
     (complex-short-float (wrap (complex-short-float-realpart number)))
     (complex-single-float (wrap (complex-single-float-realpart number)))
     (complex-double-float (wrap (complex-double-float-realpart number)))
     (complex-long-float (wrap (complex-long-float-realpart number)))
     (real (wrap number))
     (t (wrap-default (type-specifier-ntype 'real))))))

(define-simple-instruction (realpart complex-short-float-realpart) (short-float) (complex-short-float))
(define-simple-instruction (realpart complex-single-float-realpart) (single-float) (complex-single-float))
(define-simple-instruction (realpart complex-double-float-realpart) (double-float) (complex-double-float))
(define-simple-instruction (realpart complex-long-float-realpart) (long-float) (complex-long-float))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; IMAGPART

(define-fnrecord imagpart (number)
  (:properties :foldable :movable)
  (:specializer
   (ntype-subtypecase (wrapper-ntype number)
     (complex-short-float (wrap (complex-short-float-imagpart number)))
     (complex-single-float (wrap (complex-single-float-imagpart number)))
     (complex-double-float (wrap (complex-double-float-imagpart number)))
     (complex-long-float (wrap (complex-long-float-imagpart number)))
     (real (wrap (* 0 number)))
     (t (wrap-default (type-specifier-ntype 'real))))))

(define-simple-instruction (imagpart complex-short-float-imagpart) (short-float) (complex-short-float))
(define-simple-instruction (imagpart complex-single-float-imagpart) (single-float) (complex-single-float))
(define-simple-instruction (imagpart complex-double-float-imagpart) (double-float) (complex-double-float))
(define-simple-instruction (imagpart complex-long-float-imagpart) (long-float) (complex-long-float))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CONJUGATE

(define-fnrecord conjugate (number)
  (:properties :foldable :movable)
  (:specializer
   (let ((ntype (wrapper-ntype number)))
     (if (and (eql-ntype-p ntype)
              (numberp (eql-ntype-object ntype)))
         (wrap-constant
          (conjugate (eql-ntype-object ntype)))
         (ntype-subtypecase ntype
           ((not number) (abort-specialization))
           (real (wrap number))
           (complex (wrap (complex (realpart number) (- (imagpart number)))))
           (t (wrap-default (type-specifier-ntype 'number))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PHASE

(define-fnrecord phase (number)
  (:properties :foldable :movable)
  (:specializer
   (let ((ntype (wrapper-ntype number)))
     (if (and (eql-ntype-p ntype)
              (numberp (eql-ntype-object ntype)))
         (wrap-constant
          (phase (eql-ntype-object ntype)))
         (ntype-subtypecase ntype
           ((not number) (abort-specialization))
           ((float 0e0 *) (wrap (float 0 number)))
           ((rational 0 *) (wrap 0))
           ((float * (0e0)) (wrap (float #.pi number)))
           ((rational * (0)) (wrap-constant (coerce pi 'single-float)))
           ((complex short-float) (wrap-default (type-specifier-ntype 'short-float)))
           ((complex single-float) (wrap-default (type-specifier-ntype 'single-float)))
           ((complex double-float) (wrap-default (type-specifier-ntype 'double-float)))
           ((complex long-float) (wrap-default (type-specifier-ntype 'long-float)))
           ((complex rational) (wrap-default (type-specifier-ntype 'single-float)))
           (t (wrap-default (type-specifier-ntype 'number))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CIS

(define-fnrecord cis (x)
  (:properties :foldable :movable)
  (:specializer
   (ntype-subtypecase (wrapper-ntype x)
     ((not real) (abort-specialization))
     (short-float (wrap (short-float-cis x)))
     (single-float (wrap (single-float-cis x)))
     (double-float (wrap (double-float-cis x)))
     (long-float (wrap (long-float-cis x)))
     (t (wrap-default (type-specifier-ntype 'complex))))))

(define-simple-instruction (cis short-float-cis) (complex-short-float) (short-float))
(define-simple-instruction (cis single-float-cis) (complex-single-float) (single-float))
(define-simple-instruction (cis double-float-cis) (complex-double-float) (double-float))
(define-simple-instruction (cis long-float-cis) (complex-long-float) (long-float))

