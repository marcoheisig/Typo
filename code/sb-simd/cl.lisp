(in-package #:typo.sb-simd)

#+(or)
(define-fnrecord short-float-abs (x)
  (:parent abs)
  (:specializer (wrap (f32-abs x))))

#+(or)
(define-fnrecord single-float-abs (x)
  (:parent abs)
  (:specializer (wrap (f32-abs x))))

#+(or)
(define-fnrecord double-float-abs (x)
  (:parent abs)
  (:specializer (wrap (f64-abs x))))

#+(or)
(define-fnrecord long-float-abs (x)
  (:parent abs)
  (:specializer (wrap (f64-abs x))))

;;; add.lisp

(define-fnrecord short-float+ (a b)
  (:parent +)
  (:specializer (wrap (sb-simd::two-arg-f32+ a b))))

(define-fnrecord single-float+ (a b)
  (:parent +)
  (:specializer (wrap (sb-simd::two-arg-f32+ a b))))

(define-fnrecord double-float+ (a b)
  (:parent +)
  (:specializer (wrap (sb-simd::two-arg-f64+ a b))))

(define-fnrecord long-float+ (a b)
  (:parent +)
  (:specializer (wrap (sb-simd::two-arg-f64+ a b))))

;;; array.lisp

(define-fnrecord short-float-row-major-aref (array index)
  (:parent row-major-aref)
  (:specializer (wrap (sb-simd::f32-row-major-aref array index))))

(define-fnrecord single-float-row-major-aref (array index)
  (:parent row-major-aref)
  (:specializer (wrap (sb-simd::f32-row-major-aref array index))))

(define-fnrecord double-float-row-major-aref (array index)
  (:parent row-major-aref)
  (:specializer (wrap (sb-simd::f64-row-major-aref array index))))

(define-fnrecord long-float-row-major-aref (array index)
  (:parent row-major-aref)
  (:specializer (wrap (sb-simd::f64-row-major-aref array index))))

(define-fnrecord (setf short-float-row-major-aref) (value array index)
  (:parent (setf row-major-aref))
  (:specializer (wrap (setf (sb-simd::f32-row-major-aref array index) value))))

(define-fnrecord (setf single-float-row-major-aref) (value array index)
  (:parent (setf row-major-aref))
  (:specializer (wrap (setf (sb-simd::f32-row-major-aref array index) value))))

(define-fnrecord (setf double-float-row-major-aref) (value array index)
  (:parent (setf row-major-aref))
  (:specializer (wrap (setf (sb-simd::f64-row-major-aref array index) value))))

(define-fnrecord (setf long-float-row-major-aref) (value array index)
  (:parent (setf row-major-aref))
  (:specializer (wrap (setf (sb-simd::f64-row-major-aref array index) value))))

;;; mul.lisp

(define-fnrecord short-float* (a b)
  (:parent *)
  (:specializer (wrap (sb-simd::two-arg-f32* a b))))

(define-fnrecord single-float* (a b)
  (:parent *)
  (:specializer (wrap (sb-simd::two-arg-f32* a b))))

(define-fnrecord double-float* (a b)
  (:parent *)
  (:specializer (wrap (sb-simd::two-arg-f64* a b))))

(define-fnrecord long-float* (a b)
  (:parent *)
  (:specializer (wrap (sb-simd::two-arg-f64* a b))))
