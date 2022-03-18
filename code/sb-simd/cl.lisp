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
