(in-package #:typo.vm)

(define-fnrecord apply (function arg &rest more-args)
  (:specializer
   (assert-wrapper-type function function-designator)
   (let ((tail (if (null more-args)
                   arg
                   (car (last more-args)))))
     (ntype-subtypecase (wrapper-ntype tail)
       ((not list) (abort-specialization))
       (null (apply (function-specializer 'funcall) function arg (butlast more-args)))
       ;; We give up here, because we cannot determine the number of values
       ;; returned by APPLY.
       (t (wrap-default* '() '() (type-specifier-ntype 't)))))))

(define-fnrecord fdefinition (name)
  (:specializer
   (assert-wrapper-type name function-name)
   (wrap-default (type-specifier-ntype 't))))

(define-fnrecord fboundp (name)
  (:specializer
   (assert-wrapper-type name function-name)
   (wrap-default (type-specifier-ntype 't))))

(define-fnrecord fmakunbound (name)
  (:specializer
   (assert-wrapper-type name function-name)
   (wrap-default (type-specifier-ntype 'function-name))))

(define-fnrecord funcall (function &rest arguments)
  (:specializer
   (let ((function-ntype (wrapper-ntype function)))
     (if (eql-ntype-p function-ntype)
         (apply (function-specializer (eql-ntype-object function-ntype)) arguments)
         (progn
           (assert-wrapper-type function function)
           (wrap-default* '() '() (type-specifier-ntype 't)))))))

(define-fnrecord function-lambda-expression (function)
  (:specializer
   (assert-wrapper-type function function)
   (wrap-default (type-specifier-ntype 'list))))

(define-fnrecord not (x)
  (:properties :foldable :movable)
  (:specializer
   (let ((ntype (wrapper-ntype x)))
     (if (eql-ntype-p ntype)
         (wrap-constant (not (eql-ntype-object x)))
         (ntype-subtypecase ntype
           (null (wrap t))
           ((not null) (wrap nil))
           (t (wrap-default (type-specifier-ntype 'boolean))))))))

(define-fnrecord eq (a b)
  (:properties :foldable :movable)
  (:specializer
   (wrap-default (type-specifier-ntype 'generalized-boolean))))

(define-fnrecord eql (a b)
  (:properties :foldable :movable)
  (:specializer
   (wrap-default (type-specifier-ntype 'generalized-boolean))))

(define-fnrecord equal (a b)
  (:properties :foldable)
  (:specializer
   (wrap-default (type-specifier-ntype 'generalized-boolean))))

(define-fnrecord equalp (a b)
  (:properties :foldable)
  (:specializer
   (wrap-default (type-specifier-ntype 'generalized-boolean))))

(define-fnrecord identity (object)
  (:properties :foldable :movable)
  (:specializer
   (wrap object)))

(define-fnrecord complement (function)
  (:properties :foldable :movable)
  (:specializer
   (assert-wrapper-type function function)
   (wrap-default (type-specifier-ntype 'function))))

(define-fnrecord constantly (value)
  (:properties :movable)
  (:specializer
   (wrap-default (type-specifier-ntype 'function))))

(define-fnrecord values (&rest objects)
  (:properties :foldable :movable)
  (:specializer
   (wrap-function (ensure-fnrecord 'values) objects (mapcar #'wrapper-ntype objects) '() nil)))

(define-fnrecord values-list (list)
  (:properties :foldable :movable)
  (:specializer
   (assert-wrapper-type list list)
   (wrap-default* '() '() (type-specifier-ntype 't))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Control Flow Directives

(define-fnrecord choose (boolean a b)
  (:properties :foldable :movable)
  (:specializer
   (ntype-subtypecase (wrapper-ntype boolean)
     (null (wrap b))
     ((not null) (wrap a))
     (t (wrap-default (ntype-union (wrapper-ntype a) (wrapper-ntype b)))))))

(define-fnrecord cswap (boolean a b)
  (:properties :foldable :movable)
  (:specializer
   (ntype-subtypecase (wrapper-ntype boolean)
     ((not null)
      (wrap (values a b)))
     (null
      (wrap (values b a)))
     (t
      (let ((union (ntype-union (wrapper-ntype a) (wrapper-ntype b))))
        (wrap-default union union))))))

(define-fnrecord cswap2 (boolean a1 a2 b1 b2)
  (:properties :foldable :movable)
  (:specializer
   (ntype-subtypecase (wrapper-ntype boolean)
     ((not null)
      (wrap (values a1 a2 b1 b2)))
     (null
      (wrap (values b1 b2 a1 a2)))
     (t
      (if (and (eql a1 a2)
               (eql b1 b2))
          (let* ((x (wrap (cswap boolean a1 b1)))
                 (l (wrapper-nth-value 0 x))
                 (r (wrapper-nth-value 1 x)))
            (wrap (values l l r r)))
          (let ((union1 (ntype-union (wrapper-ntype a1) (wrapper-ntype b1)))
                (union2 (ntype-union (wrapper-ntype a2) (wrapper-ntype b2))))
            (wrap-default union1 union2 union1 union2)))))))

(define-fnrecord and-fn (&rest args)
  (:properties :foldable :movable)
  (:specializer
   (let ((filtered-args '()))
     (loop for (arg . rest) on args do
       (ntype-subtypecase (wrapper-ntype arg)
         ;; If any of the arguments is always NIL, return NIL.
         (null (return-from and-fn (wrap nil)))
         ;; If an argument is never NIL, and it isn't the last form, it can be
         ;; filtered out.
         ((not null)
          (when (null rest)
            (push arg filtered-args)))
         (t
          (push arg filtered-args))))
     (let ((n-before (length args))
           (n-after (length filtered-args)))
       (cond ((= n-after 0)
              (wrap t))
             ((= n-after 1)
              (first filtered-args))
             ((< n-after n-before)
              (apply (function-specializer 'and-fn) (reverse filtered-args)))
             (t (wrap-default
                 (ntype-union
                  (type-specifier-ntype 'null)
                  (wrapper-ntype (first filtered-args))))))))))

(define-fnrecord or-fn (&rest args)
  (:properties :foldable :movable)
  (:specializer
   (let ((filtered-args (remove (ntype-of nil) args :key #'wrapper-ntype :test #'ntype=)))
     (let ((n-before (length args))
           (n-after (length filtered-args)))
       (cond ((= n-after 0)
              (wrap nil))
             ((= n-after 1)
              (first filtered-args))
             ((< n-after n-before)
              (apply (function-specializer 'or-fn) filtered-args))
             (t
              (let ((result-ntype (empty-ntype)))
                (loop for arg in args do
                  (let ((arg-ntype (wrapper-ntype arg)))
                    (setf result-ntype (ntype-union result-ntype arg-ntype))
                    (ntype-subtypecase arg-ntype
                      ((not null)
                       (loop-finish)))))
                (wrap-default result-ntype))))))))

 (define-fnrecord prog2-fn (a b)
   (:properties :foldable :movable)
   (:specializer
    (wrap-function
     (ensure-fnrecord 'prog2-fn)
     (list a b) (list (wrapper-ntype b)) '() nil)))

(define-fnrecord if (test then &optional (else (wrap nil)))
   (:properties :movable)
  (:specializer (wrap (if-fn test then else))))

(define-fnrecord or (&rest args)
  (:properties :movable)
  (:specializer
   (apply (function-specializer 'or-fn) args)))

(define-fnrecord and (&rest args)
  (:properties :movable)
  (:specializer
   (apply (function-specializer 'and-fn) args)))
