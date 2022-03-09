(in-package #:typo.common-lisp)

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
  (:pure t)
  (:specializer
   (let ((ntype (wrapper-ntype x)))
     (if (eql-ntype-p ntype)
         (wrap-constant (not (eql-ntype-object x)))
         (ntype-subtypecase ntype
           (null (wrap t))
           ((not null) (wrap nil))
           (t (wrap-default (type-specifier-ntype 'boolean))))))))

(define-fnrecord eq (a b)
  (:pure t)
  (:specializer
   (wrap-default (type-specifier-ntype 'generalized-boolean))))

(define-fnrecord eql (a b)
  (:pure t)
  (:specializer
   (wrap-default (type-specifier-ntype 'generalized-boolean))))

(define-fnrecord equal (a b)
  (:pure t)
  (:specializer
   (wrap-default (type-specifier-ntype 'generalized-boolean))))

(define-fnrecord equalp (a b)
  (:pure t)
  (:specializer
   (wrap-default (type-specifier-ntype 'generalized-boolean))))

(define-fnrecord identity (object)
  (:pure t)
  (:specializer
   (wrap object)))

(define-fnrecord complement (function)
  (:pure t)
  (:specializer
   (assert-wrapper-type function function)
   (wrap-default (type-specifier-ntype 'function))))

(define-fnrecord constantly (value)
  (:pure t)
  (:specializer
   (wrap-default (type-specifier-ntype 'function))))

(define-fnrecord values (&rest objects)
  (:pure t)
  (:specializer
   (wrap-function (ensure-fnrecord 'values) objects (mapcar #'wrapper-ntype objects) '() nil)))

(define-fnrecord values-list (list)
  (:pure t)
  (:specializer
   (assert-wrapper-type list list)
   (wrap-default* '() '() (type-specifier-ntype 't))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Control Flow Directives

 (define-fnrecord choose (boolean a b)
   (:pure t)
   (:specializer
    (ntype-subtypecase (wrapper-ntype boolean)
      (null (wrap b))
      ((not null) (wrap a))
      (t (wrap-default (ntype-union (wrapper-ntype a) (wrapper-ntype b)))))))

(define-fnrecord and-fn (&rest args)
  (:pure t)
  (:specializer
   (let ((sure t))
     (loop for arg in args do
       (ntype-subtypecase (wrapper-ntype arg)
         (null (return-from and-fn (wrap nil)))
         ((not null))
         (t (setf sure nil))))
     (if sure
         (wrap t)
         (wrap-default (type-specifier-ntype 'generalized-boolean))))))

(define-fnrecord or-fn (&rest args)
  (:pure t)
  (:specializer
   (let ((sure t))
     (loop for arg in args do
       (ntype-subtypecase (wrapper-ntype arg)
         ((not null)
          (if sure
              (return-from or-fn arg)
              (wrap-default (type-specifier-ntype '(not null)))))
         (null)
         (t (setf sure nil))))
     (if sure
         (wrap nil)
         (wrap-default (type-specifier-ntype 'generalized-boolean))))))

 (define-fnrecord prog2-fn (a b)
   (:pure t)
   (:specializer
    (wrap-function
     (ensure-fnrecord 'prog2-fn)
     (list a b) (list (wrapper-ntype b)) '() nil)))
