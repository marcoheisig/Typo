(in-package #:typo.fndb)

(define-fndb-record apply (function arg &rest more-args)
  (:specializer
   (check-ntype function function-designator)
   (let ((tail (if (null more-args)
                   arg
                   (car (last more-args)))))
     (ntype-subtypecase (wrapper-ntype tail)
       ((not list) (abort-specialization))
       (null (apply (function-specializer 'funcall) function arg (butlast more-args)))
       ;; We give up here, because we cannot determine the number of values
       ;; returned by APPLY.
       (t (give-up-specialization))))))

(define-fndb-record fdefinition (name)
  (:specializer
   (check-ntype name function-name)
   (wrap-default (type-specifier-ntype 't))))

(define-fndb-record fboundp (name)
  (:specializer
   (check-ntype name function-name)
   (wrap-default (type-specifier-ntype 't))))

(define-fndb-record fmakunbound (name)
  (:specializer
   (check-ntype name function-name)
   (wrap-default (type-specifier-ntype 'function-name))))

(define-fndb-record funcall (function &rest arguments)
  (:specializer
   (let ((function-ntype (wrapper-ntype function)))
     (if (eql-ntype-p function-ntype)
         (apply (function-specializer (eql-ntype-object function-ntype)) arguments)
         (progn
           (check-ntype function function)
           (give-up-specialization))))))

(define-fndb-record function-lambda-expression (function)
  (:specializer
   (check-ntype function function)
   (wrap-default (type-specifier-ntype 'list))))

(define-fndb-record not (x)
  (:specializer
   (let ((ntype (wrapper-ntype x)))
     (if (eql-ntype-p ntype)
         (wrap-constant (not (eql-ntype-object x)))
         (ntype-subtypecase ntype
           (null (wrap t))
           ((not null) (wrap nil))
           (t (wrap-default (type-specifier-ntype 'boolean))))))))

(define-fndb-record eq (a b)
  (:specializer
   (wrap-default (type-specifier-ntype 'generalized-boolean))))

(define-fndb-record eql (a b)
  (:specializer
   (wrap-default (type-specifier-ntype 'generalized-boolean))))

(define-fndb-record equal (a b)
  (:specializer
   (wrap-default (type-specifier-ntype 'generalized-boolean))))

(define-fndb-record equalp (a b)
  (:specializer
   (wrap-default (type-specifier-ntype 'generalized-boolean))))

(define-fndb-record identity (object)
  (:specializer
   (wrap object)))

(define-fndb-record complement (function)
  (:specializer
   (check-ntype function function)
   (wrap-default (type-specifier-ntype 'function))))

(define-fndb-record constantly (value)
  (:specializer
   (wrap-default (type-specifier-ntype 'function))))

(define-fndb-record values (&rest objects)
  (:specializer
   (wrap-function 'values objects (mapcar #'wrapper-ntype objects) '() nil)))

(define-fndb-record values-list (list)
  (:specializer
   (check-ntype list list)
   (give-up-specialization)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Control Flow Directives

 (define-fndb-record choose (boolean a b)
   (:pure t)
   (:specializer
    (ntype-subtypecase (wrapper-ntype boolean)
      (null (wrap b))
      ((not null) (wrap a))
      (t (wrap-default (ntype-union (wrapper-ntype a) (wrapper-ntype b)))))))

(define-fndb-record and-fn (&rest args)
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

(define-fndb-record or-fn (&rest args)
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

 (define-fndb-record prog2-fn (a b)
   (:pure t)
   (:specializer
    (wrap-function 'prog2-fn (list a b) (list (wrapper-ntype b)) '() nil)))
