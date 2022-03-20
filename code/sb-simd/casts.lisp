(in-package #:typo.sb-simd)

(macrolet
    ((define-scalar-cast-fnrecord (scalar-cast-record-name)
       (with-accessors ((name scalar-cast-record-name))
           (find-function-record scalar-cast-record-name)
         `(define-fnrecord ,name (x)
            (:pure t)
            (:specializer
             (wrap (coerce x ',name))))))
     (define-scalar-cast-fnrecords ()
       `(progn
          ,@(loop for scalar-cast-record in (filter-function-records #'scalar-cast-record-p)
                  collect `(define-scalar-cast-fnrecord ,(function-record-name scalar-cast-record))))))
  (define-scalar-cast-fnrecords))

(macrolet
    ((define-simd-cast-fnrecord (simd-cast-record-name)
       (with-accessors ((name simd-cast-record-name)
                        (broadcast-record simd-cast-record-broadcast))
           (find-function-record simd-cast-record-name)
         (let* ((broadcast (function-record-name broadcast-record))
                (simd-record (function-record-result-record broadcast-record))
                (real-record (simd-record-scalar-record simd-record))
                (simd-type (value-record-name simd-record))
                (real-type (value-record-name real-record)))
           `(define-fnrecord ,name (x)
              (:pure t)
              (:specializer
               (typo:ntype-subtypecase x
                 (,simd-type x)
                 (real (wrap (,broadcast (,real-type x))))
                 (t (wrap-default (typo:type-specifier-ntype ',simd-type)))))))))
     (define-reinterpret-cast-fnrecord (reinterpret-cast-record)
       (with-accessors ((name reinterpret-cast-record-name)
                        (result-record function-record-result-record))
           (find-function-record reinterpret-cast-record)
         `(define-fnrecord ,name (x)
            (:pure t)
            (:specializer
             (wrap-default (typo:type-specifier-ntype ',(value-record-name result-record)))))))
     (define-simd-cast-fnrecords ()
       `(progn
          ,@(loop for simd-cast-record in (filter-function-records #'simd-cast-record-p)
                  collect
                  `(define-simd-cast-fnrecord ,(simd-cast-record-name simd-cast-record)))))
     (define-reinterpret-cast-fnrecords ()
       `(progn
          ,@(loop for reinterpret-cast-record in (filter-function-records #'reinterpret-cast-record-p)
                  collect
                  `(define-reinterpret-cast-fnrecord ,(reinterpret-cast-record-name reinterpret-cast-record))))))
  (define-simd-cast-fnrecords)
  (define-reinterpret-cast-fnrecords))
