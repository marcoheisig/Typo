(in-package #:typo.sb-simd)

(macrolet
    ((define-instruction-fnrecord (name)
       (with-accessors ((name instruction-record-name)
                        (argument-records instruction-record-argument-records)
                        (result-records instruction-record-result-records)
                        (pure instruction-record-pure))
           (find-function-record name)
         (let ((argument-record-names (mapcar #'record-name argument-records))
               (argument-symbols (prefixed-symbols "ARGUMENT-" (length argument-records)))
               (result-record-names (mapcar #'record-name result-records)))
           `(define-fnrecord ,name ,argument-symbols
              (:pure ,pure)
              (:specializer
               (if (and ,@(loop for arg in argument-symbols
                                for type in argument-record-names
                                collect
                                `(typo:ntype-subtypep
                                  (wrapper-ntype ,arg)
                                  (typo:type-specifier-ntype ',type))))
                   (wrap-default
                    ,@(loop for type in result-record-names
                            collect `(typo:type-specifier-ntype ',type)))
                   (wrap
                    (,name
                     ,@(loop for arg in argument-symbols
                             for cast in argument-record-names
                             collect `(,cast ,arg))))))))))
     (define-instruction-fnrecords ()
       `(progn
          ,@(loop for instruction-record in (filter-function-records #'instruction-record-p)
                  for name = (instruction-record-name instruction-record)
                  collect `(define-instruction-fnrecord ,name)))))
  (define-instruction-fnrecords))
