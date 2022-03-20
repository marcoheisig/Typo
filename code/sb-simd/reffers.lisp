(in-package #:typo.sb-simd)

(macrolet
    ((define-reffer-fnrecords ()
       `(progn
          ,@(loop for record in (filter-function-records
                                 (lambda (function-record)
                                   (eq (symbol-package
                                        (parse-function-name
                                         (function-record-name function-record)))
                                       (find-package "SB-SIMD"))))
                  for name = (function-record-name record)
                  for value-record = (first (function-record-result-records record))
                  for type = (value-record-name value-record)
                  when (aref-record-p record) collect
                    `(define-fnrecord ,name (array &rest subscripts)
                       (:parent aref)
                       (:specializer
                        (assert-wrapper-type array (array ,type))
                        (loop for subscript in subscripts do
                          (assert-wrapper-type subscript unsigned-byte))
                        (wrap-default (typo:type-specifier-ntype ',type))))
                  when (setf-aref-record-p record) collect
                    `(define-fnrecord ,name (value array &rest subscripts)
                       (:parent (setf aref))
                       (:specializer
                        (assert-wrapper-type value ,type)
                        (assert-wrapper-type array (array, type))
                        (loop for subscript in subscripts do
                          (assert-wrapper-type subscript unsigned-byte))
                        (wrap-default (wrapper-ntype value))))
                  when (row-major-aref-record-p record) collect
                    `(define-fnrecord ,name (array index)
                       (:parent row-major-aref)
                       (:specializer
                        (assert-wrapper-type array (array ,type))
                        (assert-wrapper-type index unsigned-byte)
                        (wrap-default (typo:type-specifier-ntype ',type))))
                  when (setf-row-major-aref-record-p record) collect
                    `(define-fnrecord ,name (value array index)
                       (:parent (setf row-major-aref))
                       (:specializer
                        (assert-wrapper-type value ,type)
                        (assert-wrapper-type array (array, type))
                        (assert-wrapper-type index unsigned-byte)
                        (wrap-default (wrapper-ntype value))))))))
  (define-reffer-fnrecords))
