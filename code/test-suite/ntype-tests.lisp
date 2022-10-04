(in-package #:typo.test-suite)

(define-test ntype-of-test
  (loop for object in *test-objects* do
    (let* ((ntype (ntype-of object))
           (type (ntype-type-specifier ntype))
           (primitive-ntype (typo.ntype::ntype-primitive-ntype ntype))
           (primitive-type (ntype-type-specifier primitive-ntype)))
      (is (typep object type))
      (is (typep object primitive-type)))))

(define-test ntype-parsing-test
  (let ((type-specifiers (loop for object in *test-objects*
                               collect (type-of object)
                               collect `(eql ,object)
                               collect (class-name (class-of object)))))
    (loop for type-specifier in type-specifiers do
      (multiple-value-bind (ntype precise-p)
          (type-specifier-ntype type-specifier)
        (is (subtypep type-specifier (ntype-type-specifier ntype)))
        (when precise-p
          (is (subtypep (ntype-type-specifier ntype) type-specifier)))))))

(define-test ntype-reasoning-test
  (let ((ntypes (remove-duplicates (append (coerce typo.ntype::*primitive-ntypes* 'list)
                                           (mapcar #'ntype-of *test-objects*)))))
    (setf ntypes (remove-if #'eql-ntype-p ntypes))
    (loop for ntype-1 in ntypes do
      (loop for ntype-2 in ntypes do
        (let ((type-specifier-1 (ntype-type-specifier ntype-1))
              (type-specifier-2 (ntype-type-specifier ntype-2)))
          (multiple-value-bind (union-ntype precise-p) (ntype-union ntype-1 ntype-2)
            (flet ((objects-matching (type)
                     (remove-if-not
                      (lambda (x) (typep x type))
                      *test-objects*)))
              (let* ((set-1 (objects-matching type-specifier-1))
                     (set-2 (objects-matching type-specifier-2))
                     (computed-union (objects-matching (ntype-type-specifier union-ntype)))
                     (expected-union (cl:union set-1 set-2)))
                (if precise-p
                    (is (null (set-difference computed-union expected-union)))
                    (is (subsetp expected-union computed-union)))))))))))
