(in-package #:typo)

;;; The NTYPE-SUBTYPECASE macro is the workhorse of ntype reasoning.  It
;;; allows a user to quickly select the first clause whose type denotes a
;;; subtype of the supplied ntype.

(defmacro ntype-subtypecase (ntype &body clauses)
  "Execute the forms in the first clause that starts with a type that is a
subtype of the supplied NTYPE."
  (alexandria:once-only (ntype)
    (labels ((match-form (type-specifier)
               (trivia:match type-specifier
                 ((list* 'or type-specifiers)
                  `(or ,@(mapcar #'match-form type-specifiers)))
                 ((list* 'and type-specifiers)
                  `(or ,@(mapcar #'match-form type-specifiers)))
                 ((list 'not type-specifier)
                  (multiple-value-bind (match-ntype precise-p)
                      (type-specifier-ntype type-specifier)
                    (unless precise-p (trivia.fail:fail))
                    `(ntype-subtypepc2 ,ntype ,match-ntype)))
                 (_
                  (multiple-value-bind (match-ntype precise-p)
                      (type-specifier-ntype type-specifier)
                    (if precise-p
                        `(ntype-subtypep ,ntype ,match-ntype)
                        `(and (ntype-subtypep ,ntype ,match-ntype)
                              (etypecase ,ntype
                                (eql-ntype
                                 (typep (eql-ntype-object ,ntype) ',type-specifier))
                                (array-ntype
                                 (subtypep (ntype-type-specifier ,ntype) ',type-specifier))
                                (primitive-ntype
                                 (logbitp
                                  (ntype-index ,ntype)
                                  ,(let ((mask 0))
                                     (loop for p across *primitive-ntypes* do
                                       (when (and (ntype-subtypep p match-ntype)
                                                     (not (eq p match-ntype))
                                                     (subtypep
                                                      (primitive-ntype-type-specifier p)
                                                      type-specifier))
                                         (setf mask (logior mask (ash 1 (ntype-index p))))))
                                     mask)))))))))))
      `(cond
         ,@(loop for clause in clauses
                 collect
                 (trivia:match clause
                   ((list* type-specifier body)
                    `(,(match-form type-specifier) ,@body))
                   (_
                    (error "Invalid ntype-subtypep-clause: ~S" clause))))))))
