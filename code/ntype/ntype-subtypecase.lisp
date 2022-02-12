(in-package #:typo)

;;; The NTYPE-SUBTYPECASE macro is the workhorse of ntype reasoning.  It
;;; allows a user to quickly select the first clause whose type denotes a
;;; subtype of the supplied ntype.
;;;
;;; The trick is to convert the type specifier of each clause to an ntype
;;; at macroexpansion time, and rely on the much faster ntype-subtypep to
;;; find each clause that is a candidate.  Then, once a candidate has been
;;; found, we still have to decide whether that candidate is a match.
;;;
;;; 1. If the conversion of the clause's type to an ntype was precise, the
;;;    candidate is also a match.
;;;
;;; 2. If the ntype being matched is a primitive ntype, the candidate is
;;;    also a match, because even if the clause's type has been widened
;;;    during ntype conversion, it hasn't been widend more than to the next
;;;    appropriate primitive ntype.
;;;
;;; 3. If the ntype being matched is an EQL ntype, it is possible to use
;;;    TYPEP to check for a match.
;;;
;;; 4. If the ntype being matched is an array ntype and the conversion of
;;;    the clause's type to an ntype wasn't precise, we are out of luck and
;;;    actually use SUBTYPEP.

(defclass ntype-subtypecase-clause ()
  ((%type
    :initarg :type
    :initform (alexandria:required-argument :type)
    :type type-specifier
    :reader ntype-subtypecase-clause-type)
   (%ntype
    :initarg :ntype
    :initform (alexandria:required-argument :ntype)
    :type ntype
    :reader ntype-subtypecase-clause-ntype)
   (%ntype-precise-p
    :initarg :ntype-precise-p
    :initform (alexandria:required-argument :ntype-precise-p)
    :type boolean
    :reader ntype-subtypecase-clause-ntype-precise-p)
   (%body
    :initarg :body
    :initform (alexandria:required-argument :body)
    :reader ntype-subtypecase-clause-body)))

(defun parse-ntype-subtypep-clause (clause)
  (trivia:match clause
    ((list* type-specifier body)
     (unless (ignore-errors (prog1 t (typep 42 type-specifier)))
       (error "Not a valid type specifier: ~S"
              type-specifier))
     (multiple-value-bind (ntype ntype-precise-p)
         (type-specifier-ntype type-specifier)
       (make-instance 'ntype-subtypecase-clause
         :type type-specifier
         :ntype ntype
         :ntype-precise-p ntype-precise-p
         :body body)))
    (_ (error "Invalid ntype-subtypecase clause: ~S"
              clause))))

(defmacro ntype-subtypecase (ntype &body clauses)
  "Execute the forms in the first clause that starts with a type that is a
subtype of the supplied NTYPE."
  (alexandria:with-gensyms (outer inner)
    (alexandria:once-only (ntype)
      `(block ,outer
         ,@(loop for clause in clauses
                 collect
                 (with-accessors ((clause-type ntype-subtypecase-clause-type)
                                  (clause-ntype ntype-subtypecase-clause-ntype)
                                  (clause-body ntype-subtypecase-clause-body)
                                  (clause-ntype-precise-p ntype-subtypecase-clause-ntype-precise-p))
                     (parse-ntype-subtypep-clause clause)
                   `(block ,inner
                      (when (ntype-subtypep ,ntype ,clause-ntype)
                        ,@(unless clause-ntype-precise-p
                            `((typecase ,ntype
                                (eql-ntype
                                 (unless (typep (eql-ntype-object ,ntype) ',clause-type)
                                   (return-from ,inner)))
                                (array-ntype
                                 (unless (subtypep (ntype-type-specifier ,ntype) ',clause-type)
                                   (return-from ,inner))))))
                        (return-from ,outer (progn ,@clause-body))))))))))
