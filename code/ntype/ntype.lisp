(in-package #:typo.ntype)

(defgeneric ntype-type-specifier (ntype)
  (:documentation
   "Returns the type specifier for the type denoted by the supplied NTYPE."))

(defgeneric ntype-bits (ntype)
  (:documentation
   "Returns the number of bits that is required to encode any object of
the supplied NTYPE."))

(defgeneric ntype-subtypep (ntype1 ntype2)
  (:documentation
   "Returns whether NTYPE1 denotes a subtype of NTYPE2.  In contrast to
SUBTYPEP this function is always precise, so there is no need for returning
a second value."))

(defgeneric ntype-subtypepc2 (ntype1 ntype2)
  (:documentation
   "Returns whether NTYPE1 denotes a subtype of (not NTYPE2), and, as a
second value, whether the result has been derived with certainty.  Similar
to SUBTYPEP, this function is conservative and will never return the vaules
NIL, NIL."))

(defgeneric ntype-union (ntype1 ntype2)
  (:documentation
   "Returns the most specific ntype that encompasses both NTYPE1 and
NTYPE2.

A second value of T means the result is precise, while a second value of
NIL means the result is a generalization."))

(defgeneric ntype-intersection (ntype1 ntype2)
  (:documentation
   "Returns the most specific ntype that encompasses the intersection of
NTYPE1 and NTYPE2.

A second value of T means the result is precise, while a second value of
NIL means the result is a generalization."))

(defgeneric ntype-contagion (ntype1 ntype2)
  (:documentation
   "Returns the most specific ntype that encompasses the result of an
arithmetic operation on arguments of the supplied ntypes.

A second value of T means the result is precise, while a second value of
NIL means the result is a generalization."))

(defgeneric ntype= (ntype1 ntype2)
  (:documentation
   "Returns whether the two supplied ntypes denote the same type."))

(defgeneric make-eql-ntype (object)
  (:documentation
   "Returns the ntype whose sole element is the supplied OBJECT."))

(defgeneric type-specifier-ntype (type-specifier)
  (:documentation
   "Returns the most specific ntype that denotes a supertype of the type
denoted by the supplied type specifier.

A second value of T means the result is precise, while a second value of
NIL means the result is a generalization."))

(defgeneric make-array-ntype (&key element-type dimensions simplep)
  (:documentation
   "Returns the array ntype with the supplied element type, dimensions,
and simplep boolean.

A second value of T means the result is precise, while a second value of
NIL means the result is a generalization."))

(defgeneric array-element-ntype (array)
  (:documentation
   "Returns the ntype that denotes the array element type of the supplied
ARRAY.

A second value of T means the result is precise, while a second value of
NIL means the result is a generalization."))

(defgeneric complex-part-ntype (complex)
  (:documentation
   "Returns the ntype that denotes the complex part type of the supplied
COMPLEX.

A second value of T means the result is precise, while a second value of
NIL means the result is a generalization."))

(defgeneric upgraded-array-element-ntype (ntype)
  (:documentation
   "Returns the ntype that denotes the upgraded array element type of the
supplied NTYPE.

A second value of T means the result is precise, while a second value of
NIL means the result is a generalization."))

(defgeneric upgraded-complex-part-ntype (ntype)
  (:documentation
   "Returns the ntype that denotes the upgraded complex part type of the
supplied NTYPE.

A second value of T means the result is precise, while a second value of
NIL means the result is a generalization."))

(defun ntype-of (object)
  "Returns the ntype for the EQL type containing this object."
  (make-eql-ntype object))

(defstruct (ntype
            (:predicate ntypep)
            (:copier nil)
            (:constructor nil))
  ;; An index into the +primitive-ntypes+ vector, denoting the most
  ;; specialized primitive type of this ntype.
  (index (alexandria:required-argument :index)
   :type ntype-index :read-only t))

(defmethod print-object ((ntype ntype) stream)
  (print-unreadable-object (ntype stream :type t)
    (format stream "~S" (ntype-type-specifier ntype))))

;;; We define this method on NTYPE-SUBTYPEP immediately, so that the
;;; definitions in the next files can already use it at macroexpansion
;;; time.  More specific methods will be added later, but only to make it
;;; faster.
(defmethod ntype-subtypep
    ((ntype1 ntype)
     (ntype2 ntype))
  (values
   (subtypep
    (ntype-type-specifier ntype1)
    (ntype-type-specifier ntype2))
   t))

(define-compiler-macro type-specifier-ntype (&whole form type-specifier)
  (if (constantp type-specifier)
      (multiple-value-bind (ntype precise-p)
          (locally (declare (notinline type-specifier-ntype))
            (type-specifier-ntype (eval type-specifier)))
        `(values ,ntype ,precise-p))
      form))

;;; Primitive Ntypes

(defstruct (primitive-ntype
            (:include ntype)
            (:predicate primitive-ntype-p)
            (:copier nil)
            (:constructor %make-primitive-ntype))
  ;; The type specifier of the type denoted by this ntype.
  (type-specifier (alexandria:required-argument :type-specifier)
   :type type-specifier
   :read-only t)
  ;; The number of bits required to represent any object of this ntype.
  (bits (alexandria:required-argument :bits)
   :type unsigned-byte
   :read-only t)
  ;; The class corresponding to the type denoted by this ntype, or NIL, if
  ;; there is no such class.
  (class (alexandria:required-argument :class)
   :type (or null class)))

(defmethod ntype-type-specifier ((primitive-ntype primitive-ntype))
  (primitive-ntype-type-specifier primitive-ntype))

(defmethod ntype-bits ((primitive-ntype primitive-ntype))
  (primitive-ntype-bits primitive-ntype))

;;; EQL Ntypes

(defstruct (eql-ntype
            (:include ntype)
            (:predicate eql-ntype-p)
            (:copier nil)
            (:constructor %make-eql-ntype))
  (object (alexandria:required-argument :object)
   :type t
   :read-only t))

(defmethod ntype-type-specifier ((eql-ntype eql-ntype))
  `(eql ,(eql-ntype-object eql-ntype)))

;;; Array Ntypes

(defstruct (array-ntype
            (:include ntype)
            (:predicate array-ntype-p)
            (:copier nil)
            (:constructor %make-array-ntype))
  (element-ntype (alexandria:required-argument :element-ntype)
   :type (or ntype (eql *))
   :read-only t)
  (dimensions (alexandria:required-argument :dimensions)
   :type (or list unsigned-byte (eql *))
   :read-only t)
  ;; Whether the ntype denotes a simple array.  A value of NIL doesn't mean
  ;; the array is not simple, only that we don't know.
  (simplep (alexandria:required-argument :simplep)
   :type boolean
   :read-only t))

(defmethod ntype-type-specifier ((array-ntype array-ntype))
  (let ((element-type
          (ntype-type-specifier
           (array-ntype-element-ntype array-ntype))))
    (if (array-ntype-simplep array-ntype)
        (trivia:ematch (array-ntype-dimensions array-ntype)
          ('*
            `(simple-array ,element-type))
          ((list* dimensions)
           `(simple-array ,element-type ,dimensions)))
        (trivia:ematch (array-ntype-dimensions array-ntype)
          ((list '*)
           `(vector ,element-type))
          ((list (and size (type unsigned-byte)))
           `(vector ,element-type ,size))
          ((list* dimensions)
           `(array ,element-type ,dimensions))
          ('*
            `(array ,element-type))))))
