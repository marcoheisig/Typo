(in-package #:typo.fndb)

(defgeneric fnrecord-name (fnrecord))

(defgeneric fnrecord-function (fnrecord))

(defgeneric fnrecord-min-arguments (fnrecord))

(defgeneric fnrecord-max-arguments (fnrecord))

(defgeneric fnrecord-purep (fnrecord))

(defgeneric fnrecord-specializer (fnrecord))

(defgeneric fnrecord-differentiator (fnrecord))

(defclass fnrecord ()
  ((%name
    :initarg :name
    :initform (alexandria:required-argument :name)
    :type (or null function-name)
    :reader fnrecord-name)
   (%function
    :initarg :function
    :initform (alexandria:required-argument :function)
    :type function
    :reader fnrecord-function)
   (%lambda-list
    :initarg :lambda-list
    :initform (alexandria:required-argument :lambda-list)
    :type list
    :reader fnrecord-lambda-list)
   (%min-arguments
    :initarg :min-arguments
    :initform (alexandria:required-argument :min-arguments)
    :type unsigned-byte
    :reader fnrecord-min-arguments)
   (%max-arguments
    :initarg :max-arguments
    :initform (alexandria:required-argument :max-arguments)
    :type unsigned-byte
    :reader fnrecord-max-arguments)
   (%purep
    :initarg :purep
    :initform (alexandria:required-argument :purep)
    :type boolean
    :reader fnrecord-purep)
   (%specializer
    :initarg :specializer
    :initform (alexandria:required-argument :specializer)
    :type function
    :reader fnrecord-specializer)
   (%differentiator
    :initarg :differentiator
    :initform (alexandria:required-argument :differentiator)
    :type function
    :reader fnrecord-differentiator)))

(defmethod print-object ((fnrecord fnrecord) stream)
  (print-unreadable-object (fnrecord stream :type t)
    (format stream "~S ~S"
            (fnrecord-name fnrecord)
            (fnrecord-lambda-list fnrecord))))

(defgeneric fnrecordp (object)
  (:method ((object t)) nil)
  (:method ((fnrecord fnrecord)) t))

(defun make-default-specializer (function-designator)
  (lambda (&rest args)
    (wrap-function function-designator args '() '() (universal-ntype))))

(defun make-default-differentiator (function-designator)
  (lambda (&rest args)
    (declare (ignore args))
    (error "Don't know the derivative of ~S."
           function-designator)))

(defmethod shared-initialize
    ((instance fnrecord) slot-names
     &rest initargs
     &key name
       (function (fdefinition name))
       (lambda-list (function-lambda-list function))
       (min-arguments (nth-value 0 (lambda-list-arity lambda-list)))
       (max-arguments (nth-value 1 (lambda-list-arity lambda-list)))
       (parent nil parent-supplied-p)
       (parent-fnrecord (if (not parent-supplied-p) nil (find-fnrecord parent)))
       (purep
        (if parent-fnrecord
            (fnrecord-purep parent-fnrecord)
            nil))
       (specializer
        (if parent-fnrecord
            (fnrecord-specializer parent-fnrecord)
            (make-default-specializer name)))
       (differentiator
        (if parent-fnrecord
            (fnrecord-differentiator parent-fnrecord)
            (make-default-differentiator name)))
     &allow-other-keys)
  (declare (notinline find-fnrecord))
  (apply #'call-next-method
         instance
         slot-names
         :name name
         :function function
         :lambda-list lambda-list
         :min-arguments min-arguments
         :max-arguments max-arguments
         :purep purep
         :specializer specializer
         :differentiator differentiator
         initargs))
