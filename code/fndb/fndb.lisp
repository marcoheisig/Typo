(in-package #:typo.fndb)

(defgeneric fnrecord-function-name (fnrecord))

(defgeneric fnrecord-min-arguments (fnrecord))

(defgeneric fnrecord-max-arguments (fnrecord))

(defgeneric fnrecord-purep (fnrecord))

(defgeneric fnrecord-specializer (fnrecord))

(defgeneric fnrecord-differentiator (fnrecord))

(defgeneric update-fnrecord (fnrecord &rest initargs &key &allow-other-keys))

(defstruct (fndb
            (:predicate fndbp)
            (:constructor make-fndb ()))
  (function-table (make-hash-table :test #'eq)
   :type hash-table
   :read-only t)
  (function-name-table (make-hash-table :test #'eq)
   :type hash-table
   :read-only t)
  (setf-function-name-table (make-hash-table :test #'eq)
   :type hash-table
   :read-only t))

(defvar *fndb* (make-fndb))
(declaim (type fndb *fndb*))

(defun make-default-specializer (function-designator)
  (lambda (&rest args)
    (wrap-function function-designator args '() '() (universal-ntype))))

(defun make-default-differentiator (function-designator)
  (lambda (&rest args)
    (declare (ignore args))
    (error "Don't know the derivative of ~S."
           function-designator)))

(defclass fnrecord ()
  ((%function-name
    :initarg :function-name
    :initform (alexandria:required-argument :function-name)
    :type function-name
    :reader fnrecord-function-name)
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

(defmethod reinitialize-instance
    ((fnrecord fnrecord)
     &key
       (function-name (alexandria:required-argument :function-name))
       (min-arguments (alexandria:required-argument :min-arguments))
       (max-arguments (alexandria:required-argument :max-arguments))
       (parent nil)
       (purep
        (if parent
            (fnrecord-purep parent)
            nil))
       (specializer
        (if parent
            (fnrecord-specializer parent)
            (make-default-specializer function-name)))
       (differentiator
        (if parent
            (fnrecord-differentiator parent)
            (make-default-differentiator function-name))))
  (declare (boolean purep)
           (type function specializer differentiator))
  ;; Check that the function of that name exists.
  (assert (fboundp function-name))
  ;; Check that the lambda list arity matches the arity of the
  ;; corresponding function.
  (multiple-value-bind (fn-min-arguments fn-max-arguments)
      (function-arity function-name)
    (assert (<= min-arguments fn-min-arguments))
    (assert (>= max-arguments fn-max-arguments)))
  (call-next-method
   fnrecord
   :function-name function-name
   :min-arguments min-arguments
   :max-arguments max-arguments
   :purep purep
   :specializer specializer
   :differentiator differentiator))

(defun ensure-fnrecord (function-designator)
  (multiple-value-bind (key table function-name)
      (trivia:ematch function-designator
        ((list 'setf (and function-name (type non-nil-symbol)))
         (values function-name
                 (fndb-setf-function-name-table *fndb*)
                 function-designator))
        ((and function-name (type non-nil-symbol))
         (values function-name
                  (fndb-function-name-table *fndb*)
                  function-name))
        ((type function)
         (values function-designator
                 (fndb-function-table *fndb*)
                 (gensym "UNKNOWN-FUNCTION")))
        (_ (error "Invalid function designator: ~S"
                  function-designator)))
    (alexandria:ensure-gethash
     key
     table
     (let ((record
             (make-instance 'fnrecord
               :function-name function-name
               :min-arguments 0
               :max-arguments (1- call-arguments-limit)
               :purep nil
               :specializer (make-default-specializer function-name)
               :differentiator (make-default-differentiator function-name))))
       (unless (functionp function-designator)
         (when (fboundp function-name)
           (setf (gethash (fdefinition function-name) (fndb-function-table *fndb*))
                 record)))
       record))))

(define-compiler-macro ensure-fnrecord (&whole form function-designator)
  (if (constantp function-designator)
      `(load-time-value
        (locally (declare (notinline ensure-fnrecord))
          (ensure-fnrecord ,function-designator)))
      form))

(defun find-fnrecord (function-designator &optional (errorp t))
  (multiple-value-bind (key table)
      (trivia:ematch function-designator
        ((list 'setf (and name (type non-nil-symbol)))
         (values name (fndb-setf-function-name-table *fndb*)))
        ((type non-nil-symbol)
         (values function-designator (fndb-function-name-table *fndb*)))
        ((type function)
         (values function-designator (fndb-function-table *fndb*)))
        (_ (error "Invalid function designator ~S" function-designator)))
    (multiple-value-bind (record present-p) (gethash key table)
      (cond (present-p record)
            (errorp (error "There is no fnrecord for ~S" function-designator))
            (t nil)))))

(define-compiler-macro find-fnrecord (&whole form function-designator &optional (errorp t))
  (if (and (constantp function-designator)
           (constantp errorp))
      `(load-time-value
        (locally (declare (notinline find-fnrecord))
          (find-fnrecord ,function-designator ,errorp)))
      form))

(defun function-specializer (function-designator)
  (fnrecord-specializer (ensure-fnrecord function-designator)))

(define-compiler-macro function-specializer (function-designator)
  `(fnrecord-specializer (ensure-fnrecord ,function-designator)))

(defun function-differentiator (function-designator)
  (fnrecord-differentiator (ensure-fnrecord function-designator)))

(define-compiler-macro function-differentiator (function-designator)
  `(fnrecord-differentiator (ensure-fnrecord ,function-designator)))
