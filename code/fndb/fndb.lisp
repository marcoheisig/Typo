(in-package #:typo.fndb)

(defgeneric fndb-record-function-name (fndb-record))

(defgeneric fndb-record-min-arguments (fndb-record))

(defgeneric fndb-record-max-arguments (fndb-record))

(defgeneric fndb-record-purep (fndb-record))

(defgeneric fndb-record-specializer (fndb-record))

(defgeneric fndb-record-differentiator (fndb-record))

(defstruct (fndb
            (:predicate fndbp)
            (:constructor make-fndb ()))
  (function-table (make-hash-table :test #'eq)
   :type hash-table
   :read-only t)
  (setf-function-table (make-hash-table :test #'eq)
   :type hash-table
   :read-only t))

(defvar *fndb* (make-fndb))
(declaim (type fndb *fndb*))

(defclass fndb-record ()
  ((%function-name
    :initarg :function-name
    :initform (alexandria:required-argument :function-name)
    :type function-name
    :reader fndb-record-function-name)
   (%min-arguments
    :initarg :min-arguments
    :initform (alexandria:required-argument :min-arguments)
    :type unsigned-byte
    :reader fndb-record-min-arguments)
   (%max-arguments
    :initarg :max-arguments
    :initform (alexandria:required-argument :max-arguments)
    :type unsigned-byte
    :reader fndb-record-max-arguments)
   (%purep
    :initarg :purep
    :initform (alexandria:required-argument :purep)
    :type (member t nil :inherited)
    :reader fndb-record-purep)
   (%specializer
    :initarg :specializer
    :initform (alexandria:required-argument :specializer)
    :type (or function (eql :inherited))
    :reader fndb-record-specializer)
   (%differentiator
    :initarg :differentiator
    :initform (alexandria:required-argument :differentiator)
    :type (or function (eql :inherited))
    :reader fndb-record-differentiator)))

(defvar *default-fndb-record*
  (make-instance 'fndb-record
    :function-name 'dummy
    :min-arguments 0
    :max-arguments (1- call-arguments-limit)
    :purep nil
    :specializer
    (lambda (&rest arguments)
      (declare (ignore arguments))
      (give-up-specialization))
    :differentiator
    (lambda (&rest arguments)
      (declare (ignore arguments))
      (error "Cannot differentiate this function."))))

(defun ensure-fndb-record
    (function-name lambda-list
     &key
       (parent *default-fndb-record*)
       (purep (fndb-record-purep parent))
       (specializer (fndb-record-specializer parent))
       (differentiator (fndb-record-differentiator parent)))
  (declare (function-name function-name)
           (list lambda-list)
           (fndb-record parent)
           (boolean purep)
           (type (or function null) specializer differentiator))
  ;; Check that the function of that name exists.
  (assert (fboundp function-name))
  (multiple-value-bind (min-arguments max-arguments)
      (lambda-list-arity lambda-list)
    ;; Check that the lambda list arity matches the arity of the
    ;; corresponding function.
    (multiple-value-bind (fn-min-arguments fn-max-arguments)
        (function-arity function-name)
      (assert (<= min-arguments fn-min-arguments))
      (assert (>= max-arguments fn-max-arguments)))
    ;; Ensure that the suitable record exists.
    (multiple-value-bind (key table)
        (trivia:ematch function-name
          ((list 'setf (and name (type non-nil-symbol)))
           (values name (fndb-setf-function-table *fndb*)))
          ((type non-nil-symbol)
           (values function-name (fndb-function-table *fndb*)))
          (_ (error "Invalid function-name ~S" function-name)))
      (multiple-value-bind (record present-p)
          (alexandria:ensure-gethash
           key
           table
           (make-instance 'fndb-record
             :function-name function-name
             :min-arguments min-arguments
             :max-arguments max-arguments
             :purep purep
             :specializer specializer
             :differentiator differentiator))
        (when present-p
          (reinitialize-instance record
            :function-name function-name
            :min-arguments min-arguments
            :max-arguments max-arguments
            :purep purep
            :specializer specializer
            :differentiator differentiator))
        record))))

(defun find-fndb-record (function-designator &optional (errorp t))
  (multiple-value-bind (key table)
      (trivia:ematch function-designator
        ((list 'setf (and name (type non-nil-symbol)))
         (values name (fndb-setf-function-table *fndb*)))
        ((type non-nil-symbol)
         (values function-designator (fndb-function-table *fndb*)))
        ((type function)
         (values function-designator (fndb-function-table *fndb*)))
        (_ (error "Invalid function designator ~S" function-designator)))
    (alexandria:ensure-gethash
     key
     table
     (if errorp
         (error "There is no fndb record for ~S" function-designator)
         (make-instance 'fndb-record
           :function-name (if (functionp function-designator)
                              (gensym "UNKNOWN-FUNCTION")
                              function-designator)
           :min-arguments (fndb-record-min-arguments *default-fndb-record*)
           :max-arguments (fndb-record-max-arguments *default-fndb-record*)
           :purep (fndb-record-purep *default-fndb-record*)
           :specializer (fndb-record-specializer *default-fndb-record*)
           :differentiator (fndb-record-differentiator *default-fndb-record*))))))

(define-compiler-macro find-fndb-record (&whole form function-designator &optional (errorp t))
  (if (and (constantp function-designator)
           (constantp errorp))
      `(load-time-value
        (locally (declare (notinline find-fndb-record))
          (find-fndb-record ,function-designator ,errorp)))
      form))
