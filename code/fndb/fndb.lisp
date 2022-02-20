(in-package #:typo.fndb)

(defgeneric fndb-record-function-name (fndb-record))

(defgeneric fndb-record-min-arguments (fndb-record))

(defgeneric fndb-record-max-arguments (fndb-record))

(defgeneric fndb-record-purep (fndb-record))

(defgeneric fndb-record-specializer (fndb-record))

(defgeneric fndb-record-differentiator (fndb-record))

(defgeneric update-fndb-record (fndb-record &rest initargs &key &allow-other-keys))

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
    :type boolean
    :reader fndb-record-purep)
   (%specializer
    :initarg :specializer
    :initform (alexandria:required-argument :specializer)
    :type function
    :reader fndb-record-specializer)
   (%differentiator
    :initarg :differentiator
    :initform (alexandria:required-argument :differentiator)
    :type function
    :reader fndb-record-differentiator)))

(defmethod reinitialize-instance
    ((fndb-record fndb-record)
     &key
       (function-name (alexandria:required-argument :function-name))
       (min-arguments (alexandria:required-argument :min-arguments))
       (max-arguments (alexandria:required-argument :max-arguments))
       (parent nil)
       (purep
        (if parent
            (fndb-record-purep parent)
            nil))
       (specializer
        (if parent
            (fndb-record-specializer parent)
            (make-default-specializer function-name)))
       (differentiator
        (if parent
            (fndb-record-differentiator parent)
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
   fndb-record
   :function-name function-name
   :min-arguments min-arguments
   :max-arguments max-arguments
   :purep purep
   :specializer specializer
   :differentiator differentiator))

(defun ensure-fndb-record (function-designator)
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
             (make-instance 'fndb-record
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

(define-compiler-macro ensure-fndb-record (&whole form function-designator)
  (if (constantp function-designator)
      `(load-time-value
        (locally (declare (notinline ensure-fndb-record))
          (ensure-fndb-record ,function-designator)))
      form))

(defun find-fndb-record (function-designator &optional (errorp t))
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
            (errorp (error "There is no fndb record for ~S" function-designator))
            (t nil)))))

(define-compiler-macro find-fndb-record (&whole form function-designator &optional (errorp t))
  (if (and (constantp function-designator)
           (constantp errorp))
      `(load-time-value
        (locally (declare (notinline find-fndb-record))
          (find-fndb-record ,function-designator ,errorp)))
      form))

(defun function-specializer (function-designator)
  (fndb-record-specializer (ensure-fndb-record function-designator)))

(define-compiler-macro function-specializer (function-designator)
  `(fndb-record-specializer (ensure-fndb-record ,function-designator)))

(defun function-differentiator (function-designator)
  (fndb-record-differentiator (ensure-fndb-record function-designator)))

(define-compiler-macro function-differentiator (function-designator)
  `(fndb-record-differentiator (ensure-fndb-record ,function-designator)))
