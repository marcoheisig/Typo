(in-package #:typo.fndb)

(defgeneric fnrecordp (object))

(defgeneric fnrecord-name (fnrecord))

(defgeneric fnrecord-function (fnrecord))

(defgeneric fnrecord-function-designator (fnrecord))

(defgeneric fnrecord-min-arguments (fnrecord))

(defgeneric fnrecord-max-arguments (fnrecord))

(defgeneric fnrecord-properties (fnrecord))

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
    :reader fnrecord-function)))

(defmethod print-object ((fnrecord fnrecord) stream)
  (print-unreadable-object (fnrecord stream :type t)
    (format stream "~S ~S"
            (fnrecord-name fnrecord)
            (fnrecord-lambda-list fnrecord))))

(defmethod fnrecordp ((object t)) nil)

(defmethod fnrecordp ((fnrecord fnrecord)) t)

(defmethod fnrecord-function-designator (fnrecord)
  (or (fnrecord-name fnrecord)
      (fnrecord-function fnrecord)))

(defclass minimal-fnrecord (fnrecord)
  ())

(defmethod fnrecord-min-arguments ((minimal-fnrecord minimal-fnrecord))
  0)

(defmethod fnrecord-max-arguments ((minimal-fnrecord minimal-fnrecord))
  (1- call-arguments-limit))

(defmethod fnrecord-lambda-list ((minimal-fnrecord minimal-fnrecord))
  '(&rest rest))

(defmethod fnrecord-specializer ((minimal-fnrecord minimal-fnrecord))
  (make-default-specializer minimal-fnrecord))

(defmethod fnrecord-differentiator ((minimal-fnrecord minimal-fnrecord))
  (make-default-differentiator minimal-fnrecord))

(defmethod fnrecord-properties ((minimal-fnrecord minimal-fnrecord))
  nil)

(defclass full-fnrecord (fnrecord)
  ((%lambda-list
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
   (%properties
    :initarg :properties
    :initform (alexandria:required-argument :properties)
    :type list
    :reader fnrecord-properties)
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

(defun make-default-specializer (fnrecord)
  (lambda (&rest args)
    (wrap-function fnrecord args '() '() (universal-ntype))))

(defun make-default-differentiator (fnrecord)
  (lambda (&rest args)
    (declare (ignore args))
    (error "Don't know the derivative of ~S."
           (or (fnrecord-name fnrecord)
               (fnrecord-function fnrecord)))))

(defmethod shared-initialize
    ((instance full-fnrecord) slot-names
     &rest initargs
     &key
       (name (fnrecord-name instance))
       (function (ensure-fdefinition name))
       (lambda-list (function-lambda-list function))
       (min-arguments (nth-value 0 (lambda-list-arity lambda-list)))
       (max-arguments (nth-value 1 (lambda-list-arity lambda-list)))
       (parent nil parent-supplied-p)
       (parent-fnrecord (if (not parent-supplied-p) nil (find-fnrecord parent)))
       (properties '())
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
         :properties (union properties (when parent-fnrecord (fnrecord-properties parent-fnrecord)))
         :specializer specializer
         :differentiator differentiator
         initargs))

(defun ensure-fdefinition (function-name)
  (declare (function-name function-name))
  (if (fboundp function-name)
      (fdefinition function-name)
      (trivia:match function-name
        ((list 'setf (and name (type symbol)))
         (if (eq (symbol-package name)
                 (find-package "CL"))
             (let* ((package (find-package "TYPO.CL-STUBS"))
                    (new-name (intern (symbol-name name) package)))
               (eval `(progn (declare (inline (setf ,new-name)))
                             (defun (setf ,new-name) (value array index)
                               (setf (,name array index) value))))
               (fdefinition `(setf ,new-name)))
             (trivia.fail:fail)))
        (_ (error "Cannot find fdefinition of ~S." function-name)))))

(defstruct (fndb
            (:predicate fndbp)
            (:constructor make-fndb ()))
  (function-table (trivial-garbage:make-weak-hash-table :test #'eq :weakness :key)
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

(defmethod shared-initialize :after
    ((fnrecord fnrecord) slot-names &rest initargs)
  (declare (ignore slot-names initargs))
  (with-accessors ((function fnrecord-function)
                   (name fnrecord-name)) fnrecord
    (setf (gethash function (fndb-function-table *fndb*))
          fnrecord)
    (unless (not name)
      (multiple-value-bind (key table) (fndb-key-and-table name)
        (setf (gethash key table) fnrecord)))))

(defun find-fnrecord (extended-function-designator &optional (errorp t))
  (multiple-value-bind (key table)
      (fndb-key-and-table extended-function-designator)
    (multiple-value-bind (record present-p)
        (gethash key table)
      (cond (present-p record)
            (errorp (error "There is no fnrecord for ~S" extended-function-designator))
            (t nil)))))

(defun ensure-fnrecord (fnrecord-designator)
  (if (fnrecordp fnrecord-designator)
      fnrecord-designator
      (or (find-fnrecord fnrecord-designator nil)
          (if (functionp fnrecord-designator)
              (make-instance 'minimal-fnrecord
                :name (function-name fnrecord-designator)
                :function fnrecord-designator)
              (make-instance 'minimal-fnrecord
                :name fnrecord-designator
                :function
                (if (and (fboundp fnrecord-designator)
                         (functionp (fdefinition fnrecord-designator)))
                    (fdefinition fnrecord-designator)
                    (lambda (&rest args)
                      (declare (ignore args))
                      (error "Call to unbound function ~S."
                             fnrecord-designator))))))))

(define-compiler-macro ensure-fnrecord (&whole form fnrecord-designator)
  (if (constantp fnrecord-designator)
      `(load-time-value
        (locally (declare (notinline ensure-fnrecord))
          (ensure-fnrecord ,fnrecord-designator)))
      form))

(defun update-fnrecord
    (function-name &rest kwargs &key &allow-other-keys)
  (check-type function-name function-name)
  (multiple-value-bind (key table)
      (trivia:match function-name
        ((list 'setf (and function-name (type non-nil-symbol)))
         (values function-name (fndb-setf-function-name-table *fndb*)))
        ((and function-name (type non-nil-symbol))
         (values function-name (fndb-function-name-table *fndb*)))
        (_ (error "Invalid function name: ~S"
                  function-name)))
    (multiple-value-bind (fnrecord present-p)
        (gethash key table)
      (if present-p
          (if (typep fnrecord 'full-fnrecord)
              (apply #'reinitialize-instance fnrecord
                     :name function-name
                     kwargs)
              (apply #'change-class fnrecord 'full-fnrecord
                     :name function-name
                     kwargs))
          (apply #'make-instance 'full-fnrecord
                 :name function-name
                 kwargs)))))

(defun fndb-key-and-table (extended-function-designator)
  (trivia:ematch extended-function-designator
    ((list 'setf (and name (type non-nil-symbol)))
     (values name (fndb-setf-function-name-table *fndb*)))
    ((and name (type non-nil-symbol))
     (values name (fndb-function-name-table *fndb*)))
    ((and function (type function))
     (values function (fndb-function-table *fndb*)))
    (_ (error "Invalid extended function designator ~S" extended-function-designator))))

(defun function-specializer (function-designator)
  (fnrecord-specializer (ensure-fnrecord function-designator)))

(define-compiler-macro function-specializer (&whole form function-designator)
  (if (constantp function-designator)
      `(fnrecord-specializer
        (load-time-value
         (ensure-fnrecord ,function-designator)))
      form))

(defun function-differentiator (function-designator)
  (let ((fnrecord (find-fnrecord function-designator nil)))
    (if (not fnrecord)
        (make-default-differentiator function-designator)
        (fnrecord-differentiator fnrecord))))

(define-compiler-macro function-differentiator (&whole form function-designator)
  (if (constantp function-designator)
      `(fnrecord-differentiator
        (load-time-value
         (ensure-fnrecord ,function-designator)))
      form))
