(in-package #:typo.fndb)

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

(defun ensure-fnrecord
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
          (if (typep fnrecord 'forward-referenced-fnrecord)
              (apply #'change-class fnrecord 'fnrecord
                     :name function-name
                     kwargs)
              (apply #'reinitialize-instance fnrecord
                     :name function-name
                     kwargs))
          (let ((function (fdefinition function-name))
                (fnrecord (apply #'make-instance 'fnrecord
                                 :name function-name
                                 kwargs)))
            (setf (gethash key table) fnrecord)
            (setf (gethash function (fndb-function-table *fndb*)) fnrecord)
            fnrecord)))))

(defun fndb-key-and-table (extended-function-designator)
  (trivia:ematch extended-function-designator
    ((list 'setf (and name (type non-nil-symbol)))
     (values name (fndb-setf-function-name-table *fndb*)))
    ((and name (type non-nil-symbol))
     (values name (fndb-function-name-table *fndb*)))
    ((and function (type function))
     (values function (fndb-function-table *fndb*)))
    (_ (error "Invalid extended function designator ~S" extended-function-designator))))

(defun find-fnrecord (extended-function-designator &optional (errorp t))
  (multiple-value-bind (key table)
      (fndb-key-and-table extended-function-designator)
    (multiple-value-bind (record present-p)
        (gethash key table)
      (cond (present-p record)
            (errorp (error "There is no fnrecord for ~S" extended-function-designator))
            (t nil)))))

(defun (setf find-fnrecord) (value extended-function-designator &optional (errorp t))
  (declare (ignore errorp))
  (multiple-value-bind (key table)
      (fndb-key-and-table extended-function-designator)
    (setf (gethash key table) value)))

(defun forward-reference-fnrecord (function-name)
  (or (find-fnrecord function-name nil)
      (setf (find-fnrecord function-name)
            (make-instance 'forward-referenced-fnrecord))))

(defun function-specializer (function-designator)
  (let ((fnrecord (find-fnrecord function-designator nil)))
    (if (not fnrecord)
        (make-default-specializer function-designator)
        (fnrecord-specializer fnrecord))))

(define-compiler-macro function-specializer (&whole form function-designator)
  (if (constantp function-designator)
      `(fnrecord-specializer
        (load-time-value
         (forward-reference-fnrecord ,function-designator)))
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
         (forward-reference-fnrecord ,function-designator)))
      form))
