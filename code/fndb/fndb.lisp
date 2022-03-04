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
          (apply #'reinitialize-instance
                 fnrecord
                 :name function-name
                 kwargs)
          (let ((function (fdefinition function-name))
                (fnrecord (apply #'make-instance 'fnrecord
                                 :name function-name
                                 kwargs)))
            (setf (gethash key table) fnrecord)
            (setf (gethash function (fndb-function-table *fndb*)) fnrecord)
            fnrecord)))))

(defun find-fnrecord (extended-function-designator &optional (errorp t))
  (multiple-value-bind (key table)
      (trivia:ematch extended-function-designator
        ((list 'setf (and name (type non-nil-symbol)))
         (values name (fndb-setf-function-name-table *fndb*)))
        ((and name (type non-nil-symbol))
         (values name (fndb-function-name-table *fndb*)))
        ((and function (type function))
         (values function (fndb-function-table *fndb*)))
        (_ (error "Invalid extended function designator ~S" extended-function-designator)))
    (multiple-value-bind (record present-p)
        (gethash key table)
      (cond (present-p record)
            (errorp (error "There is no fnrecord for ~S" extended-function-designator))
            (t nil)))))

(define-compiler-macro find-fnrecord (&whole form function-designator &optional (errorp t))
  (if (and (constantp function-designator)
           (constantp errorp))
      `(load-time-value
        (locally (declare (notinline find-fnrecord))
          (find-fnrecord ,function-designator ,errorp)))
      form))

(defun function-specializer (function-designator)
  (let ((fnrecord (find-fnrecord function-designator nil)))
    (if (not fnrecord)
        (make-default-specializer function-designator)
        (fnrecord-specializer fnrecord))))

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
