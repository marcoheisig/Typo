(in-package #:typo.fndb)

(defun specialize
    (function wrappers
     &key
       wrap-constant
       wrap-function
       wrapper-nth-value-ntype
       (wrapper-ntype (lambda (w) (funcall wrapper-nth-value-ntype 0 w))))
  "Returns a wrapper that encapsulates the information of calling FUNCTION
with arguments that are described by the supplied wrappers.  The exact
nature of a wrapper depends on the supplied WRAP* functions.

Arguments:

FUNCTION - A function designator, or the name of a special operator.

WRAPPERS - A list of one wrapper per argument.  A wrapper is an arbitrary
object that can be processed by WRAPPER-NTYPE and WRAPPER-NTH-VALUE-NTYPE.

WRAP-CONSTANT - A function that takes an object and returns a wrapper
around this object.

WRAP-FUNCTION - A function that is invoked with a first argument that is an
fnrecord, a second argument that is a list of wrapped objects, a third
argument that is a list of ntypes of arguments that will definitely be
returned, a fourth argument that is a list of ntypes of its optional
values, and an ntype of all rest values or NIL if there are no rest values.

WRAPPER-NTYPE - A function that takes a wrapper and returns the ntype of
its first argument.

WRAPPER-NTH-VALUE-NTYPE - A function that takes an index N and a wrapper W
and returns the ntype of the Nth value of W.

May signal an error of type WRONG-NUMBER-OF-ARGUMENTS or INVALID-ARGUMENTS
when the number or type of the supplied WRAPPERS is not suitable for the
supplied FUNCTION.
"
  (let ((*wrap-constant* wrap-constant)
        (*wrap-function* wrap-function)
        (*wrapper-nth-value-ntype* wrapper-nth-value-ntype)
        (*wrapper-ntype* wrapper-ntype)
        (fnrecord (ensure-fnrecord function)))
    (handler-case (apply (fnrecord-specializer fnrecord) wrappers)
      (program-error (e)
        (if (not (<= (fnrecord-min-arguments fnrecord)
                     (length wrappers)
                     (fnrecord-max-arguments fnrecord)))
            (error 'wrong-number-of-arguments
                   :function function
                   :arguments wrappers)
            (error e))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Derived Functionality

(defun infer-ntypes (function ntypes)
;;; We represent wrappers as a (REQUIRED OPTIONAL REST) list.
  (flet ((wrapper-nth-value-ntype (index wrapper)
           (trivia:ematch wrapper
             ((list required optional rest)
              (let ((n-required (length required)))
                (if (< index n-required)
                    (nth index required)
                    (let ((n-optional (length optional)))
                      (if (< index (+ n-required n-optional))
                          (nth (- index n-required) optional)
                          (if (null rest)
                              (type-specifier-ntype 'null)
                              rest))))))))
         (wrap-constant (c)
           (list (list (ntype-of c)) '() nil))
         (wrap-function (fnrecord wrappers required optional rest)
           (declare (ignore fnrecord wrappers))
           (list required optional rest)))
    (destructuring-bind (required optional rest)
        (specialize
         function
         (loop for ntype in ntypes collect (list (list ntype) '() nil))
         :wrap-constant #'wrap-constant
         :wrap-function #'wrap-function
         :wrapper-nth-value-ntype #'wrapper-nth-value-ntype)
      (values required optional rest))))

