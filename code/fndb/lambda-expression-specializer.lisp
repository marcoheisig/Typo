(in-package #:typo.fndb)

;;; The Common Lisp function FUNCTION-LAMBDA-EXPRESSION is a portable interface
;;; for querying the definition (including the lambda list) of functions and
;;; their name.  Even though the kinds of functions it applies to is
;;; implementation-dependent, whenever it does return a meaningful lambda
;;; expression, we can use that expression to turn minimal fnrecords into full
;;; ones.
;;;
;;; Most excitingly, we can use function lambda expressions to automatically
;;; derive a specializer for that function.  When deriving a specializer, we
;;; first attempt to derive a specializer that will inline the entire lambda
;;; expression into each call site, and, if that fails, a specializer that
;;; infers the result types as good as possible.
;;;
;;; Deriving a specializer from a supplied expression technically requires not
;;; just a code walker, but an entire Common Lisp compiler that solves data
;;; flow equations.  However, for now, we use simple heuristics only and fall
;;; back to the trivial solution when encountering any difficulties.

(defmethod initialize-instance :after ((fnrecord minimal-fnrecord) &key &allow-other-keys)
  "Attempt to turn the minimal fnrecord into a full fnrecord via introspection."
  (let ((function (fnrecord-function fnrecord)))
    ;; Attempt to obtain the function's lambda expression
    (multiple-value-bind (lambda-expression closure-p maybe-name)
        (function-lambda-expression function)
      (declare (ignore closure-p))
      (unless (and (consp lambda-expression)
                   (eql (first lambda-expression) 'lambda)
                   (listp (second lambda-expression)))
        (return-from initialize-instance nil))
      ;; Derive the function's name, lambda list, and body
      (let ((name (or (fnrecord-name fnrecord)
                      (and (typep maybe-name 'function-name)
                           maybe-name)))
            (lambda-list (second lambda-expression)))
        (multiple-value-bind (min-arguments max-arguments)
            (lambda-list-arity lambda-list)
          ;; Turn into a full fnrecord
          (change-class fnrecord 'full-fnrecord
            :name name
            :lambda-list lambda-list
            :min-arguments min-arguments
            :max-arguments max-arguments
            :specializer (lambda-expression-specializer fnrecord lambda-expression)))))))

(defstruct (les-wrapper
            (:copier nil)
            (:predicate les-wrapper-p)
            (:constructor make-les-wrapper))
  "The lambda expression specializer wrapper"
  ;; A description of the multiple values returned by that wrapper.
  (values-ntype nil :type values-ntype)
  ;; A wrapper in the representation used by the caller of the lambda function,
  ;; or the symbol NO-ENCAPSULATED-WRAPPER.
  (encapsulated-wrapper 'no-encapsulated-wrapper))

(defmacro with-encapsulated-wrappers (&body body)
  `(call-with-encapsulated-wrappers (lambda () ,@body)))

(defun call-with-encapsulated-wrappers (thunk)
  (let ((encapsulated-wrap-constant *wrap-constant*)
        (encapsulated-wrap-function *wrap-function*)
        (encapsulated-nth-value *wrapper-nth-value*)
        (encapsulated-wrapper-ntype *wrapper-ntype*))
    (labels ((wrap-constant (constant)
               (make-les-wrapper
                :values-ntype (make-single-value-values-ntype (ntype-of constant))
                :encapsulated-wrapper (funcall encapsulated-wrap-constant constant)))
             (wrap-function (fnrecord wrappers required optional rest)
               (make-les-wrapper
                :values-ntype (make-values-ntype required optional rest)
                :encapsulated-wrapper
                (let ((encapsulated (mapcar #'les-wrapper-encapsulated-wrapper wrappers)))
                  (if (find 'no-encapsulated-wrapper encapsulated)
                      'no-encapsulated-wrapper
                       (funcall encapsulated-wrap-function fnrecord encapsulated required optional rest)))))
             (wrapper-nth-value (n wrapper)
               (let ((encapsulated-wrapper (les-wrapper-encapsulated-wrapper wrapper))
                     (values-ntype (les-wrapper-values-ntype wrapper)))
                 (if (and (zerop n)
                          (= 1 (values-ntype-minimum-number-of-values values-ntype))
                          (not (values-ntype-rest-ntype values-ntype)))
                     wrapper
                     (if (eql encapsulated-wrapper 'no-encapsulated-wrapper)
                         (make-les-wrapper
                          :values-ntype
                          (make-single-value-values-ntype
                           (values-ntype-nth-value-ntype n values-ntype)))
                         (let ((w (funcall encapsulated-nth-value n encapsulated-wrapper)))
                           (make-les-wrapper
                            :encapsulated-wrapper w
                            :values-ntype
                            (make-single-value-values-ntype
                             (funcall encapsulated-wrapper-ntype w))))))))
             (wrapper-ntype (wrapper)
               (values-ntype-nth-value-ntype 0 (les-wrapper-values-ntype wrapper))))
      (let ((*wrap-constant* #'wrap-constant)
            (*wrap-function* #'wrap-function)
            (*wrapper-nth-value* #'wrapper-nth-value)
            (*wrapper-ntype* #'wrapper-ntype))
        (funcall thunk)))))

(defun ensure-les-wrapper (wrapper)
  (if (les-wrapper-p wrapper)
      wrapper
      (make-les-wrapper
       :values-ntype (make-single-value-values-ntype (wrapper-ntype wrapper))
       :encapsulated-wrapper wrapper)))

(defun unpack-les-wrapper (les-wrapper fnrecord arguments)
  (let ((encapsulated-wrapper (les-wrapper-encapsulated-wrapper les-wrapper)))
    (if (not (eq encapsulated-wrapper 'no-encapsulated-wrapper))
        encapsulated-wrapper
        (let* ((values-ntype (les-wrapper-values-ntype les-wrapper))
               (nrequired (values-ntype-minimum-number-of-values values-ntype))
               (nnonrest (values-ntype-number-of-optional-values values-ntype)))
          (wrap-function
           fnrecord
           arguments
           (loop for index below nrequired
                 collect (values-ntype-nth-value-ntype index values-ntype))
           (loop for index from nrequired below nnonrest
                 collect (values-ntype-nth-value-ntype index values-ntype))
           (values-ntype-rest-ntype values-ntype))))))

(define-condition give-up (serious-condition)
  ()
  (:documentation "Signaled to give up lambda expression type inference."))

(defun lambda-expression-specializer (fnrecord lambda-expression)
  "Returns a suitable specializer for the supplied lambda expression."
  ;; Abort if the argument is not a lambda expression.
  (unless (and (listp lambda-expression)
                 (eql (first lambda-expression) 'lambda)
                 (listp (second lambda-expression))
                 ;; Only handle fixed-arg lambda lists for now.
                 (every #'symbolp (second lambda-expression))
                 (null (intersection (second lambda-expression) lambda-list-keywords)))
    (return-from lambda-expression-specializer
      (fnrecord-specializer fnrecord)))
  (labels
      ((process (form env)
         #+(or) (format *trace-output* "~&Processing ~S~%" form)
         ;; dispatch on the form
         (cond ((atom form)
                (process-atom form env))
               ((and (consp form)
                     (symbolp (first form)))
                (process-compound-form (first form) (rest form) env))
               (t (error 'give-up))))
       (process-progn (forms env)
         (cond ((null forms)
                (wrap-constant nil))
               ((null (rest forms))
                (process (first forms) env))
               (t
                (process (first forms) env)
                (process-progn (rest forms) env))))
       (process-atom (form env)
         (if (constantp form)
             (wrap-constant (eval form))
             (if (symbolp form)
                 (let ((entry (assoc form env)))
                   (if (not entry)
                       (error 'give-up)
                       (cdr entry))))))
       (process-compound-form (symbol rest env)
         (case symbol
           ;; IF special forms
           ((if)
            (multiple-value-bind (test then else)
                (case (length rest)
                  (2 (values (first rest) (second rest) nil))
                  (3 (values-list rest))
                  (otherwise (error 'give-up)))
              (let ((test-wrapper (process test env)))
                (ntype-subtypecase (wrapper-ntype test-wrapper)
                  ((not null)
                   (process then env))
                  (null
                  (process else env))
                  (t
                   (handler-case
                       (make-les-wrapper
                        :values-ntype
                        (values-ntype-union
                         (les-wrapper-values-ntype (process then env))
                         (les-wrapper-values-ntype (process else env))))
                     (error () (error 'give-up))))))))
           ;; FUNCTION special forms
           ((function)
            (unless (and (= 1 (length rest))
                         (typep (first rest) 'function-designator))
              (error 'give-up))
            (wrap-constant (fdefinition (first rest))))
           ;; THE special forms
           ((the)
            (unless (= 2 (length rest))
              (error 'give-up))
            ;; TODO add type information
            (process (second rest) env))
           ;; LET special forms
           ((let)
            (unless (and (<= 1 (length rest))
                         (listp (first rest))
                         (every
                          (lambda (binding)
                            (typep binding
                                   '(or non-nil-symbol
                                     (cons non-nil-symbol (cons t null)))))
                          (first rest)))
              (error 'give-up))
            (let ((env-entries
                    (mapcar
                     (lambda (binding)
                       (if (symbolp binding)
                           (cons binding (wrap-constant nil))
                           (cons (first binding)
                                 (process (second binding) env))))
                     (first rest))))
              (process-progn (alexandria:parse-body (cdr rest)) (append env-entries env))))
           ;; BLOCK special forms
           ((block) (process-progn (cdr rest) env))
           ;; MULTIPLE-VALUE-BIND special forms
           ((multiple-value-bind)
            (unless (and (<= 2 (length rest))
                         (listp (first rest))
                         (every (lambda (x)
                                  (and (symbolp x)
                                       (not (null x))))
                                (first rest)))
              (error 'give-up))
            (let ((values-wrapper (process (second rest) env)))
              (process-progn
               (alexandria:parse-body (cddr rest))
               (append
                (loop for symbol in (first rest)
                      for n from 0
                      collect (cons symbol (wrapper-nth-value n values-wrapper)))
                env))))
           (otherwise
            (cond
              ;; macros
              ((macro-function symbol)
               (multiple-value-bind (expansion expanded-p)
                   (macroexpand-1 `(,symbol ,@rest))
                 (if expanded-p
                     (process expansion env)
                     (error 'give-up))))
              ;; function calls
              ((fboundp symbol)
               (apply (function-specializer symbol)
                      (mapcar (lambda (x) (process x env)) rest)))
              (t (error 'give-up)))))))
    (let ((lambda-list (second lambda-expression))
          ;; Use alexandria to skip declarations.
          (body (alexandria:parse-body (rest (rest lambda-expression)))))
      (lambda (&rest args)
        (unless (= (length args)
                   (length lambda-list))
          (%abort-specialization (fnrecord-function fnrecord) args))
        (let* ((wrapped-args (mapcar #'ensure-les-wrapper args))
               (env (mapcar #'cons lambda-list wrapped-args)))
          (handler-case
              (unpack-les-wrapper
               (with-encapsulated-wrappers
                 (process-progn body env))
               fnrecord args)
            (give-up ()
              (wrap-function fnrecord args '() '() (universal-ntype)))))))))
