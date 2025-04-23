(in-package #:typo.fndb)

(declaim (ftype (function (function) (values (or null function-name) &optional))
                function-name))
(defun function-name (function)
  (let ((maybe-name (nth-value 2 (function-lambda-expression function))))
    (cond ((typep maybe-name 'function-name)
           maybe-name)
          ((null maybe-name)
           nil))))

(defun function-lambda-list (function &optional (errorp t))
  "Returns the lambda list of FUNCTION, or an approximation thereof."
  (let ((arglist (trivial-arguments:arglist function)))
    (if (eq arglist :unknown)
        (if errorp
            (error "Cannot determine the lambda list of ~S." function)
            '(&rest anything))
        arglist)))

(defun function-ftype (function)
  "Returns an available ftype declaration for the supplied function, or NIL."
  (multiple-value-bind (category local-p declarations)
      (trivial-cltl2:function-information (function-name function))
    (declare (ignore category local-p))
    (values
     (alexandria:assoc-value declarations 'ftype))))

(defun annotated-function-lambda-expression (function)
  "Just like FUNCTION-LAMBDA-EXPRESSION, but with the lambda expression annotated
in a way that incorporates any available FTYPE information."
  (multiple-value-bind (lambda-expression closure-p name)
      (function-lambda-expression function)
    (let ((ftype (function-ftype function)))
      ;; Ensure an ftype is available.
      (unless (and (consp ftype)
                   (eql (first ftype) 'function)
                   (consp (rest ftype))
                   (listp (second ftype)))
        (return-from annotated-function-lambda-expression
          (values lambda-expression closure-p name)))
      ;; Ensure the lambda expression is available and well formed.
      (if (and (consp lambda-expression)
               (eql (first lambda-expression) 'lambda)
               (consp (rest lambda-expression))
               (listp (second lambda-expression)))
          ;; Create the annotated lambda expression.
          (let ((lambda-list (second lambda-expression))
                (body (cddr lambda-expression)))
            (multiple-value-bind (body declarations)
                (alexandria:parse-body body)
              (multiple-value-bind (ftype-declarations values-type)
                  (ftype-declarations-and-values-type ftype lambda-list)
                (values
                 `(lambda ,lambda-list
                    ,@ftype-declarations
                    ,@declarations
                    (the ,values-type (progn ,@body)))
                 closure-p
                 name))))
          ;; Lambda expression is not available, create a dummy lambda
          ;; expression that captures ftype information.

          ;; FIXME: handle the case where `function-lambda-list' is not
          ;; available either
          (let ((lambda-list (function-lambda-list function)))
            (multiple-value-bind (ftype-declarations values-type)
                (ftype-declarations-and-values-type ftype lambda-list)
              (values
               `(lambda ,lambda-list
                  ,@ftype-declarations
                  (the ,values-type %anything%))
               closure-p
               name)))))))

(defun ftype-declarations-and-values-type (ftype lambda-list)
  (multiple-value-bind (treq topt trest tkey taok tkeyp tvalues)
      (parse-ftype-arg-typespec ftype)
    (declare (ignore taok))
    (multiple-value-bind (areq aopt arest akey aaok aaux akeyp)
        (alexandria:parse-ordinary-lambda-list lambda-list)
      (declare (ignore aaok aaux))
      (values
       `(;; required
         ,@(loop for type in treq
                 for argument in areq
                 collect
                 `(declare (type ,type ,argument)))
         ;; &optional
         ,@(loop for type in topt
                 for (argument init) in aopt
                 when (constantp init)
                   collect
                 `(declare (type (or ,type (eql ,(eval init))) ,argument)))
         ;; &key
         ,@(when (and tkeyp akeyp)
             (list
              (loop for (keyword type) in tkey
                    for entry = (find keyword akey :key #'caar)
                    when (and entry (constantp (second entry)))
                      collect
                    `(declare (type (or ,type (eql ,(eval (second entry))))
                                    ,(second (first entry)))))))
         ;; &rest
         ,@(when (and trest arest)
             `((declare (type (or null
                                  (cons ,trest null)
                                  (cons ,trest (cons ,trest null))
                                  (cons ,trest (cons ,trest (cons ,trest null)))
                                  (cons ,trest (cons ,trest (cons ,trest (cons ,trest)))))
                              ,arest)))))
       tvalues))))

(defun parse-ftype-arg-typespec (arg-typespec)
  "Returns the following values:

1. A list with one type specifier per required argument.

2. A list with one type specifier for each optional argument.

3. A type specifier describing the nature of each &rest argument, or NIL.

4. A list with one (keyword type-specifier) entry per keyword argument.

5. A boolean indicating the presence &allow-other-keys.

6. A boolean indicating the presence of &key

7. A values type specifier describing the result of the function."
  ;; The following code is adapted from alexandria:parse-ordinary-lambda-list.
  ;; Assumes ARG-TYPESPEC is a compound function type specifier, as checked by
  ;; `ftype-declarations-and-values-type'.
  (let ((state :required)
        (allow-other-keys nil)
        (required nil)
        (optional nil)
        (rest nil)
        (keys nil)
        (keyp nil))
    (labels ((fail (elt)
               (error "Misplaced ~S in ftype:~%  ~S" elt arg-typespec)))
      (dolist (elt (cadr arg-typespec))
        (case elt
          (&optional
           (if (eq state :required)
               (setf state elt)
               (fail elt)))
          (&rest
           (if (member state '(:required &optional))
               (setf state elt)
               (fail elt)))
          (&key
           (if (member state '(:required &optional :after-rest))
               (setf state elt)
               (fail elt)))
          (&allow-other-keys
           (if (eq state '&key)
               (setf allow-other-keys t
                     state elt)
               (fail elt)))
          (otherwise
           (case state
             (:required
              (push elt required))
             (&optional
              (push elt optional))
             (&rest
              (setf rest elt
                    state :after-rest))
             (&key
              (push elt keys))
             (t
              (error "Invalid ftype:~%  ~S" arg-typespec)))))))
    (let ((result-types (or (caddr arg-typespec) '(values &rest t))))
      ;; Normalize result-types
      (unless (and (consp result-types) (eq (car result-types) 'values))
        (setq result-types `(values ,result-types &rest t)))
      (values (nreverse required) (nreverse optional) rest (nreverse keys)
              allow-other-keys keyp result-types))))

(defun lambda-list-arity (lambda-list)
  "Return two values:

   1. The number of mandatory arguments

   2. The maximal number of permissible arguments"
  (let ((mandatory-arguments 0)
        (max-arguments 0)
        (upper-bound-p t)
        (mandatory-increment 1)
        (max-increment 1))
    (declare (type (integer 0 (#.call-arguments-limit))
                   mandatory-arguments max-arguments
                   mandatory-increment max-increment)
             (type boolean upper-bound-p))
    (dolist (item lambda-list)
      (case item
        ((&key)
         (setf max-increment 2)
         (setf mandatory-increment 0))
        ((&optional)
         (setf max-increment 1)
         (setf mandatory-increment 0))
        ((&aux)
         (setf max-increment 0)
         (setf mandatory-increment 0))
        ((&rest &allow-other-keys #+ccl ccl::&lexpr #+sbcl sb-int:&more)
         (setf max-increment 0)
         (setf mandatory-increment 0)
         (setf upper-bound-p nil))
        (t
         (incf mandatory-arguments mandatory-increment)
         (incf max-arguments max-increment))))
    (if upper-bound-p
        (values mandatory-arguments max-arguments)
        (values mandatory-arguments (1- call-arguments-limit)))))

(defun function-arity (function)
  (lambda-list-arity
   (function-lambda-list function nil)))
