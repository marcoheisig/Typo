(in-package #:typo.ntype)

(defmethod type-specifier-ntype (type-specifier)
  (multiple-value-bind (expansion expansionp)
      (introspect-environment:typexpand-1 type-specifier)
    (if expansionp
        (type-specifier-ntype expansion)
        (find-primitive-ntype type-specifier))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Atomic Type Specifiers

(defvar *atomic-type-specifier-table*
  (make-hash-table :test #'eq))

(setf (gethash 'null *atomic-type-specifier-table*)
      (list (make-eql-ntype nil) t))

(setf (gethash 'array *atomic-type-specifier-table*)
      (multiple-value-list
       (make-array-ntype)))

(setf (gethash 'simple-array *atomic-type-specifier-table*)
      (multiple-value-list
       (make-array-ntype
        :simplep t)))

(setf (gethash 'vector *atomic-type-specifier-table*)
      (multiple-value-list
       (make-array-ntype
        :dimensions 1)))

(setf (gethash 'simple-vector *atomic-type-specifier-table*)
      (multiple-value-list
       (make-array-ntype
        :element-type 't
        :dimensions 1
        :simplep t)))

(setf (gethash 'bit-vector *atomic-type-specifier-table*)
      (multiple-value-list
       (make-array-ntype
        :element-type 'bit
        :dimensions 1)))

(setf (gethash 'simple-bit-vector *atomic-type-specifier-table*)
      (multiple-value-list
       (make-array-ntype
        :element-type 'bit
        :dimensions 1
        :simplep t)))

(setf (gethash 'string *atomic-type-specifier-table*)
      (multiple-value-list
       (make-array-ntype
        :element-type 'character
        :dimensions 1)))

(setf (gethash 'simple-string *atomic-type-specifier-table*)
      (multiple-value-list
       (make-array-ntype
        :element-type 'character
        :dimensions 1
        :simplep t)))

(defmethod type-specifier-ntype :around ((atomic-type-specifier symbol))
  (values-list
   (alexandria:ensure-gethash
    atomic-type-specifier
    *atomic-type-specifier-table*
    (multiple-value-list
     (call-next-method)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class as Type Specifiers

(defvar *class-type-specifier-table*
  (trivial-garbage:make-weak-hash-table
   :test #'eq
   ;; Retain the ntype as long as the class exists.
   :weakness :key))

(defmethod type-specifier-ntype :around ((class class))
  (values-list
   (alexandria:ensure-gethash
    class
    *class-type-specifier-table*
    (multiple-value-list
     (call-next-method)))))

(defmethod type-specifier-ntype ((class class))
  (if (subtypep class 'array)
      (make-array-ntype
       :simplep (subtypep class 'simple-array)
       :dimensions (if (subtypep class 'vector) '(*) '*)
       :element-type (array-element-type (class-prototype class)))
      (find-primitive-ntype class)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compound Type Specifiers

(defgeneric compound-type-specifier-ntype (first rest whole))

(defmethod type-specifier-ntype ((compound-type-specifier cons))
  (compound-type-specifier-ntype
   (first compound-type-specifier)
   (rest compound-type-specifier)
   compound-type-specifier))

(defmethod compound-type-specifier-ntype (first rest whole)
  (multiple-value-bind (expansion expansionp)
      (introspect-environment:typexpand-1 whole)
    (if expansionp
        (type-specifier-ntype expansion)
        (find-primitive-ntype whole))))

;;; Set Theoretic Type Specifiers

(defmethod compound-type-specifier-ntype ((_ (eql 'eql)) rest whole)
  (trivia:match rest
    ((list object)
     (values (make-eql-ntype object) t))
    (_ (call-next-method))))

(defmethod compound-type-specifier-ntype ((_ (eql 'member)) rest whole)
  (let ((ntype (empty-ntype))
        (ntype-precise-p t))
    (dolist (object rest)
      (multiple-value-bind (union union-precise-p)
          (ntype-union ntype (make-eql-ntype object))
        (setf ntype union)
        (setf ntype-precise-p (and ntype-precise-p union-precise-p))))
    (values ntype ntype-precise-p)))

(defmethod compound-type-specifier-ntype ((_ (eql 'not)) rest whole)
  (trivia:match rest
    ((list type-specifier)
     (multiple-value-bind (ntype precise-p)
         (type-specifier-ntype type-specifier)
       (cond ((not precise-p)
              (values (universal-ntype) nil))
             ((eql ntype (universal-ntype))
              (values (empty-ntype) t))
             ((eql ntype (empty-ntype))
              (values (universal-ntype) t))
             ((eql ntype (true-ntype))
              (values (false-ntype) t))
             ((eql ntype (false-ntype))
              (values (true-ntype) t))
             ((eql ntype (make-eql-ntype nil))
              (values (true-ntype) t))
             (t
              (values (universal-ntype) nil)))))
    (_ (call-next-method))))

(defmethod compound-type-specifier-ntype ((_ (eql 'or)) rest whole)
  (let ((ntype (empty-ntype))
        (ntype-precise-p t))
    (dolist (type-specifier rest)
      (multiple-value-bind (other-ntype other-ntype-precise-p)
          (type-specifier-ntype type-specifier)
        (multiple-value-bind (union union-precise-p)
            (ntype-union ntype other-ntype)
          (setf ntype union)
          (unless (and other-ntype-precise-p union-precise-p)
            (setf ntype-precise-p nil)))))
    (values ntype ntype-precise-p)))

(defmethod compound-type-specifier-ntype ((_ (eql 'satisfies)) rest whole)
  (values (universal-ntype) t))

(defmethod compound-type-specifier-ntype ((_ (eql 'and)) rest whole)
  (let ((ntype (universal-ntype))
        (ntype-precise-p t))
    (dolist (type-specifier rest)
      (multiple-value-bind (other-ntype other-ntype-precise-p)
          (type-specifier-ntype type-specifier)
        (multiple-value-bind (intersection intersection-precise-p)
            (ntype-intersection ntype other-ntype)
          (setf ntype intersection)
          (unless (and other-ntype-precise-p intersection-precise-p)
            (setf ntype-precise-p nil)))))
    (values ntype ntype-precise-p)))

;;; Number Type Specifiers

(defmacro search-interval-ntype (number-type expression)
  (alexandria:with-gensyms (lower-bound upper-bound)
    (alexandria:once-only (expression)
      `(trivia:match ,expression
         ((or (list ',number-type '* '*)
              (list ',number-type '*)
              (list ',number-type))
          (find-primitive-ntype ',number-type))
         ((list ',number-type
                 (and ,lower-bound (type ,number-type))
                 (and ,upper-bound (type ,number-type)))
          (if (eql ,lower-bound ,upper-bound)
              (values (make-eql-ntype ,lower-bound) t)
              (if (< ,lower-bound ,upper-bound)
                  (values (find-primitive-ntype ',number-type) nil)
                  (trivia.fail:fail))))
         ((list ',number-type
                 (or '* (type ,number-type) (list (type ,number-type)))
                 (or '* (type ,number-type) (list (type ,number-type))))
          (values (find-primitive-ntype ',number-type) nil))
         (_ (call-next-method))))))

(defmethod compound-type-specifier-ntype ((_ (eql 'short-float)) rest whole)
  (search-interval-ntype short-float whole))

(defmethod compound-type-specifier-ntype ((_ (eql 'single-float)) rest whole)
  (search-interval-ntype single-float whole))

(defmethod compound-type-specifier-ntype ((_ (eql 'double-float)) rest whole)
  (search-interval-ntype double-float whole))

(defmethod compound-type-specifier-ntype ((_ (eql 'long-float)) rest whole)
  (search-interval-ntype long-float whole))

(defmethod compound-type-specifier-ntype ((_ (eql 'float)) rest whole)
  (search-interval-ntype float whole))

(defmethod compound-type-specifier-ntype ((_ (eql 'real)) rest whole)
  (search-interval-ntype real whole))

(defmethod compound-type-specifier-ntype ((_ (eql 'rational)) rest whole)
  (search-interval-ntype rational whole))

(defmethod compound-type-specifier-ntype ((_ (eql 'unsigned-byte)) rest whole)
  (trivia:match rest
    ((or (list) (list '*))
     (find-primitive-ntype 'unsigned-byte))
    ((list (and bits (type (integer 1))))
     (find-unsigned-byte-ntype bits))
    (_ (call-next-method))))

(defun find-unsigned-byte-ntype (bits)
  (macrolet ((body ()
               `(cond
                  ,@(loop for primitive-ntype across *primitive-ntypes*
                          for type = (primitive-ntype-type-specifier primitive-ntype)
                          when (and (consp type)
                                    (= 2 (length type))
                                    (subtypep type 'unsigned-byte))
                            collect `((<= bits ,(second type))
                                      (values (find-primitive-ntype ',type)
                                              (= bits ,(second type)))))
                  (t (values (find-primitive-ntype 'unsigned-byte) nil)))))
    (body)))

(defmethod compound-type-specifier-ntype ((_ (eql 'signed-byte)) rest whole)
  (trivia:match rest
    ((or (list) (list '*))
     (find-primitive-ntype 'signed-byte))
    ((list (and bits (type (integer 1))))
     (find-signed-byte-ntype bits))
    (_ (call-next-method))))

(defun find-signed-byte-ntype (bits)
  (macrolet ((body ()
               `(cond
                  ,@(loop for primitive-ntype across *primitive-ntypes*
                          for type = (primitive-ntype-type-specifier primitive-ntype)
                          when (eq type 'fixnum)
                            collect
                          `((<= bits ,(integer-length most-positive-fixnum))
                            (values (find-primitive-ntype 'fixnum)
                                    (= bits +fixnum-bits+)))
                          when (trivia:match type
                                 ((list 'signed-byte _) t)
                                 (_ nil))
                            collect
                          `((<= bits ,(second type))
                            (values (find-primitive-ntype ',type)
                                    (= bits ,(second type)))))
                  (t (values (find-primitive-ntype 'signed-byte) nil)))))
    (body)))

(defmethod compound-type-specifier-ntype ((_ (eql 'integer)) rest whole)
  (trivia:match rest
    ;; Two unknown bounds.
    ((or (list) (list '*) (list '* '*))
     (values (find-primitive-ntype 'integer) t))
    ;; Two known bounds.
    ((list (and lower-bound (or (type integer) (list (type integer))))
           (and upper-bound (or (type integer) (list (type integer)))))
     (when (listp lower-bound)
       (setf lower-bound (1+ (first lower-bound))))
     (when (listp upper-bound)
       (setf upper-bound (1- (first upper-bound))))
     (find-integer-ntype lower-bound upper-bound whole))
    ;; A known lower bound.
    ((list (and lower-bound (or (type integer) (list (type integer)))))
     (when (listp lower-bound)
       (setf lower-bound (1+ (first lower-bound))))
     (cond ((= lower-bound 0)
            (find-primitive-ntype 'unsigned-byte))
           ((> lower-bound 0)
            (values (find-primitive-ntype 'unsigned-byte) nil))
           ((< lower-bound 0)
            (values (find-primitive-ntype 'signed-byte) nil))))
    ;; Any other valid case
    ((or (list (or '* (type integer) (list (type integer))))
         (list (or '* (type integer) (list (type integer)))
               (or '* (type integer) (list (type integer)))))
     (values (find-primitive-ntype 'integer) nil))
    (_ (call-next-method))))

(defun find-integer-ntype (lb ub whole)
  (declare (integer lb ub))
  (unless (< lb ub)
    (error "Malformed integer type specifier: ~S" whole))
  (if (= lb ub)
      (make-eql-ntype lb)
      (if (minusp lb)
          ;; Signed Integers
          (if (minusp ub)
              (values (find-signed-byte-ntype (integer-length lb)) nil)
              (let ((lb-bits (integer-length lb))
                    (ub-bits (integer-length ub)))
                (multiple-value-bind (ntype precise-p)
                    (find-signed-byte-ntype (max lb-bits ub-bits))
                  (values ntype
                          (and precise-p
                               (< lb-bits (integer-length (1- lb)))
                               (< ub-bits (integer-length (1+ ub))))))))
          ;; Unsigned Integers
          (let ((ub-bits (integer-length ub)))
            (multiple-value-bind (ntype precise-p)
                (find-unsigned-byte-ntype ub-bits)
              (values ntype
                      (and precise-p
                           (zerop lb)
                           (< ub-bits (integer-length (1+ ub))))))))))

(defmethod compound-type-specifier-ntype ((_ (eql 'mod)) rest whole)
  (trivia:match rest
    ((list)
     (find-primitive-ntype 'unsigned-byte))
    ((list (and n (type (integer 1))))
     (find-integer-ntype 0 (1- n) whole))
    (_ (call-next-method))))

(defmethod compound-type-specifier-ntype ((_ (eql 'complex)) rest whole)
  (trivia:match rest
    ((list '*)
     (find-primitive-ntype 'complex))
    ((list type-specifier)
     (multiple-value-bind (part-ntype part-precise-p)
         (type-specifier-ntype type-specifier)
       (multiple-value-bind (complex-ntype complex-precise-p)
           (make-complex-ntype part-ntype)
         (values complex-ntype (and part-precise-p complex-precise-p)))))
    (_ (call-next-method))))

(let ((v1-cache (make-array (list +primitive-ntype-limit+)
                            :element-type 'ntype
                            :initial-element (empty-ntype)))
      (v2-cache (make-array (list +primitive-ntype-limit+)
                            :element-type 'bit
                            :initial-element 1)))
  (loop for p across *primitive-ntypes* do
    (when (ntype-subtypep p (find-primitive-ntype 'real))
      (multiple-value-bind (complex-ntype precise-p)
          (find-primitive-ntype `(complex ,(ntype-type-specifier p)))
        (setf (aref v1-cache (ntype-index p))
              complex-ntype)
        (setf (aref v2-cache (ntype-index p))
              (if precise-p 1 0)))))
  (defun make-complex-ntype (part-ntype)
    (declare (ntype part-ntype))
    (values
     (aref v1-cache (ntype-index part-ntype))
     (if (and (primitive-ntype-p part-ntype)
              (plusp (aref v2-cache (ntype-index part-ntype))))
         t nil))))


;;; Array Type Specifiers

(defmethod compound-type-specifier-ntype ((_ (eql 'array)) rest whole)
  (trivia:match rest
    ((or (and (list) (<> element-type '*) (<> dimensions '*))
         (and (list element-type) (<> dimensions '*))
         (and (list element-type dimensions)))
     (make-array-ntype
      :element-type element-type
      :dimensions dimensions))
    (_ (call-next-method))))

(defmethod compound-type-specifier-ntype ((_ (eql 'simple-array)) rest whole)
  (trivia:match rest
    ((or (and (list) (<> element-type '*) (<> dimensions '*))
         (and (list element-type) (<> dimensions '*))
         (and (list element-type dimensions)))
     (make-array-ntype
      :element-type element-type
      :dimensions dimensions
      :simplep t))
    (_ (call-next-method))))

(defmethod compound-type-specifier-ntype ((_ (eql 'vector)) rest whole)
  (trivia:match rest
    ((or (and (list) (<> element-type '*) (<> size '*))
         (and (list element-type) (<> size '*))
         (and (list element-type (and size (or '* (type (and fixnum unsigned-byte)))))))
     (make-array-ntype
      :element-type element-type
      :dimensions (list size)))
    (_ (call-next-method))))

(defmethod compound-type-specifier-ntype ((_ (eql 'bit-vector)) rest whole)
  (trivia:match rest
    ((or (and (list) (<> size '*))
         (and (list (and size (or '* (type (and fixnum unsigned-byte)))))))
     (make-array-ntype
      :element-type (find-primitive-ntype 'bit)
      :dimensions (list size)))
    (_ (call-next-method))))

(defmethod compound-type-specifier-ntype ((_ (eql 'simple-bit-vector)) rest whole)
  (trivia:match rest
    ((or (and (list) (<> size '*))
         (and (list (and size (or '* (type (and fixnum unsigned-byte)))))))
     (make-array-ntype
      :element-type (find-primitive-ntype 'bit)
      :dimensions (list size)
      :simplep t))
    (_ (call-next-method))))

(defmethod compound-type-specifier-ntype ((_ (eql 'simple-vector)) rest whole)
  (trivia:match rest
    ((or (and (list) (<> size '*))
         (and (list (and size (or '* (type (and fixnum unsigned-byte)))))))
     (make-array-ntype
      :element-type (find-primitive-ntype 't)
      :dimensions (list size)
      :simplep t))
    (_ (call-next-method))))

(defmethod compound-type-specifier-ntype ((_ (eql 'string)) rest whole)
  (trivia:match rest
    ((or (and (list) (<> size '*))
         (and (list (and size (or '* (type (and fixnum unsigned-byte)))))))
     (make-array-ntype
      :element-type (find-primitive-ntype 'character)
      :dimensions (list size)))
    (_ (call-next-method))))

(defmethod compound-type-specifier-ntype ((_ (eql 'simple-string)) rest whole)
  (trivia:match rest
    ((or (and (list) (<> size '*))
         (and (list (and size (or '* (type (and fixnum unsigned-byte)))))))
     (make-array-ntype
      :element-type (find-primitive-ntype 'character)
      :dimensions (list size)
      :simplep t))
    (_ (call-next-method))))

(defmethod compound-type-specifier-ntype ((_ (eql 'base-string)) rest whole)
  (trivia:match rest
    ((or (and (list) (<> size '*))
         (and (list (and size (or '* (type (and fixnum unsigned-byte)))))))
     (make-array-ntype
      :element-type 'base-string
      :dimensions size))
    (_ (call-next-method))))

(defmethod compound-type-specifier-ntype ((_ (eql 'simple-base-string)) rest whole)
  (trivia:match rest
    ((or (and (list) (<> size '*))
         (and (list (and size (or '* (type (and fixnum unsigned-byte)))))))
     (make-array-ntype
      :element-type 'base-string
      :dimensions size
      :simplep t))
    (_ (call-next-method))))

;;; Miscellaneous Compound Type Specifiers

(defmethod compound-type-specifier-ntype ((_ (eql 'cons)) rest whole)
  (trivia:match rest
    ((or (and (list) (<> car '*) (<> cdr '*))
         (and (list car) (<> cdr '*))
         (list car cdr))
     (multiple-value-bind (car-ntype a)
         (if (eql car '*)
             (values (universal-ntype) t)
             (type-specifier-ntype car))
       (multiple-value-bind (cdr-ntype b)
           (if (eql cdr '*)
               (values (universal-ntype) t)
               (type-specifier-ntype cdr))
         (multiple-value-bind (ntype c)
             (find-primitive-ntype 'cons)
           (if (and (eq car-ntype (universal-ntype))
                    (eq cdr-ntype (universal-ntype)))
               (values ntype (and a b c))
               (values ntype nil))))))
    (_ (call-next-method))))

(defmethod compound-type-specifier-ntype ((_ (eql 'values)) rest whole)
  (error "Values type specifiers can only be parsed by VALUES-TYPE-SPECIFIER-NTYPE."))

(defmethod compound-type-specifier-ntype ((_ (eql 'function)) rest whole)
  (find-primitive-ntype 'function))

;;; Populate the Caches

(mapc #'type-specifier-ntype *standardized-atomic-type-specifiers*)
(mapc #'type-specifier-ntype *built-in-classes*)
