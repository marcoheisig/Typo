(in-package #:typo.ntype)

(defvar *primitive-ntypes*
  (coerce
   (loop for (type-specifier bits) in *primitive-ntype-information*
         for index from 0
         collect (%make-primitive-ntype
                  :index index
                  :bits bits
                  :type-specifier type-specifier
                  :class (and (symbolp type-specifier)
                              (find-class type-specifier nil))))
   'vector))

(declaim (type (simple-array primitive-ntype (#.(length *primitive-ntype-information*)))
               *primitive-ntypes*))

(defconstant +primitive-ntype-limit+
  (length *primitive-ntype-information*))

(declaim (inline primitive-ntype-from-index))
(defun primitive-ntype-from-index (index)
  (declare (ntype-index index))
  (aref *primitive-ntypes* index))

(defmethod ntype-primitive-ntype
    ((ntype ntype))
  (values (primitive-ntype-from-index (ntype-index ntype)) nil))

(defmethod make-load-form ((primitive-ntype primitive-ntype) &optional env)
  (declare (ignore env))
  `(primitive-ntype-from-index
    ,(primitive-ntype-index primitive-ntype)))

(defun find-primitive-ntype (type-specifier)
  (let ((primitive-ntype
          (find type-specifier *primitive-ntypes*
                :test #'subtypep
                :key #'primitive-ntype-type-specifier)))
    (values
     primitive-ntype
     (subtypep (primitive-ntype-type-specifier primitive-ntype) type-specifier))))

(define-compiler-macro find-primitive-ntype (&whole form type-specifier)
  (if (constantp type-specifier)
      (locally (declare (notinline find-primitive-ntype))
        (multiple-value-bind (primitive-ntype precise-p)
            (find-primitive-ntype (eval type-specifier))
          `(values ,primitive-ntype ',precise-p)))
      form))

(declaim (inline empty-ntype))
(defun empty-ntype ()
  (primitive-ntype-from-index 0))

(declaim (inline universal-ntype))
(defun universal-ntype ()
  (primitive-ntype-from-index (1- +primitive-ntype-limit+)))

(defvar *upgraded-array-element-primitive-ntypes*
  (remove-if-not
   (lambda (primitive-ntype)
     (let ((type-specifier (primitive-ntype-type-specifier primitive-ntype)))
       (alexandria:type=
        type-specifier
        (upgraded-array-element-type type-specifier))))
   (coerce *primitive-ntypes* 'list)))

(defvar *upgraded-complex-part-primitive-ntypes*
  (remove-if-not
   (lambda (primitive-ntype)
     (let ((type-specifier (primitive-ntype-type-specifier primitive-ntype)))
       (and (subtypep type-specifier 'real)
            (alexandria:type=
             type-specifier
             (upgraded-complex-part-type type-specifier)))))
   (coerce *primitive-ntypes* 'list)))

(defvar *complex-primitive-ntypes*
  (remove
   (empty-ntype)
   (remove-if-not
    (lambda (primitive-ntype)
      (subtypep (primitive-ntype-type-specifier primitive-ntype) 'complex))
    (coerce *primitive-ntypes* 'list))))

(defvar *float-primitive-ntypes*
  (remove
   (empty-ntype)
   (remove-if-not
    (lambda (primitive-ntype)
      (and (subtypep (primitive-ntype-type-specifier primitive-ntype) 'float)))
    (coerce *primitive-ntypes* 'list))))
