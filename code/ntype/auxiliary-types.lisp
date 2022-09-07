(in-package #:typo.ntype)

;;; Some objects in the Common Lisp standard have a straightforward
;;; definition, but the CL package defines no corresponding type.  We
;;; define those types here.

(deftype non-nil-symbol ()
  '(and symbol (not null)))

(deftype function-name ()
  '(or non-nil-symbol (cons (eql setf) (cons non-nil-symbol))))

(deftype variable-name ()
  'non-nil-symbol)

(deftype function-designator ()
  '(or non-nil-symbol function))

(deftype extended-function-designator ()
  '(or function-name function))

(deftype character-designator ()
  '(or (vector character 1) character))

(deftype string-designator ()
  '(or character symbol string))

(deftype package-designator ()
  '(or package string-designator))

(deftype radix ()
  '(integer 2 36))

(deftype character-code ()
  '(integer 0 (#.char-code-limit)))

(deftype arity ()
  '(integer 0 (#.call-arguments-limit)))

(deftype argument-index ()
  '(integer 0 (#.(1- call-arguments-limit))))

;; The representation of byte specifiers is implementation-dependent.
;; However, under the not-so-bold assumption that each implementation
;; consistently uses a certain representation, we can get surprisingly far.
(deftype byte-specifier ()
  '#.(let ((a (byte 0 0))
           (b (byte 16 253)))
       (if (equal (type-of a)
                  (type-of b))
           (type-of a)
           't)))

(deftype complex-short-float ()
  '(complex short-float))

(deftype complex-single-float ()
  '(complex single-float))

(deftype complex-double-float ()
  '(complex double-float))

(deftype complex-long-float ()
  '(complex long-float))

(deftype generalized-boolean ()
  't)

(deftype zero ()
  `(member
    ,@(remove-duplicates
       '(0 0S0 -0S0 0F0 -0F0 0D0 -0D0 0L0 -0L0
         #C(0S0 0S0) #C(0S0 -0S0) #C(-0S0 0S0) #C(-0S0 -0S0)
         #C(0F0 0F0) #C(0F0 -0F0) #C(-0F0 0F0) #C(-0F0 -0F0)
         #C(0D0 0D0) #C(0D0 -0D0) #C(-0D0 0D0) #C(-0D0 -0D0)
         #C(0L0 0L0) #C(0L0 -0L0) #C(-0L0 0L0) #C(-0L0 -0L0)))))

(deftype type-specifier ()
  t)

(defvar *standardized-atomic-type-specifiers*
  '(arithmetic-error                  function            simple-condition
    array                             generic-function    simple-error
    atom                              hash-table          simple-string
    base-char                         integer             simple-type-error
    base-string                       keyword             simple-vector
    bignum                            list                simple-warning
    bit                               logical-pathname    single-float
    bit-vector                        long-float          standard-char
    broadcast-stream                  method              standard-class
    built-in-class                    method-combination  standard-generic-function
    cell-error                        nil                 standard-method
    character                         null                standard-object
    class                             number              storage-condition
    compiled-function                 package             stream
    complex                           package-error       stream-error
    concatenated-stream               parse-error         string
    condition                         pathname            string-stream
    cons                              print-not-readable  structure-class
    control-error                     program-error       structure-object
    division-by-zero                  random-state        style-warning
    double-float                      ratio               symbol
    echo-stream                       rational            synonym-stream
    end-of-file                       reader-error        t
    error                             readtable           two-way-stream
    extended-char                     real                type-error
    file-error                        restart             unbound-slot
    file-stream                       sequence            unbound-variable
    fixnum                            serious-condition   undefined-function
    float                             short-float         unsigned-byte
    floating-point-inexact            signed-byte         vector
    floating-point-invalid-operation  simple-array        warning
    floating-point-overflow           simple-base-string
    floating-point-underflow          simple-bit-vector))

(defvar *standardized-compound-type-specifiers*
  '(and           long-float    simple-base-string
    array         member        simple-bit-vector
    base-string   mod           simple-string
    bit-vector    not           simple-vector
    complex       or            single-float
    cons          rational      string
    double-float  real          unsigned-byte
    eql           satisfies     values
    float         short-float   vector
    function      signed-byte
    integer       simple-array))

(defvar *standardized-compound-only-type-specifiers*
  '(and     mod  satisfies
    eql     not  values
    member  or))

(deftype standardized-atomic-type-specifier ()
  `(member ,@*standardized-atomic-type-specifiers*))

(deftype standardized-compound-type-specifier ()
  `(cons (member ,@*standardized-compound-type-specifiers*)))

(deftype standardized-type-specifier ()
  '(or standardized-atomic-type-specifier standardized-compound-type-specifier))

(defparameter *built-in-classes*
  (let ((visited (make-hash-table))
        (result '()))
    (labels ((visit (class)
               (unless (gethash class visited)
                 (setf (gethash class visited) t)
                 (when (typep class 'built-in-class)
                   (push class result))
                 (mapc #'visit (class-direct-subclasses class)))))
      (visit (find-class t)))
    result))
