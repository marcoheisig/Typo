(in-package #:typo.vm)

(macrolet
    ((defchareq (name two-arg-name two-arg-base-name)
       `(progn
          (define-fnrecord ,name (character &rest more-characters)
            (:properties :foldable :movable)
            (:specializer
             (if (null more-characters)
                 (wrap (prog2-fn (the-character character) t))
                 (apply
                  (function-specializer 'and)
                  (mapcar
                   (lambda (other-character)
                     (wrap (,two-arg-name character other-character)))
                   more-characters)))))

          (define-simple-instruction
              (,name ,two-arg-base-name) (generalized-boolean) (base-char base-char))

          (define-instruction (,name ,two-arg-name) (generalized-boolean) (a b)
            (ntype-subtypecase (wrapper-ntype a)
              ((not character) (abort-specialization))
              (base-char
               (ntype-subtypecase (wrapper-ntype b)
                 ((not character) (abort-specialization))
                 (base-char (wrap (,two-arg-base-name a b)))
                 (character (wrap-default (type-specifier-ntype 'generalized-boolean)))))
              (t (wrap-default (type-specifier-ntype 'generalized-boolean))))))))
  (defchareq char= two-arg-char= two-arg-base-char=)
  (defchareq char-equal two-arg-char-equal two-arg-base-char-equal))

(macrolet
    ((defcharneq (name two-arg-name two-arg-base-name)
       `(progn
          (define-fnrecord ,name (character &rest more-characters)
            (:properties :foldable :movable)
            (:specializer
             (if (null more-characters)
                 (wrap
                  (prog2-fn
                   (the-character character)
                   t))
                 (let ((value (wrap t)))
                   (map-unique-pairs
                    (lambda (a b)
                      (setf value
                            (wrap
                             (and-fn value (,two-arg-name a b)))))
                    (list* character more-characters))
                   value))))
          (define-simple-instruction
              (,name ,two-arg-base-name)
              (generalized-boolean)
              (base-char base-char))
          (define-instruction (,name ,two-arg-name) (generalized-boolean) (a b)
            (ntype-subtypecase (wrapper-ntype a)
              ((not character) (abort-specialization))
              (base-char
               (ntype-subtypecase (wrapper-ntype b)
                 ((not character) (abort-specialization))
                 (base-char (wrap (,two-arg-base-name a b)))
                 (character (wrap-default (type-specifier-ntype 'generalized-boolean)))))
              (t (wrap-default (type-specifier-ntype 'generalized-boolean))))))))
  (defcharneq char/= two-arg-char/= two-arg-base-char/=)
  (defcharneq char-not-equal two-arg-char-not-equal two-arg-base-char-not-equal))

(macrolet
    ((defcharcmp (name two-arg-name two-arg-base-name)
        `(progn
           (define-fnrecord ,name (character &rest more-characters)
             (:properties :foldable :movable)
             (:specializer
              (if (null more-characters)
                  (wrap
                   (prog2-fn
                    (the-character character)
                    t))
                  (let ((a character)
                        (value (wrap t)))
                    (loop for b in more-characters
                          for boolean = (wrap (,two-arg-name a b)) do
                      (setf value (wrap (and-fn value boolean)))
                      (setf a b))
                    value))))
           (define-simple-instruction
               (,name ,two-arg-base-name)
               (generalized-boolean)
               (base-char base-char))
           (define-instruction (,name ,two-arg-name) (generalized-boolean) (a b)
             (ntype-subtypecase (wrapper-ntype a)
               ((not character) (abort-specialization))
               (base-char
                (ntype-subtypecase (wrapper-ntype b)
                  ((not character) (abort-specialization))
                  (base-char (wrap (,two-arg-base-name a b)))
                  (character (wrap-default (type-specifier-ntype 'generalized-boolean)))))
               (t (wrap-default (type-specifier-ntype 'generalized-boolean))))))))
  (defcharcmp char< two-arg-char< two-arg-base-char<)
  (defcharcmp char> two-arg-char> two-arg-base-char>)
  (defcharcmp char<= two-arg-char<= two-arg-base-char<=)
  (defcharcmp char>= two-arg-char>= two-arg-base-char>=)
  (defcharcmp char-lessp two-arg-char-lessp two-arg-base-char-lessp)
  (defcharcmp char-greaterp two-arg-char-greaterp two-arg-base-char-greaterp)
  (defcharcmp char-not-greaterp two-arg-char-not-greaterp two-arg-base-char-not-greaterp)
  (defcharcmp char-not-lessp two-arg-char-not-lessp two-arg-base-char-not-lessp))

(define-fnrecord character (c)
  (:properties :foldable)
  (:specializer
   (ntype-subtypecase (wrapper-ntype c)
     ((not (or character (string 1))) (abort-specialization))
     ((string 1) (wrap (char c 0)))
     (character c)
     (t (wrap-default (type-specifier-ntype 'character))))))

(define-fnrecord digit-char (weight &optional (radix nil radix-supplied-p))
  (:properties :foldable :movable)
  (:specializer
   (assert-wrapper-type weight radix)
   (wrap-default (type-specifier-ntype '(or character null)))))

(define-fnrecord char-upcase (c)
  (:properties :foldable :movable)
  (:specializer
   (ntype-subtypecase (wrapper-ntype c)
     ((not character) (abort-specialization))
     (base-char (wrap-default (type-specifier-ntype 'base-char)))
     (t (wrap-default (type-specifier-ntype 'character))))))

(define-fnrecord char-downcase (c)
  (:properties :foldable :movable)
  (:specializer
   (ntype-subtypecase (wrapper-ntype c)
     ((not character) (abort-specialization))
     (base-char (wrap-default (type-specifier-ntype 'base-char)))
     (t (wrap-default (type-specifier-ntype 'character))))))

(define-fnrecord char-code (char)
  (:properties :foldable :movable)
  (:specializer
   (ntype-subtypecase (wrapper-ntype char)
     ((not character) (abort-specialization))
     (t (wrap-default (type-specifier-ntype '(integer 0 (#.char-code-limit))))))))

(define-fnrecord char-int (char)
  (:properties :foldable :movable)
  (:specializer
   (ntype-subtypecase (wrapper-ntype char)
     ((not character) (abort-specialization))
     (t (wrap-default (type-specifier-ntype 'unsigned-byte))))))

(define-fnrecord code-char (code)
  (:properties :foldable :movable)
  (:specializer
   (ntype-subtypecase (wrapper-ntype code)
     ((not (integer 0 (#.char-code-limit))) (abort-specialization))
     ((integer 0 (#.base-char-code-limit)) (wrap-default (type-specifier-ntype 'base-char)))
     (t (wrap-default (type-specifier-ntype 'character))))))

(define-fnrecord char-name (char)
  (:properties :foldable :movable)
  (:specializer
   (ntype-subtypecase (wrapper-ntype char)
     ((not character) (abort-specialization))
     (t (wrap-default (type-specifier-ntype '(or null string)))))))

(define-fnrecord name-char (name)
  (:properties :foldable :movable)
  (:specializer
   (ntype-subtypecase (wrapper-ntype name)
     ((not string-designator) (abort-specialization))
     (t (wrap-default (type-specifier-ntype '(or null character)))))))
