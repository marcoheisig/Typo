(in-package #:typo.fndb)

(declaim (ftype (function (function) (values (or null function-name) &optional))
                function-name))
(defun function-name (function)
  #-sbcl nil
  #+sbcl
  (let ((fun-name (sb-kernel:%fun-name function)))
    (if (typep fun-name 'function-name)
        fun-name
        nil)))
