(defmacro in-package (name)
  `(setq *package* (jcls:find-package ,name)))

(defvar foo (jcls:make-package "FOO" '(cl cl-user)))

(in-package :cl)

(defun test ()
  "test")

(cl-user::in-package :foo)

(jcls:print (cl:test))
