(defmacro in-package (name)
  `(setq *package* (jcls:find-package ,name)))

(defvar foo (jcls:make-package "FOO" '(cl cl-user) '(bar)))

(in-package :cl)

(defun test ()
  "test")

(defun test2 ()
  "this one is exported")

(jcls:export 'test2)

(cl-user::in-package :bar)

(jcls:import 'cl::test)

(jcls:print (test))
(jcls:print (test2))
