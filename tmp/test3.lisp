(defmacro in-package (name)
  `(setq *package* (jcls:find-package ,name)))

(defmacro defpackage )

(defvar foo (jcls:make-package "FOO" nil '(bar)))

(in-package :cl)

(defun test ()
  "test")

(defun test2 ()
  "this one is exported")

(jcls:export 'test2)

(cl-user::in-package :bar)
(jcls:use-package :cl)

(jcls:import 'cl::test)

(jcls:print (test))
(jcls:print (test2))
