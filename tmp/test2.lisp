(jcls:print "Reader and pacakges")

(defmacro in-package (name)
  `(setq *package* (jcls:find-package ,name)))

(defvar foo (jcls:make-package "FOO" '(cl cl-user)))
(defvar bar (jcls:make-package "BAR" '(cl cl-user)))

(defparameter s1 'mak)

(in-package :foo)

(jcls:print (eq cl-user:s1 'mak))
(jcls:print (eq 'mak 'mak))
(jcls:print cl-user:s1 'mak)
