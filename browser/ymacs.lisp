(defpackage :ymacs
  (:use :cl)
  (:export "LOG" "WITH-DELAY"))

(in-package :ymacs)

(defmacro call-ymacs (name &rest args)
  `(jcls:apply-native (jcls:native "THE_EDITOR" ,name)
                      (jcls:native "THE_EDITOR")
                      (list ,@args)))

(defun log (txt)
  (call-ymacs "jcls_log" txt))

(defmacro with-delay ((timeout) &body body)
  `(jcls:call-native (jcls:native "setTimeout") nil
                     (jcls:make-native-function (lambda ()
                                                  ,@body))
                     ,timeout))
