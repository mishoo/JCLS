;; -*- jcls -*-

(defpackage test-amb
  (:use :cl))

(in-package :test-amb)

(jcls:import '(jcls:call/cc jcls:with-cc
               ymacs:log
               ymacs:delay))

(defparameter amb-fail
  (lambda ()
    (log "amb tree exhausted")))

(defmacro amb (&rest alternatives)
  (if alternatives
      `(let ((+prev-amb-fail amb-fail))
         (with-cc (+sk)
           ,@(map (lambda (alt)
                    `(with-cc (+fk)
                       (setq amb-fail +fk)
                       (funcall +sk ,alt)))
                  alternatives)
           (setq amb-fail +prev-amb-fail)
           (delay (0)
                  (funcall +prev-amb-fail nil))))
      `(funcall amb-fail nil)))

(defun solutions (n &optional (sum 1))
  (with-cc (k)
    (let ((amb-fail k))
      (labels ((required-sum? (numbers)
                 (= sum (apply (function +) numbers)))
               (rec (numbers next)
                 (if (= next 0)
                     (progn
                       (when (required-sum? numbers)
                         (log numbers))
                       (amb))
                     (rec (cons (amb next (- next))
                                numbers)
                          (1- next)))))
        (rec () n)))))

(solutions 10)
(solutions 6)
(solutions 9)
