;;; Using continuations to implement a "sleep" function based on setTimeout
;;; -----------------------------------------------------------------------

;; Read this for a very good introduction to continuations:
;; http://www.ccs.neu.edu/home/dorai/t-y-scheme/t-y-scheme-Z-H-15.html

(defun sleep (milliseconds)
  (jcls:call/cc (lambda (k)
                  (jcls:call-native (jcls:native "setTimeout") nil
                                    (jcls:make-native-function k)
                                    milliseconds))))

;; the following is equivalent, but less verbose thanks to some macros
;; you can place the cursor on the start of "(jcls:with-cc"
;; or on "(ymacs:with-delay" and press «C-c ENTER» to see the macroexpansion
(defun sleep2 (milliseconds)
  (jcls:with-cc (k)
    (ymacs:with-delay (milliseconds)
      (funcall k))))

(progn
  (ymacs:log "foo")
  (sleep2 1000)
  (ymacs:log "bar"))

;; also see amb.lisp for more advanced use of continuations
