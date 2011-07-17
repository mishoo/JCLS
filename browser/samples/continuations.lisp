;;; Using continuations to implement a "sleep" function based on setTimeout
;;; -----------------------------------------------------------------------

(defun sleep (milliseconds)
  (jcls:call/cc (lambda (k)
                  (jcls:call-native (jcls:native "setTimeout") nil
                                    (jcls:make-native-function k)
                                    milliseconds))))

;; the following is equivalent, but less verbose thanks to some macros
;; you can place the cursor on the start of "(jcls:with-cc"
;; or on "(ymacs:delay" and press «C-c ENTER» to see the macroexpansion
(defun sleep2 (milliseconds)
  (jcls:with-cc (k)
    (ymacs:delay (milliseconds)
      (funcall k))))

(progn
  (ymacs:log "foo")
  (sleep2 1000)
  (ymacs:log "bar"))

