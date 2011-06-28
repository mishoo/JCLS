(jcls:import '(jcls:@
               jcls:native
               jcls:apply-native
               jcls:call-native
               jcls:make-native-function
               jcls:print))

(defmacro jsfun (args &body body)
  `(make-native-function (lambda ,args ,@body)))

(defun require (module)
  (apply-native (native "require") nil `(,module)))

;; so here's how it could be done
;; (defun read-file (filename)
;;   (jcls:with-cc (k)
;;     (apply-native (@ (require "fs") "readFile")
;;                        nil
;;                        `(
;;                          ,filename
;;                          "utf8"
;;                          ,(make-native-function (lambda (error ret)
;;                                                        (when error (error error))
;;                                                        (funcall k ret)))))))
;; (read-file "/etc/passwd")

(defmacro define-node-wrapper (name module func)
  `(defun ,name (&rest args)
     (jcls:with-cc (k)
       (apply-native (@ (require ,module) ,func)
                     nil
                     `(,@args
                       ,(make-native-function (lambda (error result)
                                                (when error (error error))
                                                (funcall k result))))))))

(define-node-wrapper read-file "fs" "readFile")
(read-file "/etc/passwd" "utf8")

(let ((server (call-native (require "net")
                           "createServer"
                           (make-native-function
                            (lambda (socket)
                              (call-native socket "addListener" "data"
                                           (jsfun (data)
                                             (jcls:print data)))
                              (call-native socket "write" "Blah blerg
")
                              (call-native socket "pipe" socket))))))
  (call-native server "listen" 1337 "127.0.0.1"))
