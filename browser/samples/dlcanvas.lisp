;; -*- jcls -*-

(defpackage :canvas
  (:use :cl :ymacs)
  (:import-from :jcls "@" "NEW-NATIVE"))

(in-package :canvas)

(defparameter dlg (new-native "DlDialog"
                              (jcls:alist-js
                                  '(("quitBtn" . "destroy")
                                    ("title" . "DlCanvas test")
                                    ("resizable" . T)))))

(defparameter canvas (new-native "DlCanvas"
                                 (jcls:alist-js
                                     `(("parent" . ,dlg)
                                       ("fillParent" . T)))))

(let* ((DlCanvas (jcls:native "DlCanvas"))
       (Ellipse (@ DlCanvas "Ellipse"))
       (Rect (@ DlCanvas "Rect")))
  (js:with-methods (canvas "setMouseListeners" "add")
    (set-mouse-listeners)
    (js:with-methods (DlCanvas "make_resizable")
      (foreach (list (new-native Rect 10 10 100 100)
                     (new-native Ellipse 100 100 100 100)
                     (new-native Rect 50 50 100 100))
               (lambda (el)
                 (add el)
                 (make-resizable el))))))

(js:with-methods (dlg "setSize" "show")
  (set-size (jcls:alist-js '(("x" . 600)
                             ("y" . 400))))
  (show t))
