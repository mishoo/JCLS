;; -*- jcls -*-

(defpackage canvas
  (:use :cl :ymacs))

(in-package :canvas)

(defparameter dlg (jcls:new-native "DlDialog"
                                   (jcls:alist-js
                                    '(("quitBtn" . "destroy")
                                      ("title" . "DlCanvas test")
                                      ("resizable" . T)))))

(defparameter canvas (jcls:new-native "DlCanvas"
                                      (jcls:alist-js
                                       `(("parent" . ,dlg)
                                         ("fillParent" . T)))))

(jcls:call-native canvas "setMouseListeners")

(let ((el (jcls:new-native (jcls:@ (jcls:native "DlCanvas") "Rect")
                           10 10 100 100)))
  (jcls:call-native canvas "add" el)
  (jcls:call-native (jcls:@ (jcls:native "DlCanvas") "make_resizable") nil el))

(let ((el (jcls:new-native (jcls:@ (jcls:native "DlCanvas") "Ellipse")
                           100 100 100 100)))
  (jcls:call-native canvas "add" el)
  (jcls:call-native (jcls:@ (jcls:native "DlCanvas") "make_resizable") nil el))

(jcls:call-native dlg "setSize"
                  (jcls:alist-js
                   '(("x" . 640)
                     ("y" . 480))))

(jcls:call-native dlg "show" T)
