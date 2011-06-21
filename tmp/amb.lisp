(jcls:import '(jcls:print jcls:call/cc jcls:with-cc))

;; http://www.ccs.neu.edu/home/dorai/t-y-scheme/t-y-scheme-Z-H-16.html

(defparameter amb-fail
  (lambda ()
    (print "amb tree exhausted")))

(defun map (func list)
  (if list
      (cons (funcall func (car list))
            (map func (cdr list)))))

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
           (funcall +prev-amb-fail)))
      `(funcall amb-fail)))

;; +-1 +-2 +-3 +-4 ... +- N == 1 --- figure out the + and - signs.
(flet ((solutions (n &optional (sum 1))
         (with-cc (k)
           (setq amb-fail k)
           (labels ((required-sum? (numbers)
                      (= sum (apply (function +) numbers)))
                    (rec (numbers next)
                      (if (= next 0)
                          (progn
                            (when (required-sum? numbers)
                              (print numbers))
                            (amb))
                          (rec (cons (amb next (- next)) numbers)
                               (1- next)))))
             (rec () n)))))
  (solutions 9)
  (print "we're done"))

;; (flet ((assert (condition)
;;          (unless condition (amb)))
;;        (iff (c1 c2)
;;          (assert (eq (not c1) (not c2))))
;;        (find-house (houses type val)
;;          )
;;        (wotf ()
;;          (let ((nationalities '(british swedish danish norwegian german))
;;                (beverages '(tea milk coffee beer water))
;;                (tobacco-brands '(pallmall dunhill marlboro winfield rothmans))
;;                (pets '(dogs cats horses birds fish))
;;                (colors '(red green white yellow blue)))
;;            )))
;;   (with-cc (k)
;;     (setq amb-fail k)
;;     (wotf)))
