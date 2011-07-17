;;; the nondeterministic operator (amb)

;; amb implementation as described in “Teach Yourself Scheme in Fixnum Days”
;; http://www.ccs.neu.edu/home/dorai/t-y-scheme/t-y-scheme-Z-H-16.html#node_sec_14.2

(defpackage test-amb
  (:use :cl)
  (:import-from :jcls :call/cc :with-cc)
  (:import-from :ymacs :delay :log))

(in-package :test-amb)

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

;; note above that we're delaying the previous continuation (using
;; ymacs:delay); this allows the browser to refresh the display and
;; handle events, so that you can continue to use the editor, while
;; this program runs.

;; -----------------------------------------------------------------

;; Problem: given numbers 1..N, place + or - signs between them such
;; that the sum is 1.  The following function uses the amb operator to
;; chose between X, -X (where X is the current number).  When we
;; picked N numbers, we test their sum; if it's a solution, print it,
;; otherwise call amb without arguments (which fails and backtracks to
;; the previous choice point).

(defun sumis (n &optional (sum 1))
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
        (rec () n))))
  (log (js:strcat "Finished for n = " n)))

;; <btw>
;;   This implementation is really gross; the solution can be optimized
;;   if we notice that:
;;
;;   let A = sum of numbers where we pick plus
;;   let B = sum of numbers where we pick minus:
;;
;;      A - B = 1
;;      A + B = N(N+1) / 2
;;   ==>
;;      A = (N(N+1)+2) / 4
;;
;;   Therefore (1) we only have solutions if (N(N+1)+2) mod 4 = 0 and
;;   (2) we know in advance the sum of the numbers where we pick "+",
;;   which would allow us to avoid examining many unproductive parts of
;;   the tree.
;; </btw>

;; The calls below will still run sequentially, because the
;; continuation of each line is to move to the next line.  However,
;; they can run in parallel -- to see this press C-M-x on each line.
;; That would invoke the evaluator on each expression alone.

;; (sumis 10)
;; (sumis 6)
;; (sumis 9)

;; -----------------------------------------------------------------

;; Who owns the fish? -- See http://weitz.de/einstein.html for a
;; description of the problem.

;; The solution here is based on my implementation in JavaScript:
;; http://mihai.bazon.net/blog/amb-in-javascript/take-two#wotf

;; fail if condition is not true
(defmacro assert (condition)
  `(unless ,condition (amb)))

;; asserts that both conditions are either true or false
(defmacro iff (c1 c2)
  `(assert (eq ,c1 ,c2)))

;; This is an *ugly* macro.  It assumes the existence of some
;; variables in the scope where it is used.  Normally I would use
;; macrolet for this, but JCLS doesn't yet have macrolet.
(defmacro pick (type &rest choices)
  `(let ((tmp (amb ,@choices)))
     (assert (not (find-house houses ,type tmp)))
     tmp))

;; a house is represented by a list of 5 elements -- natioinality,
;; beverage, tobacco brand, pet and color (in this order).  This
;; function retrieves the requested property of a house.
(defun house-prop (prop house)
  (destructuring-bind (nationality beverage tobacco pet color) house
    (case prop
      (:nationality nationality)
      (:beverage beverage)
      (:tobacco tobacco)
      (:pet pet)
      (:color color))))

;; In a list of `houses', locate the one that has property `type'
;; equal to `value' and return its index (zero-based).  Return NIL if
;; not found.
(defun find-house (houses type value)
  (with-cc (return)
    (let ((i 0))
      (foreach houses
               (lambda (h)
                 (when (eq (house-prop type h) value)
                   (funcall return i))
                 (incf i)))
      (funcall return nil))))

;; asserts that houses having property `t1' = `v1' and `t2' = `v2' are
;; neighbors (distance between them is 1 or -1).
(defun neighbors (houses t1 v1 t2 v2)
  (let* ((i1 (find-house houses t1 v1))
         (i2 (find-house houses t2 v2))
         (diff (- i1 i2)))
    (assert (or (= 1 diff)
                (= -1 diff)))))

;; main entry point into the problem.  using the `pick' macro, select
;; nationality, beverage, tobacco, pets and colors, such that the rest
;; of the program doesn't fail.  The “rest of the program” simply
;; asserts the problem conditions, using the helper functions and
;; macros defined above.
(defun who-owns-the-fish ()
  (labels ((add (houses index)
             (let ((nat (pick :nationality  'british 'swedish 'danish 'norwegian 'german))
                   (bev (pick :beverage     'tea 'milk 'coffee 'beer 'water))
                   (tob (pick :tobacco      'pallmall 'dunhill 'marlboro 'winfield 'rothmans))
                   (pet (pick :pet          'dogs 'cats 'horses 'birds 'fish))
                   (col (pick :color        'red 'green 'white 'yellow 'blue)))
               (iff (eq nat 'british) (eq col 'red))
               (iff (eq nat 'swedish) (eq pet 'dogs))
               (iff (eq nat 'danish) (eq bev 'tea))
               (iff (eq col 'white) (and (> index 0)
                                         (eq 'green
                                             (house-prop :color (nth (1- index) houses)))))
               (iff (eq col 'green) (eq bev 'coffee))
               (iff (eq tob 'pallmall) (eq pet 'birds))
               (iff (eq col 'yellow) (eq tob 'dunhill))
               (iff (= index 2) (eq bev 'milk))
               (iff (= index 0) (eq nat 'norwegian))
               (iff (eq tob 'winfield) (eq bev 'beer))
               (iff (eq nat 'german) (eq tob 'rothmans))
               (let* ((h (list nat bev tob pet col))
                      (houses (append houses (list h))))
                 (log houses)
                 (if (= index 4)
                     (progn
                       (neighbors houses :tobacco 'marlboro :pet 'cats)
                       (neighbors houses :pet 'horses :tobacco 'dunhill)
                       (neighbors houses :nationality 'norwegian :color 'blue)
                       (neighbors houses :tobacco 'marlboro :beverage 'water)
                       (log "*** SOLUTION!")
                       ;; and return it
                       houses)
                     (add houses (1+ index)))))))
    (add () 0)))

;; It takes 95s in Chrome.  Uncomment the following line and press
;; C-M-x inside it to run.  Note that it logs the list of houses
;; passing the first round of conditions (log houses), so you know
;; it's not frozen ;-).  When the solution is found it'll display
;; SOLUTION and return it.
;;
; (who-owns-the-fish)
;;
;; after it completes, if you want to search for more solutions you
;; can simply evaluate (amb).  There is only one solution, though, but
;; the program will continue from where it left off trying to search
;; for more (if you're patient enough it will finish with "amb tree
;; exhausted").

;; -*- jcls -*-
