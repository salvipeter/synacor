;;; Program near 6027-6067 [Haskell style]:
;;; f 0 b = b + 1
;;; f a 0 = f (a-1) x
;;; f a b = f (a-1) $ f a (b-1)

;;; Manual computation shows that
;;; f 1 b = x + b + 1
;;; f 2 b = (x + 1) * b + 2 * x + 1

(defvar *x*)

(defun f (a b)
  (case a
    (0 (mod (1+ b) 32768))
    (1 (mod (+ *x* b 1) 32768))
    (2 (mod (+ (* (1+ *x*) b) (* 2 *x*) 1) 32768))
    (t (if (> b 0)
           (f (1- a) (f a (1- b)))
           (f (1- a) *x*)))))

;;; For what X value will this be 6? (as checked in 5491)

(loop for i from 1 below 32768
      until (let ((*x* i))
              (= (f 4 1) 6))
      finally (return i))
