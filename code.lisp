;;; Program near 6027-6067 [Haskell style]:
;;; f 0 b = b + 1
;;; f a 0 = f (a-1) x
;;; f a b = f (a-1) $ f a (b-1)

(defvar *x*)

;;; Continuation passing style
(defun f (a b &optional (cont #'identity))
  (case a
    (0 (funcall cont (mod (1+ b) 32768)))
    (1 (funcall cont (mod (+ *x* b 1) 32768)))
    (2 (funcall cont (mod (+ (* (1+ *x*) b) (* 2 *x*) 1) 32768)))
    (t (if (> b 0)
           (f a (1- b) (lambda (x) (f (1- a) x cont)))
           (f (1- a) *x* cont)))))

;;; For what X value will this be 6?

(loop for i from 1 below 32768
      until (let ((*x* i))
              (= (f 4 1) 6))
      finally (return i))
