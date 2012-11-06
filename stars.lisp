(defpackage #:star
  (:use #:cl #:vecto))

(in-package #:star)

(defparameter *radius* 250)
(defparameter *center* '(300 300))
(defparameter *dimens* '(600 600))
(defparameter *path* "spiral.png")

(defun partial (fn &rest args)
  (lambda (&rest more-args)
    (apply fn (append args more-args))))

(defun star (points jump)
  (let ((times (/ points (gcd points jump))))
    (loop for i from 0 below times collect
	 (mod (* i jump) points))))

(defun angle-to-rect (rad angle)
  (mapcar (partial '* rad) (list (cos angle) (sin angle))))

(defun draw-points (nums)
  (let* ((n (length nums))
	 (step (/ (* 2 pi) n)))
    (apply #'move-to (mapcar '+ *center* (angle-to-rect *radius* (* (car (last nums)) step))))
    (loop for x in nums do
	 (apply #'line-to (mapcar '+ *center* (angle-to-rect *radius* (* x step)))))))

(defun main ()
 (with-canvas (:width (car *dimens*) :height (cadr *dimens*))
   (set-rgb-stroke 0 0 0)
   (set-rgb-fill 255 255 255)
   (apply #'rectangle 0 0 *dimens*)
   (fill-path)
   (draw-points (star 101 50))
   (stroke)
   (save-png *path*)))
