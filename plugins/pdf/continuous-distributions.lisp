;;;; CYCO pdf plugins distributions
;;;;
;;;; Defines random number generators with various continuous distributions.
;;;; Each distribution takes optional :rnf keyword to set the random-number
;;;; generator.  This function should take a single argument n, and return
;;;; a random float between 0 and n  (lambda n) --> float q,  0.0 <= q < n
;;;; the default rnf is RANDOM.
;;;;


(defun linear-dist (&key (scale 1.0)(rnf #'random))
  (* scale (- 1.0 (sqrt (funcall rnf 1.0)))))

(defun exponential-dist (&key (delta 2)(rnf #'random))
  (* (/ -1.0 delta)(log (funcall rnf 1.0))))

(defun gamma-dist (&key (mu 5)(rnf #'random))
  (let* ((sum1 1)
	 (sum2 (dotimes (count mu sum1)
		 (setf sum1 (* sum1 (funcall rnf 1.0))))))
    (* -1.0 (log sum2))))

;; mu - mean
;; tau - spread, tau > 0
;;
(defun bilateral-dist (&key (mu 0)(tau 1)(rnf #'random))
  (let ((u (funcall rnf 2.0)))
    (if (> u 1.0)
	(+ (* -1.0 tau (log (- u 1.0))) mu)
      (+ (* tau (log u)) mu))))

;; tau - variance, > 0
;;
(defun cauchy-dist (&key (tau 1.0)(rnf #'random))
  (let ((u (funcall rnf PI)))
    (* tau (/ (sin u)(cos u)))))

(defun hyperbolic-cosine-dist (&key (rnf #'random))
  (log (tan (/ (funcall rnf pi) 2.0))))

(defun gaussian-dist (&key (mu 0)(sigma 1)(rnf #'random))
  (let* ((s 0.0)
	 (s2 (dotimes (i 12 s)
	       (setf s (+ s (funcall rnf 1.0))))))
    (+ (* sigma (- s2 6.0)) mu)))


(flet ((helper (ea eb rnf)
	       (loop
		(let ((y1 (expt (funcall rnf 1.0) ea))
		      (y2 (expt (funcall rnf 1.0) eb)))
		  (if (<= (+ y1 y2) 1.0)
		      (return (/ y1 (+ y1 y2))))))))
  (defun beta-dist (&key (a 1)(b 1)(rnf #'random))
    (helper (/ 1.0 a)(/ 1.0 b) rnf)))
