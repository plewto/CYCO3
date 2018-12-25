;;;; CYCO pdf plugin discrete distributions
;;;;

(defun bernoulli-dist (&key (p 0.5)(a 1)(b 0)(rnf #'random))
  (if (< (funcall rnf 1.0) p) a b))
      
(defun binomial-dist (&key (p 0.5)(count 10)(rnf #'random))
  (let ((acc 0))
    (dotimes (i count acc)
      (setf acc (+ acc (bernoulli-dist :p p :rnf rnf))))))

(defun geometric-dist (&key (p 0.5)(count 10)(rnf #'random))
  (loop
   (if (= (bernoulli-dist :p p :rnf rnf) 1)
       (return count)
     (setf count (1+ count)))))

(defun poisson-dist (&key (mu 5)(rnf #'random))
  (let ((x 0)
	(q 0.0))
    (loop
     (setf q (- q (/ (log (funcall rnf 1.0)) mu)))
     (if (> q 1)(return x))
     (setf x (1+ x)))))


