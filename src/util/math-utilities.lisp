;;;; CYCO
;;;;

(defun 2+ (n)(+ 2 n))
(defun 3+ (n)(+ 3 n))
(defun 4+ (n)(+ 4 n))

(defun limit (n mn mx)
  "Limits number to interval (min max)"
  (max (min n mx) mn))


(labels ((pick-sign ()(if (< (random 1.0) 0.5) -1 +1))
	 (aprox-zero (range)(* (pick-sign)(random range))))

  (defun approximate (value &key (scale 0.1)(min -1e9)(max 1e9)(epsilon nil))
    "Returns random approximation of value.
value  - number, 
:scale - float, 0 <= scale <= 1,  maximum deviation ratio from value, nil treated as 0.
:min   - float, minimum result.
:max   - float, maximum result.
:epsilon - float, maximum absolute result when value is zero. epsilon = scale by default."
    (limit (if (or (null scale)(zerop scale))
		value
	      (if (zerop value)
		  (aprox-zero (or epsilon scale))
		(let* ((range (* value scale))
		       (delta (* (pick-sign)(random range))))
		  (+ value delta))))
	    min max)))
		
(defun mean (lst)
  (/ (float (apply #'+ lst))
     (length lst)))


(defun dividesp (a b)
  "Predicate, true if b evenly divides a, b != 0"
  (let ((q (truncate (/ (float a) b))))
    (= a (* q b))))
