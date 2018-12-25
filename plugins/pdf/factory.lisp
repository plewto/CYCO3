;;;; CYCO pdf plugin factory
;;;; Creates functions by wrapping distributions.
;;;; Resulting functions take no arguments and return float
;;;; Parameters are preset by keyword arguments.
;;;; If a value for :minmax is supplied it must be a list (mn mx)
;;;; where mn and mx are the minimum and maximum allowed results.
;;;;

(flet ((limiter (minmax)
		(if (not minmax)
		    #'(lambda (q) q)
		  (let ((mn (or (car minmax) 1e16))
			(mx (or (second minmax) -1e16)))
		    #'(lambda (q)(limit q mn mx))))))
  
  (defun linear-pdf (&key (scale 1.0)(rnf #'random) minmax)
    (let ((limit-fn (limiter minmax)))
      #'(lambda ()
          (funcall limit-fn (linear-dist :rnf rnf :scale scale))))) 

  (defun exponential-pdf (&key (delta 2)(rnf #'random) minmax)
    (let ((limit-fn (limiter minmax)))
      #'(lambda ()
          (funcall limit-fn (exponential-dist :rnf rnf :delta delta))))) 
    
  (defun gamma-pdf (&key (mu 5)(rnf #'random) minmax)
    (let ((limit-fn (limiter minmax)))
      #'(lambda ()
          (funcall limit-fn (gamma-dist :rnf rnf :mu mu)))))
    
  (defun bilateral-pdf (&key (mu 0)(tau 1)(rnf #'random) minmax)
    (let ((limit-fn (limiter minmax)))
      #'(lambda ()
          (funcall limit-fn (bilateral-dist :rnf rnf :mu mu :tau tau)))))
  
  (defun cauchy-pdf (&key (tau 1.0)(rnf #'random) minmax)
    (let ((limit-fn (limiter minmax)))
      #'(lambda ()
          (funcall limit-fn (cauchy-dist :rnf rnf :tau tau)))))  
  
  (defun hyperbolic-cosine-pdf (&key (rnf #'random) minmax)
    (let ((limit-fn (limiter minmax)))
      #'(lambda ()
          (funcall limit-fn (hyperbolic-cosine-dist :rnf rnf)))))  
  
  (defun gaussian-pdf (&key (mu 0)(sigma 1)(rnf #'random) minmax)
    (let ((limit-fn (limiter minmax)))
      #'(lambda ()
          (funcall limit-fn (gaussian-dist :rnf rnf :mu mu :sigma sigma)))))
  
  (defun beta-pdf (&key (a 1)(b 1)(rnf #'random) minmax)
    (let ((limit-fn (limiter minmax)))
      #'(lambda ()
          (funcall limit-fn (beta-dist :rnf rnf :a a :b b)))))
  
  (defun bernoulli-pdf (&key (p 0.5)(a 1)(b 0)(rnf #'random) minmax)
    (let ((limit-fn (limiter minmax)))
      #'(lambda ()
	  (funcall limit-fn (bernoulli-dist :rnf rnf :p p :a a :b b)))))

  (defun binomial-pdf (&key (p 0.5)(count 10)(rnf #'random) minmax)
    (let ((limit-fn (limiter minmax)))
      #'(lambda ()
          (funcall limit-fn (binomial-dist :rnf rnf :p p :count count)))))
  
  (defun geometric-pdf (&key (p 0.5)(count 10)(rnf #'random) minmax)
    (let ((limit-fn (limiter minmax)))
      #'(lambda ()
          (funcall limit-fn (geometric-dist :rnf rnf :p p :count count)))))
  
  (defun poisson-pdf (&key (mu 5)(rnf #'random) minmax)
    (let ((limit-fn (limiter minmax)))
      #'(lambda ()
          (funcall limit-fn (poisson-dist :rnf rnf :mu mu))))) )



