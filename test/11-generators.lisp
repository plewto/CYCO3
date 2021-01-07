;;;; test 11-generators
;;;;

(let* ((val 3)
       (gen (constant-value val)))
  (pass? "constant-value"
	 (every #'(lambda (n)(= n val))(next gen 10))))
       

(let* ((gen (counter))
       (expect '(0 1 2 3 4))
       (actual (next gen 5)))
  (pass? "counter"
	 (equal expect actual)))

(let* ((gen (counter :hook #'(lambda (n)(* n n))))
       (expect '(0 1 4 9 16))
       (actual (next gen 5)))
  (pass? "counter with hook"
	 (equal expect actual)))


(let ((action-counter 0))
  (flet ((action-hook (n)
		      (declare (ignore n))
		      (setf action-counter (1+ action-counter))))
    (let* ((gen1 (countdown 3))
	   (expect '(3 2 1 0 0))
	   (actual (next gen1 5)))
      (pass? "countdown test 1"
	     (equal expect actual)))
    
    (let* ((gen2 (countdown 3 :action #'action-hook :multi-trigger nil))
	   (expect '(3 2 1 0 0))
	   (actual (next gen2 5)))
      (pass? "countdown test 2 with single action"
	     (and (equal expect actual)
		  (= action-counter 1))))

    (setf action-counter 0)
    (let* ((gen3 (countdown 3 :action #'action-hook :multi-trigger t)))
      (next gen3 5)
      (pass? "countdown test 3 with multi action"
      	     (= action-counter 2))) ))
    
