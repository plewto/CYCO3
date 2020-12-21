;;;; CYCO plugins sj eastwest keyswitch.lisp
;;;;

(flet ((process-assignments
	(assignments)
	(let ((acc '())
	      (program 0))
	  (dolist (q assignments)
	    (let ((key (car (->list q)))
		  (rem (or (->list q) "")))
	      (push (list key program rem) acc))
	    (setf program (1+ program)))
	  (reverse acc))))

  (defun make-eastwest-program-map (instrument assignments)
    (let ((assignment-list (process-assignments assignments)))
      (flet ((docfn ()
		    (format t ";; ~A Program Map:~%" (name instrument))
		    (dolist (a assignment-list)
		      (let ((id (car a))
			    (pnum (second a))
			    (remarks (or (third a) "")))
			(format t ";;   [~12A] -> ~3A ~A~%" id pnum remarks)))
		    nil)
	     (numeric-program (pnum)
			      (let ((limit (length assignment-list)))
				(if (and (<= 0 pnum)(< pnum limit))
				    pnum
				  (progn
				    (cyco-warning
				     (sformat "Invalid ~A program number: ~A"
					      (name instrument) pnum))
				    nil))))
	     (symbolic-program (id)
			       (or (car (cdr (assoc id assignment-list)))
				   (progn
				     (cyco-warning
				      (sformat "Invalid ~A program: ~A"
					       (name instrument) id))
				     nil))) )
	     (let* ((ci (channel-index instrument))
		    (fn #'(lambda (time &key (program 0) bank)
			    (declare (ignore bank))
			    (let ((pnum (cond ((eq program :doc)
					       (docfn))
					      ((numberp program)
					       (numeric-program program))
					      ((eq program :default)
					       (numeric-program (or (program-number instrument) 0)))
					      ((symbolp program)
					       (symbolic-program program))
					      (t
					       (cyco-warning
						(sformat "Invalid ~A program: ~A"
							 (name instrument) program))
					       nil))))
			      (if pnum
				  (list (cons time (midi-program-change ci pnum)))
				nil)))))
	       (program-map! instrument fn)
	       fn)))) )
