;;;; CYCO orchestra round-robin-map.lisp
;;;;
;;;; Defines keynumber-map function factories with round-robin support.
;;;;


(labels ((rr-select (entry counters)
		    ;; entry (key . ((options) optional_remarks))
		    ;; counter is hash-table key -> count
		    (let ((counter (gethash (car entry) counters)))
		      (prog1
			  (cnth counter (car (cdr entry)))
			(setf (gethash (car entry) counters) (1+ counter)))))

	 (doc-function (assignments)
	 	       (format t ";; Round-Robbin keynumber map~%")
	 	       (let ((index 0))
	 		 (dolist (entry assignments)
	 		   (format t ";;   [~3D] [~16A] -> ~A" index (car entry)(car (cdr entry)))
	 		   (if (second (cdr entry))
	 		       (format t "  ~A" (second (cdr entry))))
	 		   (setf index (1+ index))
			   (format t "~%"))
	 		 (format t ";; ~%"))
	 	       +rest+)
	 
	 (gamutfn (assignments)
		  (let ((keys (mapcar #'second assignments)))
		    (sort (delete-duplicates (keynumber (flatten keys))) #'<))) )

	(defun round-robin-keymap (assignments)
	  (let ((counters (make-hash-table :size (length assignments))))
	    (dolist (entry assignments)
	      (setf (gethash (car entry) counters) 0))
	    #'(lambda (kn)
		(cond ((and (integerp kn)(not (minusp kn)))
		       (rr-select (cnth kn assignments) counters))
		      ((eq kn 'x)
		       (rr-select (first assignments) counters))
		      ((eq kn :doc)
		       (doc-function assignments)
		       +rest+)
		      ((eq kn :gamut)
		       (gamutfn assignments))
		      ((eq kn 'x)
		       (keynumber (caar assignments)))
		      (t (let ((entry (assoc kn assignments)))
			   (if entry
			       (rr-select entry counters)
			     +rest+))))))) )

(setf (documentation 'round-robin-keymap 'function)
      "Returns a symbolic keynumber-map using round-robin key-numbers.

assignments argument should be an alist of form:
     
     ((key-1 . ((keynumbers-1 ...) optional-remarks))
      (key-2 . ((keynumbers-2 ...) optional-remarks))
       ........................................
      (key-n . ((keynumbers-n ...) optional-remarks)))

Where (keynumbers-i ...) is a list of MIDI key numbers.


Example:

     (param rrmap (round-robin-keymap '((A . (10 11 12))(B . (20 21)))))
     (dotimes (i 7)
        (print (funcall rrmap 'A)))

     --> 10 11 12 10 11 12 10")
