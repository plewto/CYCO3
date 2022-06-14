;;;; cyco composition/cueutil.lisp
;;;;
;;;; Cuelist related utilities.
;;;;

(in-package :cyco)

(labels ((compress (str)
		   (let ((white '(#\space #\newline #\tab)))
		     (remove-if #'(lambda (c)(member c white :test #'char=)) str)))

	 (pad-zeros (str width)
		    (string-pad-right str width #\0))

	 (simplify-cue-point (cp)
			      (let* ((width 3)
				     (diff (- width (length cp))))
				(if (plusp diff)
				    (append cp (copies diff 1))
				  (subseq cp 0 width))))

	 (print-header (text)
		       (if text (format t ";; ~A" text)))
	  
	 (print-bar (bar-number)
		    (format t "~%;; BAR ~A~%;; " bar-number))

	 )

	(defmethod duck ((cuelist list)(mask list) &key (invert nil) &allow-other-keys)
	  (let ((sig (mapcar #'simplify-cue-point cuelist))
		(exclude (mapcar #'simplify-cue-point mask)))
	    (remove-if #'null (loop for i from 0 below (length cuelist) collect
				    (let ((m (member (nth i sig) exclude :test #'equal)))
				      (if invert
					  (if m (nth i cuelist))
					(if (not m)(nth i cuelist))))))))
					  

	(defmethod duck ((cuelist list)(mask string) &key
			 (invert nil)(timesig nil) (use-subbeats nil) &allow-other-keys)
	  (let ((bc (bincue :timesig timesig :use-subbeats use-subbeats
			    :symbols (list (cons 'mask mask)))))
	    (duck cuelist (bincue-translate bc '(mask)) :invert invert)))


	(defmethod pprint-cuelist ((cuelist list) &key header &allow-other-keys)
	  (let ((max-line-length 8)
		(current-bar (caar cuelist))
		(line-length 0))
	    (print-header header)
	    (print-bar current-bar)
	    (dolist (q cuelist)
	      (if (not (equal (car q) current-bar))
		  (progn
		    (setf current-bar (car q))
		    (setf line-length 0)
		    (print-bar current-bar)))
	      (if (> line-length max-line-length)
		  (progn
		    (format t "~%;; ")
		    (setf line-length 0)))
	      (format t "~A" q)
	      (setf line-length (1+ line-length))))
	  (format t "~%"))

	(defmethod pprint-cuelist ((cuestring string) &key header (div 4) (bar-length 16) &allow-other-keys)
	  (print-header header)
	  (setf cuestring (compress cuestring))
	  (let ((bar-count 1))
	    (dotimes (i (length cuestring))
	      (if (zerop (rem i bar-length))
		  (progn 
		    (format t "~%;; BAR ~2D : " bar-count)
		    (setf bar-count (1+ bar-count))))
	      (if (zerop (rem i div)) (format t " "))
	      (format t "~A" (char cuestring i))))
	  (format t "~%"))
			 
	)



(setf (documentation 'duck 'function)
      "Remove cuelist elements which correspond to mask elements.

cuelist - List in 'BAR' format ((bar beat subbeat) ...)
mask    - Mask may take any of following forms:
          A) Cuelist in 'BAR' format.
          B) Binary string, white space is ignored.
             The length of the string is automatically padded 
             with zeros.
          C) An instance of the PART type.
             (Currently STRUMMER is not supported)
:invert - Boolean, it true invert selection.

Let C be the cuelist and M the mask. 
If invert is nil the result is:  C and (not M).
If invert is non-nil the result is: C and M.")
