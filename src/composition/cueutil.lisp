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
				  (subseq cp 0 width)))) )

	(defun mask-cuelist (cuelist mask &key timesig invert (use-subbeats t))
	  (let* ((reject-token (if invert #\1 #\0))
		 (tsig (select-time-signature timesig))
		 (gamut (cue-gamut tsig (not use-subbeats)))
		 (bits (pad-zeros (compress mask)(length gamut)))
		 (reject-list (remove-if #'null (loop for i from 0 below (length gamut) collect
						      (let ((flag (char bits i)))
							(if (char= flag reject-token)
							    (nth i gamut))))))
		 (reject-p #'(lambda (cp)
			       (member (simplify-cue-point cp) reject-list :test #'equal))))
	    (remove-if reject-p cuelist)))

	(defmethod duck ((cuelist list)(mask list) &key (invert nil))
	  (let ((sig (mapcar #'simplify-cue-point cuelist))
		(exclude (mapcar #'simplify-cue-point mask)))
	    (remove-if #'null (loop for i from 0 below (length cuelist) collect
				    (let ((m (member (nth i sig) exclude :test #'equal)))
				      (if (or (and invert m)(not m))
					  (nth i cuelist)))))))

	
	(defmethod duck ((cuelist list)(mask string) &key (invert nil))
	  (let* ((sig (mapcar #'simplify-cue-point cuelist))
		 (msk (pad-zeros mask (length sig)))
		 (accept (if invert #\1 #\0))
		 (acc '()))
	    (dotimes (i (length sig))
	      (if (char= (char msk i) accept)
		  (push (nth i sig) acc)))
	    (reverse acc))) ) 

(setf (documentation 'mask-cuelist 'function)
      "Produces new cuelist by applying mask to to cuelist elements.
cuelist  - A cuelist in 'BAR' format  (bar beat subbeat ...) 
mask     - A string of binary digits.  Embedded white-space is ignored.
           The mask is automatically right-padded with 0s so that its
           length matches (cue-points time-signature)
:timesig - Reference time-signature, see SELECT-TIME-SIGNATURE
           Defaults to current-section of *project* if defined.
:invert  - Boolean, if true invert mask values.
:use-subbeats - If true use time-signature subbeats as rhythmic unit,
                otherwise use tsubbeats. Default t.

Only cuelist elements with corresponding mask '1' are included in the result. 


(mask-cuelist '((1 1 1)(1 2 1)(1 3 1)(1 4 1)) ''1010'') --> ((1 1 1)(1 3 1))")


(setf (documentation 'duck 'function)
      "Remove cuelist elements which correspond to mask elements.

cuelist - List in 'BAR' format ((bar beat subbeat) ...)
mask    - Mask may take any of following forms:
          A) Cuelist in 'BAR' format.
          B) Binary string, white space is ignored.
             The length of the string is automatically padded 
             with zeros.
          C) An instance of the PART type.
             (Currently STRUMMER is not suported)
:invert - Boolean, it true invert selection.

Let C be the cuelist and M the mask. 
If invert is nil the result is:  C and (not M).
If invert is non-nil the result is: C and M.")


