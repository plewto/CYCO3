;;;; CYCO orchestra keynumber-map.lisp
;;;;
;;;; A keynumber-map is a function which maps an arbitrary value to a
;;;; MIDI keynumber.
;;;; 
;;;;
;;;;  basic-keynumber-map
;;;;     The default mapping with optional key range and transposition.
;;;;
;;;;  circular-keynumber-map
;;;;     Defined list of keynumber are accessed cyclically.
;;;;     klist = (W X Y)
;;;;     klist[0] --> W
;;;;     klist[1] --> X
;;;;     klist[2] --> Y
;;;;     klist[3] --> W
;;;;
;;;; symbolic-keynumber-map
;;;;   Defined symbols map to MIDI keynumbers, useful for use with percussion synths.
;;;;   kmap('kick)  --> 36
;;;;   kmap('snare) --> 42
;;;;
;;;;   With numeric arguments symbolic-keynumber-map acts like a circular map.
;;;;   kmap(0) --> 36
;;;;   kmap(1) --> 42
;;;;   kmap(2) --> 36
;;;;
;;;; pattern-keynumber-map
;;;;   Like symbolic-keynumber-map but values may be patterns.
;;;;
;;;; round-robin-keymap
;;;;
;;;; metronome-keynumber-map
;;;;   Highly specialized symbolic map for use with metronomes.
;;;;
;;;; These special argument values are defined for all keynumber-maps
;;;;  :doc   - prints documentation.
;;;;  :gamut - returns list of used keynumbers.
;;;;  :reset - resets internal patterns where used.
;;;;  'x     - default (first key assignment
;;;; 

(in-package :cyco)

(defun basic-keynumber-map (&key (min 0)(max 127)(transpose 0))
  (flet ((docfn ()
		(format t ";; Basic keynumber map,  Range [~3D,~3D] transpose ~D.~%"
			min max transpose)
		+rest+)
	 (gamutfn (mn mx transpose)
	 	  (keynumber (loop for i from mn to mx collect (+ i transpose))))
	 (warnfn (kn)
		 (progn 
		   (cyco-keynumber-warning (sformat "BASIC-KEYNUMBER-MAP  unknown keynumber ~A" kn))
		   +rest+)))
    #'(lambda (kn)
	(cond ((eq kn :doc)
	       (docfn))
	      ((eq kn :gamut)
	       (gamutfn min max transpose))
	      ((eq kn 'x)
	       (transpose min transpose))
	      ((keynumber-p kn)
	       (let ((kn2 (keynumber kn)))
		 (if (minusp kn2)
		     +rest+
		   (progn
		     (setf kn2 (transpose kn2 transpose))
		     (or (and (<= min kn2)(<= kn2 max) kn2)
			 +rest+)))))
	      (t (warnfn kn))))))


(constant +default-keynumber-map+ (basic-keynumber-map))


(defun wrapping-keynumber-map (&key (min 0)(max 127)(transpose 0))
  (flet ((gamutfn (mn mx transpose)
	 	  (keynumber (loop for i from mn to mx collect (+ i transpose))))
	 (docfn ()
		(format t ";; WRAPPING-KEYNUMBER-MAP Range [~3D,~3D] transpose ~D.~%"
			min max transpose)
		+rest+))
    #'(lambda (kn)
	(cond ((eq kn :doc)
	       (docfn))
	      ((eq kn :gamut)
	       (gamutfn min max transpose))
	      ((eq kn 'x)
	       (transpose min transpose))
	      ((keynumber-p kn)
	       (let ((kn2 (keynumber kn)))
		 (if (minusp kn2)
		     +rest+
		   (progn
		     (setf kn2 (transpose kn2 transpose))
		     (while (< kn2 min)(setf kn2 (+ kn2 12)))
		     (while (> kn2 max)(setf kn2 (- kn2 12)))
		     kn2))))))))


(defun circular-keynumber-map (start end)
 
  (flet ((gamutfn (mn mx transpose)
		  (keynumber (loop for i from mn to mx collect (+ i transpose))))
	 (docfn ()
		(format t ";; CIRCULAR-KEYNUMBER-MAP range (~A ~A)~%" start end)
		(let ((diff (- end start)))
		  (dotimes (i diff)
		    (format t ";;   [~3d] --> ~3d~%" i (+ start i))))
		(format t ";; ~%")
		+rest+))
    (let* ((kn1 (keynumber start))
	   (kn2 (keynumber end))
	   (offset (min kn1 kn2))
	   (delta (1+ (- (max kn1 kn2) offset)))
	   (fn #'(lambda (kn)
		   (cond ((eq kn :doc)
			  (docfn))
			 ((eq kn :gamut)
			  (gamutfn start end 0))
			 ((eq kn :reset)
			  +rest+)
			 ((eq kn 'x)
			  (keynumber start))
			 ((rest-p kn)
			  +rest+)
			 (t (let ((k (keynumber kn)))
			      (+ offset (rem k delta))))))))
      fn)))


(defun circular-list-keynumber-map (keylist)
  "Creates circular keynumber map over list of keynumbers."
  (flet ((docfn ()
		(format t ";; CIRCULAR-LIST-KEYNUMBER-MAP ~A~%" keylist)
		(dotimes (i (length keylist))
		  (format t ";;  [~3d] --> ~3d~%" i (nth i keylist)))
		(format t ";; ~%")
		+rest+) )
    #'(lambda (kn)
	(cond ((eq kn :doc)
	       (docfn))
	      ((eq kn :gamut)
	       (sort (remove-duplicates (keynumber keylist)) #'<))
	      ((eq kn :reset)
	       +rest+)
	      ((eq kn 'x)
	       (keynumber (car keylist)))
	      ((rest-p kn)
	       +rest+)
	      (t (cnth kn keylist))))))

(defun finite-list-keynumber-map (keylist)
  "Indexes into list for keynumber.
Out of bounds indexes return a +REST+"
  (flet ((docfn ()
		(format t ";; FINITE-LIST-KEYNUMBER-MAP ~A~%" keylist)
		(dotimes (i (length keylist))
		  (format t ";;  [~3d] --> ~3d~%" i (nth i keylist)))
		(format t ";;~%")
		+rest+))
    (let ((key-vector (->vector keylist))
	  (limit (length keylist)))
      #'(lambda (kn)
	  (cond ((eq kn :doc)(docfn))
		((eq kn :gamut)
		 (sort (remove-duplicates (keynumber keylist)) #'<))
		((eq kn :reset) +rest+)
		((rest-p kn) +rest+)
		((eq kn 'x)
		 (keynumber (first keylist)))
		((and (integerp kn)(>= kn 0)(< kn limit))
		 (aref key-vector kn))
		(t +rest+))))))


(defun symbolic-keynumber-map (assignments)
  (flet ((docfn ()
		(format t ";; Symbolic keynumber map~%")
		(let ((index 0))
		  (dolist (p assignments)
		    (format t ";;   [~3d] [~16A] --> ~3D~%" index (car p)(cdr p))
		    (setf index (1+ index))))
		(format t ";; ~%")
		+rest+)
	 (warnfn (kn)
		 (cyco-keynumber-warning (sformat "Unknown keynumber ~A" kn))
		 +rest+)
	 (gamutfn (htab)
		  (keynumber (remove-duplicates
			      (flatten
			       (loop for v being the hash-value in htab
				     collect (car v)))))) )

    (let* ((htab (alist->hash-table assignments (length assignments))))
      #'(lambda (kn)
	  (cond ((eq kn :doc)
		 (docfn)
		 +rest+)
		((eq kn :reset)
		 +rest+)
		((eq kn :gamut)
		 (gamutfn htab))
		((eq kn :assignments)
		 assignments)
		((rest-p kn)
		 +rest+)
		((eq kn 'x)
		 (keynumber (second (car assignments))))
		((integerp kn)
		 (keynumber (second (cnth kn assignments))))
		(t (let ((assignment (gethash kn htab)))
		     (if assignment
			 (keynumber (car assignment))
		       (warnfn kn)))))))))

		
(defun metronome-keynumber-map (&key (phrase 72)(bar 67)(beat 60))

  (let ((ktab (make-hash-table :size 3)))
    (setf (gethash :phrase ktab)(keynumber phrase))
    (setf (gethash :bar ktab)(keynumber bar))
    (setf (gethash :beat ktab)(keynumber beat))
    (flet ((docfn ()
		  (format t ";; Metronome keynumber map~%")
		  (format t ";;   :PHRASE  --> ~3D~%" (gethash 'phrase ktab))
		  (format t ";;   :BAR     --> ~3D~%" (gethash 'bar ktab))
		  (format t ";;   :BEAT    --> ~3D~%" (gethash 'beat ktab))
		  +rest+))
      (let ((fn #'(lambda (kn)
		    (cond ((eq kn :doc)
			   (docfn))
			  ((eq kn :gamut)
			   (sort (keynumber (list phrase bar beat)) #'<))
			  ((eq kn :reset)
			   +rest+)
			  (t (or (gethash kn ktab) +rest+))))))
	fn))))

(labels ((prefix-match-p (prefix key)
			 (let* ((sprefix (string-upcase (->string prefix)))
				(skey (string-upcase (->string key)))
				(pos (search sprefix skey)))
			   (and pos (zerop pos))))

	 (strip-prefix (key)
		       (let* ((skey (->string key))
			      (pos (position #\- skey))
			      (suffix nil))
			 (if pos
			     (progn 
			       (setf suffix (subseq skey (1+ pos)))
			       (->symbol suffix))
			   nil)))
	 
	 (find-matching-prefixes (prefix klist)
				 (let ((acc '()))
				   (dolist (item klist)
				     (if (prefix-match-p prefix (car item))
					 (push item acc)))
				   (reverse acc))) )

	(defun extract-sub-symbolic-keylist (prefix klist)
	  (let ((acc '()))
	    (dolist (item klist)
	      (let ((key (car item)))
		(if (prefix-match-p prefix key)
		    (cond ((eq prefix key)
			   (push (cons 'x (cdr item)) acc))
			  (t (push (cons (strip-prefix key)(cdr item)) acc))))))
	    (reverse acc))) )
	

(labels ((process-assignments (assignments)
			      (let ((htab (make-hash-table :size (length assignments))))
				(dolist (item assignments)
				  (let ((key (car item))
					(value (second item))
					(remarks (->string (or (third item) ""))))
				    (setf value (cond ((pattern-p value)
						       value)
						      ((atom value)
						       (line :of (->list value)))
						      (t (dice :of (->list value)))))
				    (setf (gethash key htab) (list value remarks))))
				htab))
	 (next-key (pat)
		   (cond ((null pat)
			  +rest+)
			 (t 
			  (next-1 pat))))

	 (docfn (assignments htab)
		(format t ";; PATTERN-KEYNUMBER-MAP~%")
		(loop for i from 0
		      for item in assignments do
		      (let* ((key (car item))
			     (value (gethash key htab))
			     (pattern (car value))
			     (remark (->string (or (second value) "")))
			     (elements (elements pattern)))
			(format t ";; [~2D] ~8A ~8A : ~16A ~A~%"
				i key (type-of pattern) elements remark)))
		+rest+)

	 ;; We call next an excessive number of times (50) to increase
	 ;; chance of capturing values from deeply nested or random
	 ;; patterns.   Would rather get all of the values the be efficient.
	 ;; Still there is no guarantee all values possible values will be
	 ;; captured.
	 (gamutfn (htab)
		  (let ((acc '()))
		    (loop for v being the hash-value of htab do
			  (push (next (car v) 50) acc))
		    (sort (remove-duplicates (keynumber (flatten acc))) #'<)))

	 )

	(defun pattern-keynumber-map (assignments)
	  (let ((htab (process-assignments assignments))
		(first-key (caar assignments)))
	    #'(lambda (kn)
		(cond
		 ((eq kn :reset)
		  (maphash #'(lambda (key value)
			       (declare (ignore key))
			       (reset (car value)))
			   htab)
		  +rest+)
		 ((eq kn :doc)
		  (docfn assignments htab))
		 
		 ((eq kn :gamut)
		  (gamutfn htab))
		 
		 ((eq kn 'x)
		  (next-key (car (gethash first-key htab))))
		      
		 ((numberp kn)
		  (let* ((key  (car (cnth kn assignments))))
		    (next-key (car (gethash key htab)))))

		 (t (next-key (car (gethash kn htab)))))))) )


(setf (documentation 'basic-keynumber-map 'function)
"Creates a basic keynumber-map
Keynumbers outside range (min max) return +REST+ 
The transpose amount is applied after the key range test.")

(setf (documentation 'wrapping-keynumber-map 'function)
"Similar to basic-keynumber-map but transposes out of bounds values as needed.
The transpose parameter is applied prior to the range-test.")

(setf (documentation 'circular-keynumber-map 'function)
 "Creates a circular keynumber map.
Keynumbers outside range (start end) are reflected back into the range.")

(setf (documentation 'symbolic-keynumber-map 'function)
"Creates a symbolic keynumber-map.
Symbolic maps are most useful with percussion instruments.
The assignments list has the form  ((sym1 . keynumber1)
                                    (sym2 . keynumber2)
                                    (sym3 . (keynumber  optional-remark))
                                     ..................)

For integer arguments the map functions as with circular-list-keynumber-map
The spacial symbol 'x returns the first keynumber in the list.

If the kenumber has the value :assignments, the keynumber-map
returns two values (+rest+ assignments) where the 2nd value is the 
assignment list.")

(setf (documentation 'metronome-keynumber-map 'function)
"Creates specialized symbolic keynumber-map for metronomes.
The map defines three event types:
:PHRASE - A strong accent on the first beat of the phrase.
:BAR    - A strong accent on the first beat of each bar, except the first.
:BEAT   - all other beats.")

(setf (documentation 'extract-sub-symbolic-keylist 'function)
"Extracts a sub-list from general symbolic keymap specification.
The intended use is to automatically generate sub-instrument
arguments for general percussion-kit instruments.
                                 
     (param drum-kit-keylist '((kick        . (20))
                               (kick-analog . (21))
                               (snare       . (30))
                               (snare-rim   . (31))
                               (snare-edge  . (32))))
                           
      (extract-sub-symbolic-key-list 'snare drum-kit-keylist)
                           
        --> ((x    30)
             (rim  31)
             (edge 32))
                        
The resulting list is suitable for use with symbolic-keynumber-list")


(setf (documentation 'pattern-keynumber-map 'function)
"A PATTERN-KEYNUMBER map is similar to a SYMBOLIC-KEYNUMBER-MAP but allows 
keynumber patterns. The general form of the assignments list is:

      ((key-1 pattern-1 optional-remarks)
       (key-2 pattern-2 optional-remarks)
        ...
       (key-n pattern-n optional-remarks))

Where keys and remarks have the same usage as symbolic-keynumber-map and 
pattern may be any of the following forms:

      1) A single keynumber - 
      2) A list of keynumbers - converted to a dice pattern
      3) A pattern - used directly

      (param kmap (pattern-keynumber-map 
                    (list '(ape a4)
                          '(bat (30 40 50))
                          (list 'cat (cycle :of '(60 70))))))

      (funcall kmap 'ape)   --> 57  ;; 57 = A4
      (funcall kmap 'bat)   --> 40  ;; selects 30, 40 or 50 
      (funcall kmap 'bat)   --> 30  ;; at random
      (funcall kmap 'bat)   --> 30
      (funcall kmap 'cat)   --> 60  ;; returns 60 or 70 alternately
      (funcall kmap 'cat)   --> 70
      (funcall kmap 'cat)   --> 60

      Unrecognized keys return -1 (a rest) without warning.

      The following special keys are predefined.

      'x       returns the default (first) key 
               (funcall kmap 'x) --> 57
       
      number   behaves like a circular-keymap
               (funcall kmap 0) --> 57
               (funcall kmap 4) --> 40  

      :gamut   returns list of possible results.
               WARNING: gamut does not work properly for nested patterns.
               (funcall kmap :gamut) -> (30 40 50 57 60 70)

      :reset   Resets internal patterns, returns -1
 
      :doc     Prints documentation.")
