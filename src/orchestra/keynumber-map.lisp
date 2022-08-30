;;;; CYCO orchestra keynumber-map.lisp
;;;;
;;;; A keynumber-map is a single-argument function which maps an arbitrary
;;;; value to a MIDI keynumber.  The following factory functions return
;;;; various keynumber-map functions.  All keynumber maps should have the
;;;; following properties: 
;;;;
;;;; Handle special arguments:
;;;;   1) :doc   -> display map documentation.
;;;;   2) :gamut -> return list of keynumbers the map may return.
;;;;                The list should be of unique MIDI key numbers 
;;;;                sorted in ascending order.
;;;;  3) :reset  -> reset internal structures, if any.
;;;;  4) 'x      -> return a default value.
;;;;  5) 'r      -> return a rest.
;;;;
;;;; Otherwise map the argument to some MIDI keynumber or +rest+.
;;;; Unrecognized arguments should return +rest+. 
;;;; If *ENABLE-KEYNUMBER-WARNINGS* is true, a warning message is
;;;; displayed for unrecognized arguments.
;;;;

(in-package :cyco)

(param *enable-keynumber-warnings* t)

(defun key-warnings-on ()
  (setf *enable-keynumber-warnings* t))

(defun key-warnings-off ()
  (setf *enable-keynumber-warnings* nil))


(labels ((key-warning (map-type value)
		      (if *enable-keynumber-warnings*
			  (progn
			    (format t "KEYNUMBER WARNING: ***********************~%")
			    (format t "KEYNUMBER WARNING: ~A Unknown keynumber:  ~A" map-type value)))
		      +rest+)

	 (key-gamut (keylist)
		    (sort (remove-duplicates (keynumber keylist)) #'<))


	 (symbolic-doc-function (map-type assignments)
				(format t ";; ~A~%" map-type)
				(loop for i from 0 for item in assignments do
				      (let ((key (car item))
					    (value (second item))
					    (remark (third item)))
					(format t ";;   [~3D] ~8A -> " i key)
					(cond  ((pattern-p value)
						(format t "~A " (->string value)))
					       ((atom value) (format t "~4A " value))
					       ((listp value) (format t "~A " value))
					      (t "??? "))
					(if remark
					    (format t "REM: ~A" remark))
					(format t "~%"))))
					       
				      
	 (process-pattern-assignments (assignments)
				      (loop for item in assignments collect
				      	    (let* ((key (car item))
				      		   (value (second item))
				      		   (remark (third item))
				      		   (pat (cond
							 ((pattern-p value) value)
							 ((listp value)(dice :of (keynumber value)))
							 ((atom value)(line :of (keynumber value)))
							 (t (cyco-error (sformat "Invalid PATTERN-KEYNUMBER-MAP assignment")
									(sformat "key   : ~A" key)
									(sformat "value : ~A" value))))))
				      	      (list key pat remark))))
	 (pattern-gamut-function (alist)
				 (key-gamut
				  (let ((sample 50))
					; sample sets number of values to
					; extract from current pattern. 
					; The value is overkill to increase
					; Chance of collecting all possible
					; values.   Better to get all values
					; then worry about efficiency.
				    (flatten (loop for item in alist collect
						   (next (second item) sample)))))) )

	(defun basic-keynumber-map (&key (min 0)(max 127)(transpose 0))
	  (let ((min-key (keynumber min))
		(max-key (keynumber max)))
	    (list min-key max-key transpose)
	    #'(lambda (kn)
	       (cond ((eq kn :doc)
	    	      (format t ";; BASIC-KEYNUMBER-MAP ~A .. ~A  transpose ~A~%~%"
	    		      min-key max-key transpose))
	    	     ((eq kn :gamut)
	    	      (loop for k from min-key to max-key collect k))
	    	     ((eq kn :reset) nil)
	    	     ((eq kn 'x) min-key)
	    	     ((rest-p kn) +rest+)
	    	     ((keynumber-p kn)
		      (let ((n (keynumber kn)))
			(if (and (>= n min-key)(<= n max-key))
			    (transpose n transpose)
			  +rest+)))
	    	     (t (key-warning 'basic-keynumber-map kn)
			+rest+)))))

	(defun circular-list-keynumber-map (keylist &optional (map-type 'circular-list-keynumber-map))
	  (setf keylist (keynumber keylist))
	  #'(lambda (kn)
	      (cond ((eq kn :doc)
		     (format t ";; ~A~%" map-type)
		     (loop for i from 0 below (length keylist) do
			   (format t ";;   [~3D] --> ~3D~%" i (nth i keylist)))
		     (format t "~%"))
		    ((eq kn :gamut)(key-gamut keylist))
		    ((eq kn :reset) nil)
		    ((eq kn 'x)(car keylist))
		    ((rest-p kn) +rest+)
		    ((keynumber-p kn)
		     (cnth (keynumber kn) keylist))
		    (t (key-warning 'circular-keynumber-map kn)
		       +rest+))))
		    
	(defun circular-keynumber-map (start end)
	  (circular-list-keynumber-map
	   (if (> end start)
	       (range start (1+ end))
	     (range start (1- end)))
	   'circular-keynumber-map))
		     
	(defun finite-list-keynumber-map (keylist)
	  (setf keylist (keynumber keylist))
	  #'(lambda (kn)
	      (cond ((eq kn :doc)
		     (format t ";; FINITE-LIST-KEYNUMBER-MAP~%")
		     (loop for i from 0 for v in keylist do
			   (format t ";;   [~3D] -> ~3D~%" i v))
		     (format t "~%"))
		    ((eq kn :gamut)(key-gamut keylist))
		    ((eq kn :reset) nil)
		    ((eq kn 'x)(car keylist))
		    ((rest-p kn) +rest+)
		    ((keynumber-p kn)
		     (let ((n (keynumber kn)))
		       (or (and (< n (length keylist))(nth n keylist))
			   +rest+)))
		    (t (key-warning 'finite-list-keynumber-map kn)
		       +rest+))))

	;; NOTE for key range less then 1-octave result may be outside range [start..end]
	;;
	(defun wrapping-keynumber-map (start end)
	  (let* ((min (keynumber start))
		 (max (keynumber end))
		 (wrap-fn #'(lambda (n)
			      (while (< n min) (setf n (+ n 12)))
			      (while (> n max) (setf n (- n 12)))
			      n)))
	  #'(lambda (kn)
	      (cond ((eq kn :doc)
		     (format t ";; WRAPPING-KEYNUMBER-MAP ~A .. ~A~%" start end))
		    ((eq kn :gamut)
		     (key-gamut (loop for i from 0 below 128 collect (funcall wrap-fn i))))
		    ((eq kn :reset) nil)
		    ((eq kn 'x) start)
		    ((rest-p kn) +rest+)
		    ((keynumber-p kn)
		     (funcall wrap-fn (keynumber kn)))
		    (t (key-warning 'wrapping-keynumber-map kn)
		       +rest+)))))

	(defun symbolic-keynumber-map (assignments &optional (map-type 'symbolic-metronome-map))
	  (let ((htab (alist->hash-table assignments (length assignments))))
	    #'(lambda (kn)
		(cond ((eq kn :doc)
		       (symbolic-doc-function map-type assignments))
		      ((eq kn :gamut)
		       (key-gamut (loop for v being the hash-value of htab
					collect (keynumber (car v)))))
		      ((eq kn :reset) nil)
		      ((eq kn 'x)(keynumber (second (car assignments))))
		      ((rest-p kn) +rest+)
		      ((integerp kn)
		       (keynumber (second (cnth kn assignments))))
		      (t (let ((value (gethash kn htab)))
			   (or (and value (keynumber (car value)))
			       (key-warning map-type kn))))))))

	(defun metronome-keynumber-map (&key (phrase 72)(bar 67)(beat 60))
	  (symbolic-keynumber-map (list (list :phrase (keynumber phrase))
					(list :bar (keynumber bar))
					(list :beat (keynumber beat)))
				  'metronome-keynumber-map))

	(defun pattern-keynumber-map (assignments &optional (map-type 'pattern-keynumber-map))
	  (let ((alist (process-pattern-assignments assignments)))
	    #'(lambda (kn)
		(cond ((eq kn :doc)(symbolic-doc-function map-type alist))
		      ((eq kn :gamut)(pattern-gamut-function alist))
		      ((eq kn :reset)(loop for item in alist do (reset (second item))))
		      ((eq kn 'x)(keynumber (next-1 (second (car alist)))))
		      ((eq kn 'r) +rest+)
		      ((integerp kn)(keynumber (next-1 (second (cnth kn alist)))))
		      (t (let ((value (second (assoc kn alist))))
			   (or (and value
				    (keynumber (next-1 value)))
			       (prog1
				   +rest+
				 (key-warning map-type kn)))))))))

	(defun round-robin-keymap (assignments)
	  (pattern-keynumber-map (loop for a in assignments collect
				       (let ((key (car a))
					     (value (second a))
					     (remark (third a)))
					 (cond ((listp value)
						(list key (cycle :of value) remark))
					       (t (list key value remark)))))
				 'round-robin-keymap)) )

(constant +default-keynumber-map+ (basic-keynumber-map))
 
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

(let* ((general-docs "

All keynumber maps take the following special arguments
                                              
    :doc   -> prints documentation to terminal.
    :gamut -> returns sorted list of possible returned keynumbers.
    :reset -> resets internal states, if any.
    'x     -> returns default keynumber.
    'r     -> returns +REST+.
")
       (general-symbolic-docs (str+ general-docs "
                                                                  
    Behaves like circular-list-keynumber-map for numeric arguments.
")))


      (setf (documentation 'basic-keynumber-map 'function)
      (str+ "Returns a numeric based keynumber map.  Optional key range and
      transposition may be used.   Transposition is applied after the
      key-range test.   Out of bounds keys return +rest+

      (param kmap (basic-keynumber-map :min 60 :max 72 :transpose 7))
      (funcall kmap 59)   -> -1
      (funcall kmap 60)   -> 67
      (funcall kmap 72)   -> 72
      (funcall kmap 80)   -> -1" general-docs))

      (setf (documentation 'circular-list-keynumber-map 'function)
      (str+ "Returns key mapping function which returns values from a circular list. 
      
       (param kmap (circular-list-keynumber-map '(30 31 32)))
       (funcall kmap 0) -> 30
       (funcall kmap 1) -> 31
       (funcall kmap 2) -> 32
       (funcall kmap 3) -> 30" general-docs))

      (setf (documentation 'circular-keynumber-map 'function)
      (str+ "Short cut for (circular-list-keynumber-map (range start (1+ end)))"))


      (setf (documentation 'finite-list-keynumber-map 'function)
      (str+ "Returns key function similar to circular-list-keynumber-map 
       except indexes are not circular.   Out of bounds indexes return 
       +rest+.

       (param foo (finite-list-keynumber-map '(10 20 30)))
       (funcall foo 0) -> 10
       (funcall foo 1) -> 20
       (funcall foo 2) -> 30
       (funcall foo 3) -> -1" general-docs))

      (setf (documentation 'wrapping-keynumber-map 'function)
      (str+ "Returns key mapping function which wraps out of bounds values.
       
       (param foo (wrapping-keynumber-map 60 71))
       (funcall foo 59) -> 71
       (funcall foo 60) -> 60
       (funcall foo 61) -> 61
        ..............     
       (funcall foo 70) -> 70
       (funcall foo 71) -> 71
       (funcall foo 72) -> 60 
       (funcall foo 73) -> 61

       Wrapping works best if the start end range is in perfect octaves
       minus 1,   I.E.  11, 23, 35 ... etc.  

       For non-octave ranges the results may fall slightly outside the
       defined range, they will always be pitch-equivalent." general-docs) )

      (setf (documentation 'symbolic-keynumber-map 'function)
      (str+ "Returns key mapping from arbitrary symbol to MIDI note number.  
       
       (param foo (symbolic-keynumber-map '((bass  36 'optional-remark)
                                            (rim   37 'optional-remark)
                                            (snare 38))))

       (funcall foo 'bass)   -> 36
       (funcall foo 'rim)    -> 37" general-symbolic-docs))

      (setf (documentation 'metronome-keynumber-map 'function)
      (str+ "Special case symbolic key map with the following keys
       :phrase - beep key number at start of phrase
       :bar    - key number for first beat of each bar 
                 (except for the first bar of the phrase)
       :beat   - key number for all other beats." general-symbolic-docs))

      (setf (documentation 'pattern-keynumber-map 'function)
      (str+ "Returns a generalized symbolic-map.  Where a symbolic-keynumber-map
      returns single key-numbers, a pattern map uses patterns to select a
      key.  This allows a pattern map to return different keynumbers for
      the same argument. 

      (param foo (pattern-keynumber-map (list '(a 34 optional-remark)
                                              '(b (40 41 52))
                                               (list 'c (cycle :of '(60 64 67))))))

      For the above map 
         'a always maps to 34, just like a normal symbolic map.
         'b is converted to a dice pattern and returns 40, 41 or 52 at random.
         'c returns 60, 64 and 67 in a cycle.

     (funcall foo 'a)   -> 34
     (funcall foo 'b)   -> 52
     (funcall foo 'b)   -> 41
     (funcall foo 'c)   -> 60
     (funcall foo 'c)   -> 64
     (funcall foo 'c)   -> 67
     (funcall foo 'c)   -> 60" general-symbolic-docs))

      (setf (documentation 'round-robin-keymap 'function)
      (str+ "Returns specialized pattern keymap, instead of converting list to a 
       dice pattern, they are converted to a cycle.

       (param foo (round-robin-keymap '((a (34 35) 'optional-remark)
                                        (b (40 41 42)))))


     (funcall foo 'a)   -> 34
     (funcall foo 'a)   -> 35
     (funcall foo 'b)   -> 40
     (funcall foo 'b)   -> 41
     (funcall foo 'b)   -> 42
     (funcall foo 'b)   -> 40" general-symbolic-docs))) 
     
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
