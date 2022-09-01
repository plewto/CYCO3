;;;; cyco composition binball.lisp
;;;;
;;;; Defines combined structures for BINCUE+QBALL and BINCUE+XBALL.
;;;;

(in-package :cyco)

(labels ((binball-duck (cuelist duck-param tsig use-subbeats)
		       (let* ((p (->list duck-param))
			      (mask (car p))
			      (invert (second p))
			      (rs (duck cuelist mask :invert invert
					:timesig tsig :use-subbeats use-subbeats)))
			 rs))

	 ;; cuelist: (euclid points rotation invert)
	 ;;
	 (eu1 (cuelist tsig use-subbeats)
	      (euclid (second cuelist)
		      :rot (third cuelist)
		      :invert (fourth cuelist)
		      :timesig tsig
		      :use-subbeats use-subbeats))

	 ;; cuelist: (euclid2 split (P1 R1 I1)(P2 R2 I2))
	 ;;
	 (eu2 (cuelist tsig use-subbeats)
	      (let ((split (second cuelist))
		    (e1 (third cuelist))
		    (e2 (fourth cuelist)))
		(euclid2 (car e1)(car e2)
			 :r1 (second e1) :r2 (second e2)
			 :inv1 (third e1) :inv2 (third e2)
			 :split split
			 :timesig tsig
			 :use-subbeats use-subbeats))) )

	(defun binball-cuelist (cuelist bincue time-signature use-subbeats duck)
	  "Helper function for binball and binxball macros."
	  (let* ((tsig (select-time-signature time-signature))
		 (clist (cond ((stringp cuelist)
			       (bincue-translate bincue cuelist))
			      ((eq (car cuelist) 'euclid)
			       (eu1 cuelist tsig use-subbeats))
			      ((eq (car cuelist) 'euclid2)
			       (eu2 cuelist tsig use-subbeats))
			      (t (bincue-translate bincue cuelist)))))
	    (bincue-translate bincue (binball-duck clist duck tsig use-subbeats)))) )
	

(defmacro binball (name instruments &key
			section shuffle shift tempo unit bars beats
			subbeats render-once transposable reversible 
			cue key dur amp reset-on-repeat duck
			remarks symbols (use-subbeats t))
  `(let ((parent (or ,section (property *project* :current-section))))
     (part-banner (name parent) ',name)
     (let* ((tsig (if (or ,bars ,beats ,subbeats)
		      (time-signature :bars (or ,bars (bars parent))
				      :beats (or ,beats (beats parent))
				      :subbeats (or ,subbeats (subbeats parent)))
		    parent))
	    (bincue (bincue :symbols ,symbols :timesig tsig :use-subbeats ,use-subbeats))
	    (bar-cuelist (binball-cuelist ,cue bincue tsig ,use-subbeats ,duck))
	    (qb (make-qball ',name ,instruments
			    :section parent
			    :cuefn *default-cue-function*
			    :shuffle (or ,shuffle #'no-shuffle)
			    :shift (or ,shift 0)
			    :render-once ,render-once
			    :transposable ,transposable
			    :reversible ,reversible
			    :tempo ,tempo
			    :unit ,unit
			    :bars ,bars
			    :beats ,beats
			    :subbeats ,subbeats
			    :cue bar-cuelist
			    :key (or ,key '(60))
			    :dur (or ,dur '(q))
			    :amp (or ,amp '(fff))
			    :reset-on-repeat ,reset-on-repeat
			    :remarks (->string (or ,remarks "")))))
       (defparameter ,name qb)
       qb)))

		      
(defmacro binxball (name instrument &key
			 section shuffle shift render-once tempo unit bars beats
			 subbeats transposable reversible chord-model cue key
			 dur amp chord inversion octave strum end-together
			 direction reset-on-repeat duck
			 symbols remarks (use-subbeats t))
  `(let ((parent (or ,section (property *project* :current-section))))
     (part-banner (name parent) ',name)
     (let* ((tsig (if (or ,bars ,beats ,subbeats)
		      (time-signature :bars (or ,bars (bars parent))
				      :beats (or ,beats (beats parent))
				      :subbeats (or ,subbeats (subbeats parent)))
		    parent))
	    (bincue (bincue :symbols ,symbols :timesig tsig :use-subbeats ,use-subbeats))
	    (bar-cuelist (binball-cuelist ,cue bincue tsig ,use-subbeats ,duck))
	    (xb (make-xball ',name ,instrument
			    :section parent
			    :cuefn *default-cue-function*
			    :bars ,bars
			    :beats ,beats
			    :subbeats ,subbeats
			    :tempo ,tempo
			    :unit ,unit
			    :shuffle (or ,shuffle #'no-shuffle)
			    :shift (or ,shift 0)
			    :render-once ,render-once
			    :transposable ,transposable
			    :reversible ,reversible
			    :chord-model (or ,chord-model *chord-table*)
			    :cue bar-cuelist
			    :key (or ,key '(60))
			    :dur (or ,dur '(q))
			    :amp (or ,amp '(fff))
			    :chord (or ,chord '([solo]))
			    :inversion (or ,inversion '(0))
			    :octave (or ,octave '(0))
			    :strum (or ,strum '(0.01))
			    :end-together (or ,end-together nil)
			    :direction (or ,direction '(down))
			    :reset-on-repeat ,reset-on-repeat
			    :remarks (->string (or ,remarks "")))))
       (defparameter ,name xb)
       xb)))
				 
       
(setf (documentation 'binball 'function)
      "Creates QBALL bound to symbol name using 'binary' cuelist.

name             - Symbol (do not quote)
instruments      - Pattern of instruments, as per QBALL.
:section         - Parent Section, as per QBALL.
:shuffle         - Shuffle function, as per QBALL.
:shift           - Time shift, as per QBALL.
:tempo           - Tempo, as per QBALL.
:unit            - Time-signature unit, as per QBALL.
:bars            - Time-signature bar count, as per QBALL.
:beats           - Time-signature beat count, as per QBALL.
:subbeats        - Time-signature subbeats count, as per QBALL.
:render-once     - Boolean flag, as per QBALL.
:transposable    - Transposable flag, as per QBALL.
:reversible      - Reversible flag, as per QBALL.
:reset-on-repeat - Boolean, as per QBALL.
:key             - Key pattern, as per QBALL.
:dur             - Duration pattern, as per QBALL.
:amp             - Dynamic pattern, as per QBALL.


:symbols         - User defined cue symbols, as per BINCUE
:cue             - The cue list may take any of the following forms.
                   A) A list of binary tokens as per BINCUE.

                   B) A list of form (EUCLID POINTS ROTATION INVERT)
                      Generates Euclidean rhythm. The Euclidean length is 
                      automatically determined by the time-signature.  Where

                      POINTS is number of events and may either be: 
                        1) integer (absolute)   0 < points <= length.
                        2) float (relative to length),  0.0 < points <= 1.0
                        3) nil, use random point count.

                      rot is number of steps to rotate rhythm and may be:
                        1) integer (absolute) 0 <= rot <= length, default 0.
                        2) float (relative) 0.0 <= rot <= 1.0
                        3) nil, random.

                      INVERT is Boolean, if t invert selected events. Default nil.

                   C) A list of form: (EUCLID2 SPLIT (P1 R1 I1)(P2 R2 I2))
                      Splices 2 Euclidean rhythms. 
                      The sub list (P1 R1 I1) and (P2 R2 I2) are the 
                      points, rot and invert parameters for the two rhythms.
                      
                      Split defines where the splice is made and is always at 
                      a bar boundary.  Split may be
                        1) integer (absolute) bar number  0 < split <= bar-count.
                        2) float, relative to bar-count  0.0 < split <= 1.0
                        Defaults to 0.5

:use-subbeats    - Boolean, if true use subbeats as the basic time unit, 
                   otherwise use tsubbeats.
:duck            - Uses another cuelist or part to duck events.  
                   duck has the form (mask invert)  where
 
                   mask is one of:
                      1) A cuelist in 'BAR' format ((bar beat subbeat) ...)
                      2) A 'binary' cuelist as a string ''0011 0110 ...''  
                         white space is ignored.  Non-specified bits are treated 
                         as 0.
                      3) An instance of PART.

                  Normally (with invert nil) events which are common to both
                  the main cuelist and mask are suppressed.  C ^ (not MASK) --> accept

                  If invert is true then cuelist events which are not in mask 
                  are suppressed.   C ^ MASK --> accept.

:remarks         - Optional remarks text.")


(setf (documentation 'binxball 'function)
      "Creates XBALL bound to name and using 'binary' cuelist.

name             - Symbol (do not quote)
instrument       - Instance of Instrument
                   Like XBALL, BINXBALL may only use a single instrument.
:section         - As per XBALL.
:shuffle         - As per XBALL.
:shift           - As per XBALL.
:tempo           - As per XBALL.
:unit            - As per XBALL.
:bars            - As per XBALL.
:beats           - As per XBALL.
:subbeats        - As per XBALL.
:render-once     - As per XBALL.
:reset-on-repeat - As per XBALL.
:transposable    - As per XBALL.
:reversible      - As per XBALL.
:chord-model     - As per XBALL.
:key             - As per XBALL.
:dur             - As per XBALL.
:amp             - As per XBALL.
:chord           - As per XBALL.
:inversion       - As per XBALL.
:octave          - As per XBALL.
:strum           - As per XBALL.
:end-together    - As per XBALL.
:direction       - As per XBALL.
:remarks         - As per XBALL.
:symbols         - Symbol list, as per BINCUE.
:use-subbeats    - Boolean, as per BINCUE.
:cue             - As per BINBALL.
:duck            - As per BINBALL.")
