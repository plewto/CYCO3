;;;; CYCO parts istrum
;;;;
;;;; Provides Implicit events from strummer.
;;;;


(in-package :cyco-part)

(labels (;; Returns -/+ integer starting at position start of string.
	 ;; Returns nil if invalid.
	 ;;
	 (parse-int (str &optional (start 0))
		    (let ((digits '(#\0 #\1 #\2 #\3 #\4 #\5
				    #\6 #\7 #\8 #\9 #\- #\+)) 
			  (v (subseq str start)))
		      (if (every #'(lambda (n)
				     (member n digits :test #'char=))
				 v)
			  (parse-integer v)
			nil)))

	 (is-signed-p (str)
		    (and (plusp (length str))
			 (let ((c (char str 0)))
			   (or (char= c #\-)
			       (char= c #\+)))))
	 
	 (increment-time (previous-1 previous-2 cue-gamut)
			 (let ((pos-1 (position previous-1 cue-gamut :test #'equal))
			       (pos-2 (position previous-2 cue-gamut :test #'equal)))
			   (if (and pos-1 pos-2 (>= pos-1 pos-2))
			       (let* ((diff (- pos-1 pos-2))
				      (new-pos (+ diff pos-1)))
				 (nth new-pos cue-gamut))
			     nil)))

	 (parse-time (token previous previous-2 cue-gamut)
		     (cond ((eq token '=)
			    previous)
			   ((eq token '+)
			    (or (increment-time previous previous-2 cue-gamut)
				previous))
			   (t token)))

	 (parse-keynumber (token previous)
			  (let* ((name (name token))
				 (tail (char (reverse name) 0))
				 (sign (cond ((char= tail #\-) -1)
					     ((char= tail #\+) +1)
					     (t nil))))
			    (cond ((eq token '=)
				   previous)
				  ((and sign (keynumber-p previous))
				   (let ((n (parse-int (subseq name 0 (1- (length name))))))
				     (or (and (numberp n)
					      (keyname (transpose previous (* sign n))))
					 (cyco-error "Expected ISTRUM key transpose"
						     (sformat "GOT ~A" token)))))
				  (t
				   token))))
	 
	 ;; Chords recognized as either a member of the chord-model or a list.
	 ;; ISSUE: It would be nice to check that any list only contains
	 ;;        integers.  For some presently unknown reason trying
	 ;;        (every #'integerp token) crashes!
	 ;;
	 (is-chord-p (token chord-model)
		     (and (or (and (listp token)
				   (plusp (length token)))
			      (defines-chord-p chord-model (->cyco-symbol token)))
			  token))

	 (parse-chord-inversion (token)
				(or (and (eq token :inv-off) 0)
				    (let ((name (name token)))
				      (if (and (> (length name) 3)
					       (string= (subseq name 0 3) "INV"))
					  (let ((svalue (subseq name 3)))
					    (or (and (string= svalue "OFF") 0)
						(parse-int svalue)
						(cyco-error
						 "ISTRUM expected integer chord interval"
						 (sformat "Got: ~A" token))))))))
	 
	 (inversion-p (token)
		      (let ((name (name token)))
			(and (> (length name) 4)
			     (string= (subseq name 0 4) "INV-"))))
				      

	 (parse-chord-octave (token)
			     (cond ((eq token :oct+) 1)
				   ((eq token :oct-) -1)
				   ((eq token :oct-off) 0)
				   (t nil)))

	 (parse-chord-direction (token)
				(let ((conversion '((:up . up)
						    (:down . down)
						    (:rand . random)
						    (:rand-up . dice)
						    (:up-down . (up down))
						    (:down-up . (down up)))))
				  (cdr (assoc token conversion))))
	 ;; :prog-xxx
	 (parse-program (token)
	 		(let ((name (name token)))
	 		  (if (and (> (length name) 5)
	 			   (string= (subseq name 0 5) "PROG-"))
			      (let ((value (parse-int name 5)))
				(or value
				    (->cyco-symbol (subseq name 5))))))) )

	 
	;; Translates istrum event-list to strummer event-list.
	;;
	(defun istrum->events (i-events timesig chord-model)
	  (let* ((cue-gamut (cue-gamut timesig))
		 (s-events '())
		 (previous-time-2 nil)
		 (previous-time-1 nil)
		 (previous-key 60))
	    (dolist (i-event (->list i-events))
	      (let* ((time (parse-time (car i-event) previous-time-1 previous-time-2 cue-gamut))
		     (key (parse-keynumber (second i-event) previous-key))
		     (s-event (list :time time :key key)))
		(setf previous-time-2 previous-time-1)
		(setf previous-time-1 time)
		(setf previous-key key)
		(dolist (token (cddr i-event))
		   (let ((chord (is-chord-p token chord-model))
			(inversion (parse-chord-inversion token))
			(octave (parse-chord-octave token))
			(direction (parse-chord-direction token))
			(strum (and (floatp token) token))
			(amp (and (symbolp token)(dynamic-p token) token))
			(dur (and (symbolp token)(metric-expression-p token) token))
			(program (parse-program token))
			(etog (eq token :etog))
			(etog-off (eq token :etog-off)))
		    (cond (chord (setf s-event (append s-event (list :chord chord))))
		    	  (inversion (setf s-event (append s-event (list :inversion inversion))))
		    	  (octave (setf s-event (append s-event (list :octave octave))))
		    	  (direction (setf s-event (append s-event (list :direction direction))))
		    	  (strum (setf s-event (append s-event (list :strum strum))))
		    	  (amp (setf s-event (append s-event (list :amp amp))))
		    	  (dur (setf s-event (append s-event (list :dur dur))))
			  (program (setf s-event (append s-event (list :program program))))
		    	  (etog (setf s-event (append s-event (list :end-together t))))
		    	  (etog-off (setf s-event (append s-event (list :end-together nil))))
		    	  (t (cyco-error "Invalid ISTRUM event"
					 (sformat "Event ~A  at token: ~A" i-event token))))))
		(push s-event s-events)))
	    (reverse s-events)))
		
		     
	(defun make-istrum (name instrument &key
                                  section cuefn shuffle shift 
                                  tempo unit bars beats subbeats
                                  render-once transposable reversible
                                  chord-model remarks hold events form)
	  (let* ((strummer (make-eventless-strummer name instrument
						    :section section
						    :cuefn cuefn
						    :shuffle shuffle
						    :shift  shift 
						    :tempo  tempo 
						    :unit  unit 
						    :bars  bars 
						    :beats  beats 
						    :subbeats subbeats
						    :render-once render-once
						    :transposable transposable
						    :reversible reversible
						    :chord-model chord-model
						    :hold hold
						    :remarks remarks))
		 (s-events (istrum->events events strummer (property strummer :chord-model))))
	    (setf (strummer-states strummer)
		  (process-strummer-states strummer s-events))
	    (validate-render strummer)
	    (put strummer :cuelist (extract-strummer-cuelist s-events))
	    (reset strummer)
	    (cond ((eq form :events) s-events)
		  (t strummer)))) )


(defmacro istrum (name instrument &key
		       section cuefn shuffle shift 
		       tempo unit bars beats subbeats
		       render-once transposable reversible
		       chord-model remarks hold events)
  `(progn
     (part-banner (name ,section) ',name)
     (let ((new-istrum (make-istrum ',name ,instrument
				    :section ,section 
				    :cuefn ,cuefn 
				    :shuffle ,shuffle 
				    :shift ,shift 
				    :tempo ,tempo 
				    :unit ,unit 
				    :bars ,bars 
				    :beats ,beats 
				    :subbeats,subbeats
				    :render-once ,render-once 
				    :transposable ,transposable 
				    :reversible ,reversible 
				    :chord-model ,chord-model 
				    :remarks ,remarks
				    :hold ,hold
				    :events ,events)))
       (defparameter ,name new-istrum)
       new-istrum)))

(setf (documentation 'make-istrum 'function)
      "Creates STRUMMER part using an alternate event-list format.
The name, instrument, :section, :cuefn, :shuffle, :shift, 
:tempo, :unit, :bars, :beats, :subbeats, :render-once, 
:transposable, :reversible, :chord-model and :remarks arguments 
have identical usage as STRUMMER.   The remaining keyword arguments 
are

:form - If set to :events, returns the strummer event-list. Otherwise
        returns a new instance of STRUMMER.  This is primarily used
        for testing.

:events - The istrum event list (the i is for implicit) is more concise
          then the equivalent STRUMMER events.   Most of the STRUMMER 
          event keywords have been eliminated and event parameters are
          determined either by position or by context.

          The first two elements of an istrum event list are the time 
          and keynumber respectively 

             (time keynumber ...)

         There is no need for either :TIME or :KEY as with STRUMMER.

         1) TIME is always the first element and must have a format 
            recognized by the cue-function.  The default is a 'BAR' 
            format list (bar beat subbeat).  A few special case 
            symbols are allowed.

            ((1 2 3) ... )      Default bar format, bar 1, beat 2, subbeats 3
            
            ((1 1 1) ... )      bar 1, beat 1
            ((1 2 1) ... )      bar 1, beat 2
            (+       ... )      bar 1, beat 3 
                                + increments time by difference of the 
                                previous two values.  
            (=       ...)       bar 1, beat 3
                                = repeats the previous time.

         2) KEYNUMBER is always the 2nd event element.
            
            (time keynumber ...) 

           Ultimately the instrument's keynumber-map is responsible for
           processing all keynumbers.  There are a few special cases

           A) The = symbol repeats the previous key
           B) Numbers of the form n- and n+ transpose the previous key
              by -/+ n steps.  Transpose may only be used if the predicate
              KEYNUMBER-P would return true for the previous key number.
              
           Otherwise the keynumber is free to be any symbol the instrument's
           keynumber-map recognizes.

         3) CHORDS are recognized by either a valid chord name or as a 
            list of keynumber offsets.

            (time key ... [min] ...)    A minor chord
            (time key ... (0 3 7) ...)  Also a minor chord, *but see 
                                        exception below.
            (time key ... [solo] ...)   Turns chords off.

            * A chord list is treated differently depending on the 
              chord-model.  The default chord-model treats a list 
              as a list of keynumber offsets,  (0 3 7) is a minor 
              chord built on whatever the keynumber is.

              The guitar chord model treats a list as absolute 
              key numbers, (0 3 7) is still a minor key but it 
              will be in octave 0.  A c-minor key in octave 5 would 
              be (60 63 67).

         4) CHORD INVERSION have the form :INV-N or :INV+N where n
            is a small integer. 

            (time key ... :INV+1 ...)  Use first inversion

            The symbol :INV-OFF is equivalent to :INV+0.

         5) CHORD OCTAVE, the symbols :OCT+ :OCT- and :OCT-OFF
            add an octave (up +/- down) copy of the first
            chord note.  :OCT-OFF turns octave note off.
                                 
            (time 60 (0 3 7) :OCT+ ...)  actual produced notes: 60 63 67 72.
 
         6) STRUM TIME, a non-negative float sets the strum delay.
            
            (time key ... 0.01 ...)   strum delay 0.01 seconds.

         7) STRUM END-TOGETHER. The symbols :ETOG and :ETOG-OFF determine
            if all notes in a strummed chord end at the same time or 
            if they are staggered.

            (time key [min] 0.01 :etog)     strum chord with 0.01 second
                                            delay, all notes end at same time.

            (time key [min] 0.01 :etog-off) note offs are staggered.

         8) STRUM DIRECTION.  The symbols :UP, :DOWN, :RAND and :RAND-UP
            set the strummed note order.

            :DOWN    - play chord notes in order.
            :UP      - reverse order
            :RAND    - random order
            :RAND-UP - select up or down at random.
            :UP-DOWN - cycle between up and down
            :DOWN-UP - cycle between down and up

         9) DYNAMICS. Note dynamics are set by and any of the dynamic symbols

             pppp- pppp pppp+ ... mp- mp mp+ mf- mf mf+ ... ffff- ffff ffff+

             (time key ... pp ...)


        10) ARTICULATION.  Note duration is set by any valid metric-expression

            (time key ... q ...)      quarter note
            (time key ... h. ...)     dotted half note
            (time key ... q+s ...)    quarter note tied to 16th note.

        11) PROGRAM CHANGES.  Any symbol of the form :PROGRAM-XXX 
            produces a program change. XXX may be an integer 0 ... 127 
            or name recognized by the instrument's program map.

            (time key ... program-64 ...)
            (time key ... program-grung ...)  Presumably the program-map knows
                                              what grung means. 


Comparison between equivalent ISTRUM and STRUMMER event list.


STRUMMER

 '((:time (1 1 1) :key 60 :chord [min] :inversion -2 :octave 1 
                          :strum  0.01 :direction up :dur q :amp ff)
   (:time (1 2 1) :key 61 :chord [solo] :inversion 0  :octave 0)
   (:time (1 3 1) :key 60)
   (:time (1 4 1) :key 60 :amp mp :program 12))

ISTRUM:

 '(((1 1 1) 60 [min]  :inv-2   :oct+ 0.01 :up q ff)
   ((1 2 1) 61 [solo] :inv-off :oct-off)
   (+       60 )
   (+        = mp :prog-12))

 Like strummer, istrum parameters stay in effect until explicitly changed.")


(setf (documentation 'istrum 'function)
      "The ISTRUM macro is almost identical to the MAKE-ISTRUM function, except 
istrum binds the new object to the symbol name while make-istrum does not.

Quote the name argument for make-istrum and leave it unquoted for istrum.")
