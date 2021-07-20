;;;; CYCO parts strummer-state.lisp
;;;; 
;;;; Defines internal state of STRUMMER objects.
;;;;

(in-package :cyco-part)

(defstruct strummer-state
  (source nil)	                     ; event source text
  (time-specification nil)            
  (time nil)		             ; nil | float >= 0
  (key nil)		             ; nil | -1 | keynumber
  (chord-type '(0))	             ; Either list of keynumber offsets or chord-name
  (chord-inversion 0)	             ; int -+, template rotation
  (chord-octave 0)	             ; int +, if non-zero append octave to chord.
  (strum-delay 0.0)	             ; float >= 0.0
  (strum-acceleration 1.0)           ; float > 0
  (strum-direction (line :of 'down)) ; pattern {down up dice random}
  (strum-amp-scale 1.0)	             ; float > 0
  (strum-end-together t)             ; bool
  (grace-key nil)		     ; Grace notes
  (grace-delay 0.0)
  (grace-amp-scale 1.0)
  (grace-articulation 1.0)
  (articulation 0.0)	             ; float (note duration)
  (dynamic (cycle :of '(0.5)))
  (dynamic-blur 0.0)
  (dynamic-min 0.0)
  (dynamic-max 1.0)
  (controller-number nil)
  (controller-value nil)
  (bend nil)
  (program-number nil)	             ; nil | keyword | int
  (program-bank nil))                ; nil | keyword | int


(defmethod pattern-reset ((state strummer-state))
  (reset (strummer-state-strum-direction state))
  (reset (strummer-state-dynamic state)))

(defmethod soft-reset ((state strummer-state))
  (setf (strummer-state-key state) nil)
  (setf (strummer-state-controller-value state) nil)
  (setf (strummer-state-bend state) nil)
  (setf (strummer-state-program-number state) nil)
  (setf (strummer-state-program-bank state) nil)
  (setf (strummer-state-grace-key state) nil)
  state)

(defmethod reset ((state strummer-state))
  (soft-reset state)
  (setf (strummer-state-time-specification state) nil)
  (setf (strummer-state-time state) nil)
  (setf (strummer-state-chord-type state) '(0))
  (setf (strummer-state-chord-inversion state) 0)
  (setf (strummer-state-chord-octave state) 0)
  (setf (strummer-state-strum-delay state) 0.0)
  (setf (strummer-state-strum-acceleration state) 1.0)
  (setf (strummer-state-strum-direction state) (line :of 'down))
  (setf (strummer-state-strum-amp-scale state) 1.0)
  (setf (strummer-state-strum-end-together state) t)
  (setf (strummer-state-grace-delay state) 0.0)
  (setf (strummer-state-grace-amp-scale state) 1.0)
  (setf (strummer-state-grace-articulation state) 1.0)
  (setf (strummer-state-articulation state) 1.0)
  (setf (strummer-state-controller-number state) nil)
  (setf (strummer-state-dynamic state) (line :of '(0.5)))
  (setf (strummer-state-dynamic-min state) 0.0)
  (setf (strummer-state-dynamic-max state) 1.0)
  state)

      
(defmethod clone ((mother strummer-state) &key new-name new-parent)
  (declare (ignore new-name new-parent))
  (make-strummer-state
   :source (strummer-state-source mother)
   :time-specification (strummer-state-time-specification mother)
   :time (strummer-state-time mother)
   :key (strummer-state-key mother)
   :chord-type (strummer-state-chord-type mother)
   :chord-inversion (strummer-state-chord-inversion mother)
   :chord-octave (strummer-state-chord-octave mother)
   :strum-delay (strummer-state-strum-delay mother)
   :strum-acceleration (strummer-state-strum-acceleration mother)
   :strum-direction (strummer-state-strum-direction mother)
   :strum-amp-scale (strummer-state-strum-amp-scale mother)
   :strum-end-together (strummer-state-strum-end-together mother)
   :grace-key (strummer-state-grace-key mother)
   :grace-delay (strummer-state-grace-delay mother)
   :grace-amp-scale (strummer-state-grace-amp-scale mother)
   :grace-articulation (strummer-state-grace-articulation mother)
   :articulation (strummer-state-articulation mother)
   :dynamic (strummer-state-dynamic mother)
   :dynamic-blur (strummer-state-dynamic-blur mother)
   :dynamic-min (strummer-state-dynamic-min mother)
   :dynamic-max (strummer-state-dynamic-max mother)
   :controller-number (strummer-state-controller-number mother)
   :controller-value (strummer-state-controller-value mother)
   :bend (strummer-state-bend mother)
   :program-number (strummer-state-program-number mother)
   :program-bank (strummer-state-program-bank mother)))

(defmethod transpose ((state strummer-state)(x number))
  (let ((kn (strummer-state-key state))
	(gk (strummer-state-grace-key state)))
    (setf (strummer-state-key state)(transpose kn x))
    (setf (strummer-state-grace-key state)(transpose gk x))
    state))

(defmethod invert ((state strummer-state)(pivot t))
  (if pivot
      (let ((kn (strummer-state-key state))
	    (gk (strummer-state-grace-key state)))
	(setf (strummer-state-key state)
	      (invert kn pivot))
	(setf (strummer-state-grace-key state)
	      (invert gk pivot)))
    state))

(defmethod transpose ((state strummer-state)(amount t))
  (setf (strummer-state-key state)
	(transpose (strummer-state-key state) amount)
	(strummer-state-grace-key state)
	(transpose (strummer-state-grace-key state) amount))
  state)

(defmethod invert ((state strummer-state)(pivot t))
  (setf (strummer-state-key state)
	(invert (strummer-state-key state) pivot)
	(strummer-state-grace-key state)
	(invert (strummer-state-grace-key state) pivot))
  state)
  
  
