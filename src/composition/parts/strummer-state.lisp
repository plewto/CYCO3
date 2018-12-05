;;;; CYCO
;;;; Strummer hellper object.

(constant +strum-directions+ '(:up :down :dice :random))

(defstruct strummer-state
  (source "")	                     ; CYCO event source text
  (time nil)		             ; nil | float >= 0
  ;;(time-increment 0)	             ; **DEPRECIATED**
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
  (setf (strummer-state-articulation state) nil)
  (setf (strummer-state-controller-number state) nil)
  (setf (strummer-state-dynamic state) (line :of '(0.5)))
  (setf (strummer-state-dynamic-min state) 0.0)
  (setf (strummer-state-dynamic-max state) 1.0)
  state)

(defmethod clone ((state strummer-state) &key new-name new-parent)
  (dismiss new-name new-parent)
  (make-strummer-state
   :source (strummer-state-source state)
   :time (strummer-state-time state)
   :key (strummer-state-key state)
   :chord-type (strummer-state-chord-type state)
   :chord-inversion (strummer-state-chord-inversion state)
   :chord-octave (strummer-state-chord-octave state)
   :strum-delay (strummer-state-strum-delay state)
   :strum-acceleration (strummer-state-strum-acceleration state)
   :strum-direction (strummer-state-strum-direction state)
   :strum-amp-scale (strummer-state-strum-amp-scale state)
   :strum-end-together (strummer-state-strum-end-together state)
   :grace-key (strummer-state-grace-key state)
   :grace-delay (strummer-state-grace-delay state)
   :grace-amp-scale (strummer-state-grace-amp-scale state)
   :grace-articulation (strummer-state-grace-articulation state)
   :articulation (strummer-state-articulation state)
   :controller-number (strummer-state-controller-number state)
   :program-number (strummer-state-program-number state)
   :program-bank (strummer-state-program-bank state)
   :dynamic (strummer-state-dynamic state)
   :controller-value (strummer-state-controller-value state)
   :bend (strummer-state-bend state)))
   
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



