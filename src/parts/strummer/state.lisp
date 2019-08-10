;;;; cyco-strummer state.lisp
;;;;


(defstruct state
  (source "")	                     ; CYCO event source text
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


(defmethod soft-reset ((state state))
  (setf (state-key state) nil)
  (setf (state-controller-value state) nil)
  (setf (state-bend state) nil)
  (setf (state-program-number state) nil)
  (setf (state-program-bank state) nil)
  (setf (state-grace-key state) nil)
  state)

(defmethod reset ((state state))
  (soft-reset state)
  (setf (state-time-specification state) nil)
  (setf (state-time state) nil)
  (setf (state-chord-type state) '(0))
  (setf (state-chord-inversion state) 0)
  (setf (state-chord-octave state) 0)
  (setf (state-strum-delay state) 0.0)
  (setf (state-strum-acceleration state) 1.0)
  (setf (state-strum-direction state) (line :of 'down))
  (setf (state-strum-amp-scale state) 1.0)
  (setf (state-strum-end-together state) t)
  (setf (state-grace-delay state) 0.0)
  (setf (state-grace-amp-scale state) 1.0)
  (setf (state-grace-articulation state) 1.0)
  (setf (state-articulation state) 1.0)
  (setf (state-controller-number state) nil)
  (setf (state-dynamic state) (line :of '(0.5)))
  (setf (state-dynamic-min state) 0.0)
  (setf (state-dynamic-max state) 1.0)
  state)

(defun real-event-p (state)
  (or (state-key state)
      (state-controller-value state)
      (state-bend state)
      (state-program-number state)
      (state-program-bank state)
      (state-grace-key state)))
      

(defmethod clone ((source state) &key new-name new-parent)
  (dismiss new-name new-parent)
  (make-state
   :source (state-source source)
   :time-specification (state-time-specification source)
   :time (state-time source)
   :key (state-key source)
   :chord-type (state-chord-type source)
   :chord-inversion (state-chord-inversion source)
   :chord-octave (state-chord-octave source)
   :strum-delay (state-strum-delay source)
   :strum-acceleration (state-strum-acceleration source)
   :strum-direction (state-strum-direction source)
   :strum-amp-scale (state-strum-amp-scale source)
   :strum-end-together (state-strum-end-together source)
   :grace-key (state-grace-key source)
   :grace-delay (state-grace-delay source)
   :grace-amp-scale (state-grace-amp-scale source)
   :grace-articulation (state-grace-articulation source)
   :articulation (state-articulation source)
   :dynamic (state-dynamic source)
   :dynamic-blur (state-dynamic-blur source)
   :dynamic-min (state-dynamic-min source)
   :dynamic-max (state-dynamic-max source)
   :controller-number (state-controller-number source)
   :controller-value (state-controller-value source)
   :bend (state-bend source)
   :program-number (state-program-number source)
   :program-bank (state-program-bank source)))

(defmethod transpose ((state state)(x number))
  (let ((kn (state-key state))
	(gk (state-grace-key state)))
    (setf (state-key state)(transpose kn x))
    (setf (state-grace-key state)(transpose gk x))
    state))

(defmethod invert ((state state)(pivot t))
  (if pivot
      (let ((kn (state-key state))
	    (gk (state-grace-key state)))
	(setf (state-key state)
	      (invert kn pivot))
	(setf (state-grace-key state)
	      (invert gk pivot)))
    state))



