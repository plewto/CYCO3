;;;; CYCO3 src/composition/parts/epart-state
;;;; EPART helper object.

(constant +strum-directions+ '(:up :down :dice :random))

(defstruct epart-state
  (source "")	                     ; CYCO event source text
  (time nil)		             ; nil | float >= 0
  (time-increment 0)	             ; **DEPRECIATED**
  (key nil)		             ; nil | -1 | keynumber
  (chord-template '(0))	             ; list keynumber offsets
  (chord-inversion 0)	             ; int -+, template rotation
  (chord-octave 0)	             ; int +, if non-zero append octave to chord.
  (strum-delay 0.0)	             ; float >= 0.0
  (strum-acceleration 1.0)           ; float > 0
  (strum-direction (line :of :down)) ; pattern {:down :up :dice :random}
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
  (touch nil)
  (controller-number nil)
  (controller-value nil)
  (bend nil)
  (program-number nil)	             ; nil | keyword | int
  (program-bank nil))                ; nil | keyword | int

(defmethod soft-reset ((state epart-state))
  (setf (epart-state-key state) nil)
  (setf (epart-state-touch state) nil)
  (setf (epart-state-controller-value state) nil)
  (setf (epart-state-bend state) nil)
  (setf (epart-state-program-number state) nil)
  (setf (epart-state-program-bank state) nil)
  (setf (epart-state-grace-key state) nil)
  state)

(defmethod reset ((state epart-state))
  (soft-reset state)
  (setf (epart-state-time state) nil)
  (setf (epart-state-time-increment state) 0)
  (setf (epart-state-chord-template state) '(0))
  (setf (epart-state-chord-inversion state) 0)
  (setf (epart-state-chord-octave state) 0)
  (setf (epart-state-strum-delay state) 0.0)
  (setf (epart-state-strum-acceleration state) 1.0)
  (setf (epart-state-strum-direction state) (line :of :down))
  (setf (epart-state-strum-amp-scale state) 1.0)
  (setf (epart-state-strum-end-together state) t)
  (setf (epart-state-grace-delay state) 0.0)
  (setf (epart-state-grace-amp-scale state) 1.0)
  (setf (epart-state-grace-articulation state) 1.0)
  (setf (epart-state-articulation state) nil)
  (setf (epart-state-controller-number state) nil)
  (setf (epart-state-dynamic state) (line :of '(0.5)))
  (setf (epart-state-dynamic-min state) 0.0)
  (setf (epart-state-dynamic-max state) 1.0)
  state)

(defmethod clone ((state epart-state) &key new-name new-parent)
  (dismiss new-name new-parent)
  (make-epart-state
   :source (epart-state-source state)
   :time (epart-state-time state)
   :time-increment (epart-state-time-increment state)
   :key (epart-state-key state)
   :chord-template (epart-state-chord-template state)
   :chord-inversion (epart-state-chord-inversion state)
   :chord-octave (epart-state-chord-octave state)
   :strum-delay (epart-state-strum-delay state)
   :strum-acceleration (epart-state-strum-acceleration state)
   :strum-direction (epart-state-strum-direction state)
   :strum-amp-scale (epart-state-strum-amp-scale state)
   :strum-end-together (epart-state-strum-end-together state)
   :grace-key (epart-state-grace-key state)
   :grace-delay (epart-state-grace-delay state)
   :grace-amp-scale (epart-state-grace-amp-scale state)
   :grace-articulation (epart-state-grace-articulation state)
   :articulation (epart-state-articulation state)
   :controller-number (epart-state-controller-number state)
   :program-number (epart-state-program-number state)
   :program-bank (epart-state-program-bank state)
   :dynamic (epart-state-dynamic state)
   :touch (epart-state-touch state)
   :controller-value (epart-state-controller-value state)
   :bend (epart-state-bend state)))
   
(defmethod transpose ((state epart-state)(x number))
  (let ((kn (epart-state-key state))
	(gk (epart-state-grace-key state)))
    (setf (epart-state-key state)(transpose kn x))
    (setf (epart-state-grace-key state)(transpose gk x))
    state))

(defmethod invert ((state epart-state)(pivot t))
  (let ((kn (epart-state-key state))
	(gk (epart-state-grace-key state)))
    (setf (epart-state-key state)
	  (invert kn pivot))
    (setf (epart-state-grace-key state)
	  (invert gk pivot))
    state))

