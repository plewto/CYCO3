;;;; CYCO Emu Procussion plugin: beach-party
;;;;
;;;; Zone Stack             key range : 
;;;;  1 478 SurfinUSA      : 061 127  : 
;;;;  2 420 CongaSet       : 037 059  :
;;;;  3 457 ShakerDrm2     : 036      :
;;;;  4 455 Kalimdrum      : 048      :
;;;;  5 443 ShakerDrum     : 060      :
;;;;  6 473 WeirdoSlap     : 042      :
;;;;  7 452 Matalimba      : 054      :
;;;;  8 473 WeirdoSlap     : 037      :
;;;;  9 473 WeirdoSlap     : 039      :
;;;; 10 473 WeirdoSlap     : 044      :
;;;; 11 473 WeridoSlap     : 046      :
;;;; 12 452 Metalimba      : 049      :
;;;; 13 452 Metalimba      : 051      :
;;;; 14 452 Metalimba      : 056      :
;;;; 15 452 Metalimba      : 058      : 
;;;; 16                    :          :
;;;; 17                    :          :
;;;; 18                    :          :
;;;; 19                    :          :
;;;; 20                    :          :
;;;; 21                    :          : 
;;;; 22                    :          :
;;;; 23                    :          : 
;;;; 24                    :          :
;;;;

(defun beach-party (&key (parent procussion) channel)
  (let ((inst (make-instrument 'beach-party
			       :parent parent
			       :channel channel
			       :program (procussion-program 'beach-party)
			       :keynumber-map (procussion-keymap 36 127))))
    (defparameter bp-surfin (make-instrument 'bp-surfin
					     :parent inst
					     :keynumber-map (circular-keynumber-map 61 127)))
    (defparameter bp-drums (make-instrument 'bp-drums
					    :parent inst
					    :keynumber-map (circular-keynumber-map 36 59)))
    (defparameter beach-party inst)
    inst))
					    
