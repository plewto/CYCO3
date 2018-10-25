;;;; CYCO sj config emu/procussion/toolkit
;;;;
;;;; Zone    Stack             key-range 
;;;; Z01     S540 twank        036 038
;;;; Z02     S541 megaPlanks   039 041
;;;; Z03     S514 waterpin     042 044
;;;; Z04     S542 templodoom   045 047
;;;; Z05     S543 glong        048 050
;;;; Z06     S544 giantrtachet 051 053
;;;; Z07     S367 wood block   054 056 *
;;;; Z08     S330 clank        054 056 *
;;;; Z09     S545 echo thud    057 059
;;;; Z10     S546 awk bosk     060 062
;;;; Z11     S547 gymnasium    063 065
;;;; Z12     S544 giantratchet 066 068
;;;; Z13     S541 mega planks  069 071
;;;; Z14     S547 gymnasium    072 074
;;;; Z15     S546 awk bosk     075 077
;;;; Z16     S057 spaceKick    078 080
;;;; Z17     S124 pariSnare    081 083
;;;; Z18     S287 16Choke1     084 086
;;;; Z19     S318 HyperReal    087 089
;;;; Z20     S115 AmbiSnare1   090 092
;;;; Z21     S118 ModVerbSnr   093 095
;;;; Z22     S171 RevSnare2    096 098
;;;; Z23     S125 Snare 1157   099 101
;;;; Z24     S209 TunedTomz    102 104
;;;;
;;;; toolkit
;;;;    |
;;;;    +-- tk-twank
;;;;    +-- tk-waterpin
;;;;    +-- tk-doom
;;;;    +-- tk-echo-thud
;;;;    +-- tk-clank
;;;;    +-- tk-kick
;;;;    +-- tk-tom
;;;;    +-- tk-planks
;;;;    +-- tk-ratchet
;;;;    +-- tk-awk-bosk
;;;;    +-- tk-gymnasium
;;;;    +-- tk-cym
;;;;    +-- tk-snare

(flet ((make-keymap (ranges)
		    (let ((acc '()))
		      (dolist (q (->list ranges))
			(dotimes (i 3)
			  (push (+ q i) acc)))
		      (setf acc (reverse acc))
		      #'(lambda (kn)
			  (cond ((eq kn :doc)
				 (format t "Circular keynumber map ~A" acc)
				 +rest+)
				((and (numberp kn)(minusp kn))
				 +rest+)
				((numberp kn)
				 (cnth (truncate kn) acc))
				(t +rest+)))))) ;; ignore unrecognized keynumbers
				 
  
  (defun toolkit (&key (parent pro3)(channel nil) articulation-map dynamic-map)
    (let ((inst (instrument toolkit
			    :parent parent
			    :channel channel
			    :program (procussion-program 'toolkit)
			    :articulation-map articulation-map
			    :dynamic-map dynamic-map
			    :transient t)))
      (instrument tk-twank 
		  :parent inst
		  :remarks "Procussion toolkit XXX instrument"
		  :transient t
		  :keynumber-map (make-keymap 36))

      (instrument tk-waterpin 
		  :parent inst
		  :remarks "Procussion toolkit XXX instrument"
		  :transient t
		  :keynumber-map (make-keymap 42))

      (instrument tk-doom 
		  :parent inst
		  :remarks "Procussion toolkit XXX instrument"
		  :transient t
		  :keynumber-map (make-keymap 45))

      (instrument tk-clank 
		  :parent inst
		  :remarks "Procussion toolkit XXX instrument"
		  :transient t
		  :keynumber-map (make-keymap 54))

      (instrument tk-echo-thud 
		  :parent inst
		  :remarks "Procussion toolkit XXX instrument"
		  :transient t
		  :keynumber-map (make-keymap 57))

      (instrument tk-kick 
		  :parent inst
		  :remarks "Procussion toolkit XXX instrument"
		  :transient t
		  :keynumber-map (make-keymap 78))

      (instrument tk-tom
		  :parent inst
		  :remarks "Procussion toolkit XXX instrument"
		  :transient t
		  :keynumber-map (make-keymap 102))

      (instrument tk-planks
		  :parent inst
		  :remarks "Procussion toolkit XXX instrument"
		  :transient t
		  :keynumber-map (make-keymap '(39 69)))

      (instrument tk-ratchet 
		  :parent inst
		  :remarks "Procussion toolkit XXX instrument"
		  :transient t
		  :keynumber-map (make-keymap '(51 66)))

      (instrument tk-awk-bosk 
		  :parent inst
		  :remarks "Procussion toolkit XXX instrument"
		  :transient t
		  :keynumber-map (make-keymap '(60 75)))

      (instrument tk-gymnasium 
		  :parent inst
		  :remarks "Procussion toolkit XXX instrument"
		  :transient t
		  :keynumber-map (make-keymap '(63 72)))

      (instrument tk-cym 
		  :parent inst
		  :remarks "Procussion toolkit XXX instrument"
		  :transient t
		  :keynumber-map (make-keymap '(84 87)))

      (instrument tk-snare 
		  :parent inst
		  :remarks "Procussion toolkit XXX instrument"
		  :transient t
		  :keynumber-map (make-keymap '(90 81 93 96 99)))
      inst))) 

      

