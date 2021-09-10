;;;; CYCO Emu Procussion plugin: disco-ball
;;;;
;;;; Zone Stack             key rang
;;;;  1 022 BigFoot      :  36              *
;;;;  2 049 CymbalKick   :  37              *
;;;;  3 104 Slap Snare   :  38              *
;;;;  4 154 TrashSnr 1   :  41              *
;;;;  5 157 TrashSnr 4   :  43              *
;;;;  6 143 HouseSnare   :  45              *
;;;;  7 265 HouseHat 3   :  42              *
;;;;  8 336 Syn Scrtch   :  44              *
;;;;  9 272 Horny Hat    :  46              *
;;;; 10 368 Tambourine   :  49              *
;;;; 11 157 TrashSnr 4   :  98              *
;;;; 12 333 RapScratch   : 100              *
;;;; 13                  :                  *
;;;; 14                  :                  *
;;;; 15                  :                  *
;;;; 16                  :                  *
;;;; 17                  :                  *
;;;; 18 022 BigFoot      :  50  60          *
;;;; 19 343 HandClaps    :  39              *
;;;; 20 429 Bass Stack   :  72  96          *
;;;; 21 147 HouseSnr 5   :  40              *
;;;; 22 066 Trash Kick   :  47              *
;;;; 23 164 ClaveSnare   :  48              *
;;;; 24 104 SlapSnare    :  61 71           *
;;;;
;;;; DISCO-BALL
;;;;    DB-KICK
;;;;    DB-SNARE
;;;;    DB-HAT
;;;;    DB-BIGFOOT
;;;;    DB-SLAP-SNARE
;;;;    DB-BASS (melodic)

(let ((kick-list '((A  . (36 "BigFoot"))
		   (B  . (47 "Trash Kick"))
		   (C  . (37 "Cymbal Kick"))))
      (snare-list '((slap     . (38 "Slap Snare"))
		    (house    . (40 "HouseSnr 5"))
		    (trash    . (41 "TrashSnr 1"))
		    (trash-2  . (43 "TrashSnr 4"))
		    (house-2  . (45 "HouseSnare"))
		    (clave    . (48 "ClaveSnare"))
		    (trash-3  . (98 "TrashSnr 4"))
		    (clap     . (39 "HandClaps"))
		    (x        . (41))))
      (hat-list '((house      . (042 "HouseHat 3"))
		  (scrtch     . (044 "Syn Scrtch"))
		  (horny      . (046 "Horny Hat"))
		  (tambourine . (049 "Tambourine"))
		  (scratch    . (100 "RapScratch"))
		  (x          . (042 "HouseHat 3")))))
  (defun disco-ball (&key (parent procussion) channel)
    (let ((dball (make-instrument 'disco-ball
				  :parent parent
				  :channel channel
				  :keynumber-map (procussion-keymap 36 100)
				  :program (procussion-program 'disco-ball))))
      (defparameter db-kick (make-instrument 'db-kick
					     :parent dball
					     :keynumber-map (procussion-subkey-map kick-list)))
      (defparameter db-snare (make-instrument 'db-snare
					      :parent dball
					      :keynumber-map (procussion-subkey-map snare-list)))
      (defparameter db-hat (make-instrument 'db-hat
					    :parent dball
					    :keynumber-map (procussion-subkey-map hat-list)))
      (defparameter db-bigfoot (make-instrument 'db-bigfoot
						:parent dball
						:keynumber-map (circular-keynumber-map 50 60)))
      (defparameter db-slap-snare (make-instrument 'db-slap-snare
						   :parent dball
						   :keynumber-map (circular-keynumber-map 61 71)))
      ;; 72 -- 96
      (defparameter db-bass (make-instrument 'db-bass
					     :parent dball
					     :remarks "Melodic"
					     :keynumber-map
					     #'(lambda (k)
						 (cond ((eq k :doc)
							(format t "2-octave wrapped  kn --> [72..95]")
							+rest+)
						       ((rest-p k)
							+rest+)
						       ((keynumber-p k)
							(let* ((kn (keynumber k))
							       (pc (pitch-class kn))
							       (oc (octave kn)))
							  (+ pc (if (evenp oc) 72 86))))
						       (t +rest+)))))
      (param disco-ball dball)
      dball)))
							     
							
							      
		    
