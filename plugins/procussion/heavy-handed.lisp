;;;; CYCO Emu Procussion plugin: heavy-handed
;;;;
;;;; Zone Stack             key range : 
;;;;  1 019 The Kicker     : 036      : 
;;;;  2                    :          :
;;;;  3 098 SmackSnare     : 038      :
;;;;  4 102 Dance Snare    : 040      :
;;;;  5 167 BackwardSnr    : 037      :
;;;;  6                    :          :
;;;;  7 270 Beasty Hat     : 042      :
;;;;  8 271 Beasty Open    : 044      :
;;;;  9 236 HiHatAstmp     : 039      :
;;;; 10                    :          :
;;;; 11 194 Power Toms     : 041      :
;;;; 12 194 Power Toms     : 043      :
;;;; 13 194 Power Toms     : 045      :
;;;; 14 194 Power Toms     : 047      :
;;;; 15 512 Bent China     : 046      : 
;;;; 16 288 16" Crash 2    : 049      :
;;;; 17 289 19" Pang       : 051      :
;;;; 18 289 19" Pang       : 054      :
;;;; 19                    :          :
;;;; 20 194 Power Toms     : 048      :
;;;; 21 195 PowerTom 1     : 053      : 
;;;; 22 202 Electro Tom    : 052      :
;;;; 23 202 Electro Tom    : 050      : 
;;;; 24 200 Epic Tom       : 055 096  :
;;;;

(defun heavy-handed (&key (parent procussion) channel)
  (let ((inst (make-instrument 'heavy-handed
			       :parent parent
			       :channel channel
			       :program (procussion-program 'heavy-handed)
			       :keynumber-map (procussion-keymap 36 96))))
    (defparameter hvy-kick (make-instrument 'hyv-kick
					    :parent inst
					    :keynumber-map
					    #'(lambda (kn)
						(cond ((eq kn :doc)
						       (format t "x --> 36~%")
						       +rest+)
						      ((rest-p kn) +rest+)
						      ((eq kn 'x) 36)
						      (t nil)))))
    (defparameter hvy-snare (make-instrument 'hvy-snare
					     :parent inst
					     :keynumber-map (procussion-subkey-map
							     '((x . (38 "SmackSnare"))
							       (rev . (37 "BackwardSnr"))
							       (dance . (40))))))
    (defparameter hvy-hat (make-instrument 'hvy-hat
					   :parent inst
					   :keynumber-map (procussion-subkey-map
							   '((x . (42 "Beasty Hat"))
							     (op . (40 "Beasty Open"))
							     (opn . (40 "Beasty Open"))
							     (open . (40 "Beasty Open"))
							     (ped . (39 "HiHatA Stomp"))))))

    (defparameter hvy-cym (make-instrument 'hvy-cym
					   :parent inst
					   :keynumber-map (procussion-subkey-map
							   '((crash . (49))
							     (china . (46 "Bent China"))
							     (pang  . (51))
							     (pang-2 . (54))))))
    (defparameter hvy-toms (make-instrument 'hvy-toms
					    :parent inst
					    :keynumber-map (procussion-subkey-map
							    '((A . (41))
							      (B . (43))
							      (C . (45))
							      (D . (47))
							      (E . (48))
							      (F . (53))
							      (electro . (50))
							      (electro-2 . (52))))))
    (defparameter hvy-power-toms (make-instrument 'hvy-power-toms
						  :parent inst
						  :keynumber-map (circular-keynumber-map 55 96)))
    (defparameter heavy-handed inst)
    inst))
						 
