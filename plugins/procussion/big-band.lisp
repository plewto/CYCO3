;;;; CYCO Emu Procussion plugin: big-band
;;;;
;;;; Zone Stack             key range : 
;;;;  1 015 Room Kick 1    : 036      : 
;;;;  2 015 Room Kick 1    : 053 059  :
;;;;  3 174 BbandSnare     : 038      :
;;;;  4 175 BbandRim       : 037      :
;;;;  5 176 BbandClick     : 039      :
;;;;  6 174 BbandSnare     : 060 065  :
;;;;  7 279 BbandHatCl     : 042      :
;;;;  8 280 BbandHatOp     : 044      :
;;;;  9 281 BbandHatSt     : 046      :
;;;; 10 175 BbandRim       : 066 071  :
;;;; 11 218 BbandTom       : 041      :
;;;; 12 218 BbandTom       : 043      :
;;;; 13 218 BbandTom       : 045      :
;;;; 14 218 BbandTom       : 047      :
;;;; 15 291 Ride Bell      : 049      : 
;;;; 16 290 Ride Ping      : 051      :
;;;; 17 286 16 Crash 1     : 048      :
;;;; 18 287 16 Choke 1     : 050      :
;;;; 19 372 BbandCwbel     : 040      :
;;;; 20 288 16 Crash 2     : 052      :
;;;; 21 176 BbandClick     : 072 077  : 
;;;; 22 218 BbandTom       : 078 096  :
;;;; 23 042 Drk-Space      : 053 059  : Reverb ?
;;;; 24 042 Drk-Space      : 033 036  : Reverb ?
;;;;

(defun big-band (&key (parent procussion) channel)
  (let ((bb (make-instrument 'big-band
			     :parent parent
			     :channel channel
			     :keynumber-map (procussion-keymap 36 96))))
  (defparameter bbnd-kick (make-instrument 'bbnd-kick
					   :parent bb
					   :keynumber-map (circular-list-keynumber-map (cons 36 (range 53 59)))))
  (defparameter bbnd-snare (make-instrument 'bbnd-snare
					    :parent bb
					    :keynumber-map (procussion-subkey-map '((x        . (38))
										     (rim      . (37))
										     (click    . (39))
										     (x-1      . (60))
										     (rim-1    . (66))
										     (click-1  . (72))
										     (x-2      . (61))
										     (rim-2    . (67))
										     (click-2  . (73))
										     (x-3      . (62))
										     (rim-3    . (68))
										     (click-3  . (74))
										     (x-4      . (63))
										     (rim-4    . (67))
										     (click-4  . (75))
										     (x-5      . (64))
										     (rim-5    . (68))
										     (click-5  . (76))
										     (x-6      . (66))
										     (rim-6    . (69))
										     (click-6  . (77))))))
  (defparameter bbnd-tom (make-instrument 'bbnd-tom
					  :parent bb
					  :keynumber-map (circular-list-keynumber-map (append '(41 43 45 47)(range 78 96)))))
  (defparameter bbnd-hat (make-instrument 'bbnd-hat
					  :parent bb
					  :keynumber-map (procussion-subkey-map '((x    . (42))
										   (open . (44))
										   (ped  . (46))))))
  (defparameter bbnd-cow (make-instrument 'bbnd-cow
					  :parent bb
					  :keynumber-map #'(lambda (k)
							     (cond ((rest-p k) +rest+)
								   (t 40)))))

  (defparameter bbnd-cym (make-instrument 'bbnd-cym
					  :parent bb
					  :keynumber-map (procussion-subkey-map '((ride . (51))
										   (bell . (49))
										   (crash . (48))
										   (choke . (50))
										   (crash-2 . (52))))))
  (defparameter big-band bb)
  bb))
  
  
