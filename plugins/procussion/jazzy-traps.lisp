;;;; CYCO Emu Procussion plugin: jazzy-traps
;;;;
;;;; Zone Stack             key range : 
;;;;  1 023 Bonzo Kick     : 033 036  : 
;;;;  2 023 Bonzo Kick     : 053 058  :
;;;;  3 100 Tonal Snare    : 038      :
;;;;  4 081 BrushSnr B     : 039      :
;;;;  5 080 BrushSnr A     : 040      :
;;;;  6 100 Tonal Snare    : 059 064  :
;;;;  7 232 HiHatAshut     : 042      :
;;;;  8 235 HiHatAopen     : 044      :
;;;;  9 242 ModHiHat 1     : 052      :
;;;; 10 294 MalletRoll     : 046      :
;;;; 11 179 Dry 12 Tom     : 041      :
;;;; 12 179 Dry 12 Tom     : 043      :
;;;; 13 179 Dry 12 Tom     : 045      :
;;;; 14 179 Dry 12 Tom     : 047      :
;;;; 15 301 Vlcty Ride     : 050      : 
;;;; 16 289 19 Pang        : 051      :
;;;; 17 287 16 Choke 1     : 048      :
;;;; 18 292 MallCymbal     : 049      :
;;;; 19 367 Wood Block     : 037      :
;;;; 20 437 Warm Bass      : 072 120  :
;;;; 21 236 HiHatAstomp    : 065      : 
;;;; 22 232 HiHatAshut     : 066 067  :
;;;; 23 234 HiHatA 2/3     : 068 069  : 
;;;; 24 235 HiHatAopen     : 070 071  :
;;;;


(defun jazzy-traps (&key (parent procussion) channel)
  (let ((inst (make-instrument 'jazzy-traps
			       :parent parent
			       :channel channel
			       :program (procussion-program 'jazzy-traps)
			       :keynumber-map (procussion-keymap 33 120))))
    (defparameter jt-kick (make-instrument 'jt-kick
					   :parent inst
					   :keynumber-map
					   (circular-list-keynumber-map (append (range 33 37)
										(range 53 59)))))
    (defparameter jt-snare (make-instrument 'jt-snare
					    :parent inst
					    :keynumber-map (procussion-subkey-map
							    '((x . (38 "Tonal snare"))
							      (brush . (39))
							      (brush-2 . (40))
							      (x-2 . (59))
							      (x-3 . (60))
							      (x-4 . (61))
							      (x-5 . (62))
							      (x-6 . (63))
							      (x-7 . (64))))))
    (defparameter jt-hat (make-instrument 'jt-hat
					  :parent inst
					  :keynumber-map (procussion-subkey-map
							  '((x . (42 "A shut"))
							    (open . (44 "A open"))
							    (mod  . (52 "Mod wheel"))
							    (b . (66))
							    (b-ped . (65))
							    (b-opn . (68))
							    (b-open . (70))))))
    (defparameter jt-tom (make-instrument 'jt-tom
					  :parent inst
					  :keynumber-map (procussion-subkey-map
							  '((A . (41))
							    (B . (43))
							    (C . (45))
							    (D . (47))))))

    (defparameter jt-cym (make-instrument 'jt-cym
					  :parent inst
					  :keynumber-map (procussion-subkey-map
							  '((ride . (50))
							    (crash . (49))
							    (pang . (51))
							    (choke . (48))
							    (roll . (46))))))
    (defparameter jt-woodblock (make-instrument 'jt-woodblock
						:parent inst
						:keynumber-map #'(lambda (n)
								   (cond ((eq n :doc)
									  (format t "x --> wood block~%")
									  +rest+)
									 ((eq n 'x) 37)
									 (t +rest+)))))
    (defparameter jt-bass (make-instrument 'jt-bass
					   :parent inst
					   :keynumber-map (procussion-keymap 72 120)))
    

    (defparameter jazzy-traps inst)
    inst))
	
			       
