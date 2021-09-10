;;;; CYCO Emu Procussion plugin: percussion-3
;;;;
;;;; Zone Stack             key rang
;;;;  1 508 RitualDrum   :  36              *
;;;;  2 368 Tambourine   :  37              *
;;;;  3 423 Tone Drum    :  38              *
;;;;  4 351 Hand Drum    :  40              *
;;;;  5 364 HiTriangle   :  39              *
;;;;  6 377 Log Drum     :  41              *
;;;;  7 464 MelodicBell  :  42              *
;;;;  8 366 Castanet     :  44              *
;;;;  9 370 Cow Block    :  46              *
;;;; 10 455 Kalimdrum    :  43              *
;;;; 11 392 Guiro Down   :  45              *
;;;; 12 511 BabyBreath   :  49              *
;;;; 13 444 Drumborine   :  47              *
;;;; 14 509 CymbolDrum   :  48              *
;;;; 15 539 The Stick    :  50              *
;;;; 16 554 Rasparity    :  51              *
;;;; 17 443 ShakerDrum   :  52              *
;;;; 18 535 Cool Fx      :  54              *
;;;; 19 371 Ambi Stick   :  53              *
;;;; 20 538 The Shaker   :  56              *
;;;; 21 534 SpaceBlock   :  55              *
;;;; 22 457 ShakerDrm2   :  57 71           *
;;;; 23 465 ShakerBell   :  72 96           *
;;;; 24 389 CabasaFrnt   :  98 98           *
;;;;
;;;; PERCUSSION-3
;;;;    P3-DRUM
;;;;    P3-METAL
;;;;    P3-SHAKER
;;;;    p3-shaker-drum (melodic)
;;;;    p3-shaker-bell (melodic)


(let* ((drum-keys '((A .  (36 "RitualDrum"))
		    (B .  (38 "Tone Drum"))
		    (C .  (40 "Hand Drum"))
		    (D .  (41 "Log Drum"))
		    (E .  (42 "MelodicBell"))
		    (F .  (43 "Kalimdrum"))
		    (G .  (44 "Castanet"))
		    (H .  (46 "Cow Block"))
		    (I .  (47 "Drumborine"))
		    (J .  (48 "CymbolDrum"))
		    (K .  (50 "The Stick"))
		    (L .  (52 "ShakerDrum"))
		    (M .  (53 "Ambi Stick"))
		    (N .  (55 "SpaceBlock"))))
       (metal-keys '((A .  (37 "Tambourine"))
		     (B .  (39 "HiTriangle"))
		     (C .  (51 "Rasparity"))))
       (shaker-keys '(45 49 54 56 98)))
  (defun percussion-3 (&key (parent procussion) channel)
    (let ((p3 (make-instrument 'percussion-3
			       :parent parent
			       :channel channel
			       :program (procussion-program 'percussion3)
			       :keynumber-map (procussion-keymap 36 98)
			       :transient t)))
      (param p3-drums (make-instrument 'p3-drums
				       :parent p3
				       :keynumber-map (procussion-subkey-map drum-keys)))
      (param p3-metal (make-instrument 'p3-metal
				       :parent p3
				       :keynumber-map (procussion-subkey-map metal-keys)))
      (param p3-shaker (make-instrument 'p3-shaker
					:parent p3
					:keynumber-map (circular-list-keynumber-map shaker-keys)))
      (param p3-shaker-drum (make-instrument 'p3-shaker-drum
					     :parent p3
					     :keynumber-map (procussion-keymap 57 71)))
      (param p3-shaker-bell (make-instrument 'p3-shaker-bell
					     :parent p3
					     :keynumber-map (procussion-keymap 72 96)))
      (param percussion-3 p3)
      p3)))
  
