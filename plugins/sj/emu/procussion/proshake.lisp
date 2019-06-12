;;;; CYCO plugins sj emu procussion proshake.lisp
;;;;
;;;; Zone Stack             key range
;;;;  1 368 Tambourine    : 024 026     
;;;;  2 379 Shakatam      : 027 029     
;;;;  3 274 NoiseHat A    : 030 032     
;;;;  4 276 NoiseHat B    : 033 035     
;;;;  5 254 Syn Hat 2     : 036 038     
;;;;  6 267 HouseHat 5    : 039 041     
;;;;  7 269 Tam Hat       : 042 044     
;;;;  8 266 Castanet      : 045 047     
;;;;  9 389 CabasaFnt     : 048 050     
;;;; 10 390 CabasaBack    : 051 053     
;;;; 11 391 CabasaRoll    : 054 056     
;;;; 12 392 Guiro Down    : 057 058     
;;;; 13 393 Guiro Up      : 059         
;;;; 14 394 ShakerNet     : 060 062     
;;;; 15 395 ShakerSnp     : 063 065     
;;;; 16 396 ShakerBack    : 066 068     
;;;; 17 397 ShakerFrnt    : 069 071     
;;;; 18 419 Shake it      : 072 074     
;;;; 19 424 VlctyGuiro    : 075 077     
;;;; 20 479 VlctySweep    : 078 080     
;;;; 21 499 Jangler       : 081 083     
;;;; 22 519 Crack         : 084 086     
;;;; 23 520 CircleNoys    : 087 095     
;;;; 24 478 Surfin USA    : 096 127
;;;;
;;;; proshake
;;;;    |
;;;;    +-- tambourine
;;;;    +-- cabasa
;;;;    +-- shaker
;;;;    +-- guiro
;;;;    +-- noisy-hat
;;;;    +-- jangler
;;;;    +-- crack
;;;;    +-- circle-noise
;;;;    +-- castanet
;;;;    +-- surfinusa
;;;;

(let* ((tam-keys '((x      . (24 "Tambourine"))
		   (shake  . (27 "Shakatam"))
		   (hit    . (36 "Syn Hat 2"))
		   (palm   . (39 "Househat 5"))
		   (x2     . (25 "Tambourine"))
		   (shake2 . (28 "Shakatam"))
		   (hit2   . (37 "Syn Hat 2"))
		   (palm2  . (40 "Househat 5"))
		   (x3     . (26 "Tambourine"))
		   (shake3 . (29 "Shakatam"))
		   (hit3   . (38 "Syn Hat 2"))
		   (palm3  . (41 "Househat 5"))))
       (cabasa-keys '((front  . (48))
		      (back   . (51))
		      (roll   . (54))
		      (front2 . (49))
		      (back2  . (52))
		      (roll2  . (55))
		      (front3 . (50))
		      (back3  . (53))
		      (roll3  . (56))))
       (shaker-keys '((net     . (60))
		      (snap    . (63))
		      (back    . (66))
		      (front   . (69))
		      (shake   . (72))
		      (net2    . (61))
		      (snap2   . (64))
		      (back2   . (67))
		      (front2  . (70))
		      (shake2  . (73))
		      (net3    . (62))
		      (snap3   . (65))
		      (back3   . (68))
		      (front3  . (71))
		      (shake3  . (74))))
       (guiro-keys '((down       . (57))
		     (up         . (59))
		     (velocity   . (75))
		     (down2      . (58))
		     (velocity2  . (76))
		     (velocity3  . (77))))
       (noisy-keys '((A         . (30))
		     (B         . (33))
		     (TAM       . (42))
		     (VELOCITY  . (78))
		     (A2        . (31))
		     (B2        . (34))
		     (TAM2      . (43))
		     (VELOCITY2 . (79))
		     (A3        . (32))
		     (B3        . (35))
		     (TAM3      . (44))
		     (VELOCITY3 . (80))))
       (extras '((jangler    . (81))
		 (crack      . (84))
		 (castanet   . (45))
		 (jangler-2  . (82))
		 (crack-2    . (85))
		 (castanet-2 . (46))
		 (jangler-3  . (83))
		 (crack-3    . (86))
		 (castanet-3 . (47))
		 (cnoise     . (87))
		 (cnoise-2   . (88))
		 (cnoise-3   . (89))
		 (cnoise-4   . (90))
		 (cnoise-5   . (91))
		 (cnoise-6   . (92))
		 (cnoise-7   . (93))
		 (cnoise-8   . (94))
		 (cnoise-9   . (95))) ) )
  
  (defun proshake (&key (parent PROB))
    (let ((ps (instrument proshake
			  :parent parent
			  :remarks "Procussion proshake parent instrument"
			  :transient t
			  :keynumber-map (symbolic-keynumber-map
					  (append tam-keys cabasa-keys shaker-keys
						  guiro-keys noisy-keys extras)))))
      (instrument tambourine 
		  :parent ps 
		  :transient t
		  :keynumber-map (symbolic-keynumber-map tam-keys))
      (instrument cabasa  
		  :parent ps 
		  :transient t
		  :keynumber-map (symbolic-keynumber-map cabasa-keys)) 
      (instrument shaker 
		  :parent ps 
		  :transient t
		  :keynumber-map (symbolic-keynumber-map shaker-keys))
      (instrument guiro 
		  :parent ps 
		  :transient t
		  :keynumber-map (symbolic-keynumber-map guiro-keys))
      (instrument noisy-hat 
		  :parent ps 
		  :transient t
		  :keynumber-map (symbolic-keynumber-map noisy-keys))
      (instrument jangler 
		  :parent ps
		  :transient t
		  :keynumber-map (circular-keynumber-map 81 83))
      (instrument crack 
		  :parent ps
		  :transient t
		  :keynumber-map (circular-keynumber-map 84 86))
      (instrument circle-noise 
		  :parent ps
		  :transient t
		  :keynumber-map (circular-keynumber-map 87 95))
      (instrument castanet 
		  :parent ps
		  :transient t
		  :keynumber-map (circular-keynumber-map 45 47))
      (instrument surfinusa 
		  :parent ps
		  :transient t
		  :keynumber-map (circular-keynumber-map 96 127))
      ps))) 
