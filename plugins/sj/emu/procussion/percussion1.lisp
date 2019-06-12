;;;; CYCO plugins sj emu procussion percusssion1.lisp
;;;;
;;;; Zone    Stack             key-range  cyco-instrument
;;;; Z01     S306 TempleBlok   036 038    WoodBlock
;;;; Z02     S367 Wood Block   039 041    WoodBlock
;;;; Z03     S365 Clave        042 044    Clave
;;;; Z04     S361 Agogo Bell   045 047    Agogo
;;;; Z05     S362 Triangle     048 050    Triangle
;;;; Z06     S363 TringlMute   051 053    Trinagle
;;;; Z07     S383 Mambo Open   054 056    Mambo
;;;; Z08     S384 Mambo Clsd   057 059    Mambo
;;;; Z09     S385 Campana Op   060 062    Campana
;;;; Z10     S386 CampanaHel   063 065    Campana
;;;; Z11     S387 ChaCha Opn   066 068    ChaCha
;;;; Z12     S388 ChaCha Cls   069 071    ChaCha
;;;; Z13     S389 CabasaFrnt   072 074  * Cabasa1
;;;; Z14     S390 CabasaBak    075 077  * Cabasa1
;;;; Z15     S391 CabasaRoll   078 080  * Cabasa1
;;;; Z16     S393 Guiro Up     081 083  * Guiro
;;;; Z17     S394 ShakerNet    084 086    shakernet
;;;; Z18     S395 ShakerSnp    087 089    shakernet
;;;; Z19     S396 ShakerBack   090 092    shakernet
;;;; Z20     S397 Shakerfrnt   093 096    shakernet
;;;; Z21     S365 Clave        098 098    Clave
;;;; Z22     S365 Clave        098 098    Clave
;;;; Z23     S392 Guiro Down   100 100    Guiro
;;;; Z24     S424 VlctyGuiro   101 101    Guiro
;;;;
;;;; percussion1
;;;;    |
;;;;    +-- p1-woodblock
;;;;    +-- p1-clave
;;;;    +-- p1-agogo
;;;;    +-- p1-triangle
;;;;    +-- p1-mambo
;;;;    +-- p1-campana
;;;;    +-- p1-chacha
;;;;    +-- p1-cabasa
;;;;    +-- p1-guiro
;;;;    +-- p1-shakernet
;;;;

(param percussion1 nil)

(let ((woodblock-keys '((A  . (36 ""))
			(A2 . (37 ""))
			(A3 . (38 ""))
			(B  . (39 ""))
			(B2 . (40 ""))
			(B3 . (41 ""))))
      (clave-keys '((A  . (42 ""))
		    (B  . (98 ""))
		    (A1 . (43 ""))
		    (A2 . (44 ""))))
      (agogo-keys '((A . (45 ""))
		    (A1 . (46 ""))
		    (A2 . (47 ""))))
      (triangle-keys '((A       . (48 ""))
		       (A2      . (49 ""))
		       (A3      . (50 ""))
		       (MUTE    . (51 ""))
		       (MUTE2   . (52 ""))
		       (MUTE3   . (53 ""))))
      (mambo-keys '((OP    . (54 "Open"))
		    (OP2   . (55 "Open"))
		    (OP3   . (56 "Open"))
		    (CL    . (57 "Closed"))
		    (CL2   . (58 "Closed"))
		    (CL3   . (59 "Closed"))))
      (campana-keys '((OP    . (60 "Open"))
		      (OP2   . (61 "Open"))
		      (OP3   . (62 "Open"))
		      (HL    . (63 "Heel"))
		      (HL2   . (64 "Heel"))
		      (HL3   . (65 "Heel"))))
      (chacha-keys '((OP    . (66 "Open"))
		     (OP2   . (67 "Open"))
		     (OP3   . (68 "Open"))
		     (CL    . (69 "Closed"))
		     (CL2   . (70 "Closed"))
		     (CL3   . (71 "Closed"))))
      (cabasa-keys '((FRNT    . (72 ""))
		     (BACK    . (75 ""))
		     (ROLL    . (78 ""))
		     (FRNT2   . (73 ""))
		     (BACK2   . (76 ""))
		     (ROLL2   . (79 ""))
		     (FRNT3   . (74 ""))
		     (BACK3   . (77 ""))
		     (ROLL3   . (80 ""))))
      (guiro-keys '((UP      . (81 ""))
		    (DOWN    . (100 ""))
		    (VLCTY   . (101 ""))
		    (UP2     . (82 ""))
		    (UP3     . (83 ""))))
      (shakernet-keys '((A       . (84 ""))
			(SNAP    . (87 ""))
			(BACK    . (90 ""))
			(FRNT    . (93 ""))
			(A2      . (85 ""))
			(SNAP2   . (88 ""))
			(BACK2   . (91 ""))
			(FRNT2   . (94 ""))
			(A3      . (86 ""))
			(SNAP3   . (89 ""))
			(BACK3   . (92 ""))
			(FRNT3   . (95 ""))))
      (composite-keys '((woodblock-1 . (36 ""))
			(woodblock-2 . (37 ""))
			(woodblock-3 . (38 ""))
			(woodblock-4 . (39 ""))
			(woodblock-5 . (40 ""))
			(woodblock-6 . (41 ""))
			(clave-1     . (42 ""))
			(clave-2     . (98 ""))
			(clave-3     . (43 ""))
			(clave-4     . (44 ""))
			(agogo-1     . (45 ""))
			(agogo-2     . (46 ""))
			(agogo-3     . (47 ""))
			(triangle-1  . (48 ""))
			(triangle-2  . (49 ""))
			(triangle-3  . (50 ""))
			(triangle-4  . (51 ""))
			(triangle-5  . (52 ""))
			(triangle-6  . (53 ""))
			(mambo-1     . (54 "Open"))
			(mambo-2     . (55 "Open"))
			(mambo-3     . (56 "Open"))
			(mambo-4     . (57 "Closed"))
			(mambo-5     . (58 "Closed"))
			(mambo-6     . (59 "Closed"))
			(campana-1   . (60 "Open"))
			(campana-2   . (61 "Open"))
			(campana-3   . (62 "Open"))
			(campana-4   . (63 "Heel"))
			(campana-5   . (64 "Heel"))
			(campana-6   . (65 "Heel"))
			(chacha-1    . (66 "Open"))
			(chacha-2    . (67 "Open"))
			(chacha-3    . (68 "Open"))
			(chacha-4    . (69 "Closed"))
			(chacha-5    . (70 "Closed"))
			(chacha-6    . (71 "Closed"))
			(cabasa-7    . (72 ""))
			(cabasa-8    . (75 ""))
			(cabasa-9    . (78 ""))
			(cabasa-10   . (73 ""))
			(cabasa-11   . (76 ""))
			(cabasa-12   . (79 ""))
			(cabasa-13   . (74 ""))
			(cabasa-14   . (77 ""))
			(cabasa-15   . (80 ""))
			(guiro-1     . (81 ""))
			(guiro-2     . (100 ""))
			(guiro-3     . (101 ""))
			(guiro-4     . (82 ""))
			(guiro-5     . (83 ""))
			(shaker-1    . (84 ""))
			(shaker-2    . (87 ""))
			(shaker-3    . (90 ""))
			(shaker-4    . (93 ""))
			(shaker-5    . (85 ""))
			(shaker-6    . (88 ""))
			(shaker-7    . (91 ""))
			(shaker-8    . (94 ""))
			(shaker-9    . (86 ""))
			(shaker-10   . (89 ""))
			(shaker-11   . (92 ""))
			(shaker-12   . (95 "")) )))
  
  (defun percussion1 (&key (parent PROB)(channel nil) articulation-map dynamic-map)
    (let ((inst (instrument percussion1
			    :remarks "Emu Procussion Percussion1 parent instrument"
			    :parent parent
			    :channel channel
			    :program (procussion-program 'percussion1)
			    :keynumber-map (symbolic-keynumber-map composite-keys)
			    :articulation-map articulation-map
			    :dynamic-map dynamic-map
			    :transient t)))
      (instrument p1-woodblock
		  :parent inst
		  :remarks "Procussion Percussion1 woodblock"
		  :transient t
		  :keynumber-map (symbolic-keynumber-map woodblock-keys))
      (instrument p1-clave
		  :parent inst
		  :remarks "Procussion Percussion1 clave"
		  :transient t
		  :keynumber-map (symbolic-keynumber-map clave-keys))
      (instrument p1-agogo
		  :parent inst
		  :remarks "Procussion Percussion1 agogo"
		  :transient t
		  :keynumber-map (symbolic-keynumber-map agogo-keys))
      (instrument p1-triangle
		  :parent inst
		  :remarks "Procussion Percussion1 triangle"
		  :transient t
		  :keynumber-map (symbolic-keynumber-map triangle-keys))
      (instrument p1-mambo
		  :parent inst
		  :remarks "Procussion Percussion1 mambo"
		  :transient t
		  :keynumber-map (symbolic-keynumber-map mambo-keys))
      (instrument p1-campana
		  :parent inst
		  :remarks "Procussion Percussion1 campana"
		  :transient t
		  :keynumber-map (symbolic-keynumber-map campana-keys))
      (instrument p1-chacha
		  :parent inst
		  :remarks "Procussion Percussion1 chacha"
		  :transient t
		  :keynumber-map (symbolic-keynumber-map chacha-keys))
      (instrument p1-cabasa
		  :parent inst
		  :remarks "Procussion Percussion1 cabasa1"
		  :transient t
		  :keynumber-map (symbolic-keynumber-map cabasa-keys))
      (instrument p1-guiro
		  :parent inst
		  :remarks "Procussion Percussion1 guiro"
		  :transient t
		  :keynumber-map (symbolic-keynumber-map guiro-keys))
      (instrument p1-shakernet
		  :parent inst
		  :remarks "Procussion Percussion1 shakernet"
		  :transient t
		  :keynumber-map (symbolic-keynumber-map shakernet-keys))
      inst))) 

  
      
