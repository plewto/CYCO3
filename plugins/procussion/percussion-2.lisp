;;;; CYCO Emu prucussionplugi0n: percussion-2
;;;;
;;;; Zone Stack         : key range  layers
;;;;  1 418 SlideConga  : 036 039    36  37 38 [39
;;;;  2 368 Tambourine  : 039 042    39] 40 41 [42 
;;;;  3 381 PlasticCow  : 042 045    42] 43 44 [45
;;;;  4 379 ShakaTam    : 045 048    45] 46 47 [48
;;;;  5 380 Synclaves   : 048 051    48] 49 50 [51
;;;;  6 350 BckwrdSwp   : 051 054    51] 52 53 [54
;;;;  7 374 Stereo Mba  : 054 057    54] 55 56 [57
;;;;  8 419 Shake it    : 057 060    57] 58 59 [60
;;;;  9 420 Conga Set   : 060 063    60] 61 62 [63  velocity switch
;;;; 10 422 Bongo Boy   : 063 066    63] 64 65 [66
;;;; 11 424 VlctyGuiro  : 066 069    66] 67 68 [69
;;;; 12 377 Log Drum    : 069 072    69] 70 71 [72
;;;; 13 370 Cow block   : 072 075    72] 73 74 [75
;;;; 14 423 Tone drum   : 075 078    75] 76 77 [78
;;;; 15 438 StoneBone   : 078 081    78] 79 80 [81
;;;; 16 444 Drumborine  : 081 084    81] 82 83 [84
;;;; 17 439 Amazon      : 084 087    84] 85 86 [87
;;;; 18 443 ShakerDrum  : 087 090    87] 88 89 [90
;;;; 19 465 ShakerBell  : 090 093    90] 91 92 [93
;;;; 20 457 ShakerDrum2 : 093 096    93] 94 95 [96
;;;; 21 365 Clave       : 098 098    98*
;;;; 22 365 Clave       : 098 098    98*
;;;; 23 392 Guiro Down  : 100 100    100
;;;; 24 424 VlctyGuiro  : 101 101    101
;;;;
;;;; PERCUSSION-2
;;;;    P2-SHAKER
;;;;    P2-CLAVE
;;;;    P2-COW
;;;;    P2-TAMBOURINE
;;;;    P2-DRUM

(let* ((shaker-keys '((shake   . (58))
		      (shake-2  . (59))
		      (shake-3  . (57))
		      (shake-4  . (60))
		      (tam      . (46))
		      (tam-2    . (47))
		      (tam-3    . (45))
		      (tam-4    . (48))
		      (drum     . (88))
		      (drum-1   . (89))
		      (drum-2   . (87))
		      (drum-3   . (90))
		      (bell     . (91))
		      (bell-1   . (92))
		      (bell-2   . (93))
		      (drum-4   . (94))
		      (drum-5   . (95))
		      (drum-6   . (96))))
       (clave-keys '((clave       . (98))
		     (clave-2     . (49))
		     (clave-3     . (50))
		     (clave-4     . (48))
		     (clave-5     . (51))
		     (guiro       . (67 "velocity switch"))
		     (guiro-2     . (68 "velocity switch"))
		     (guiro-3     . (66 "velocity switch"))
		     (guiro-4     . (69 "velocity switch"))
		     (guiro-down  . (100))))
       (cow-keys '((plastic      . (43))
		   (plastic-2    . (44))
		   (plastic-3    . (42))
		   (plastic-4    . (45))
		   (block        . (73))
		   (block-2      . (74))
		   (block-3      . (72))
		   (block-4      . (75))
		   (maraimba     . (55))
		   (maraimba-2   . (56))
		   (maraimba-3   . (54))
		   (maraimba-4   . (57))))
       (tambourine-keys '((tam      . (40))
			  (tam-2    . (41))
			  (tam-3    . (39))
			  (tam-4    . (42))
			  (drum     . (82))
			  (drum-2   . (83))
			  (drum-3   . (81))
			  (drum-4   . (84))))
       (drum-keys '((conga      . (36 "slide conga"))
		    (conga-2    . (37 "slide conga"))
		    (conga-3    . (38 "slide conga"))
		    (conga-4    . (39 "slide conga"))
		    (xconga     . (61 "velocity-switch"))
		    (xconga-2   . (62 "velocity-switch"))
		    (xconga-3   . (60 "velocity-switch"))
		    (xconga-4   . (63 "velocity-switch"))
		    (bongo      . (64))
		    (bongo-2    . (65))
		    (bongo-3    . (63))
		    (bongo-4    . (66))
		    (log        . (70))
		    (log-2      . (71))
		    (log-3      . (69))
		    (log-4      . (72))
		    (tone       . (76))
		    (tone-2     . (77))
		    (tone-3     . (75))
		    (tone-4     . (78))
		    (bone       . (79))
		    (bone-2     . (80))
		    (bone-3     . (78))
		    (bone-4     . (81))
		    (amazon     . (85))
		    (amazon-2   . (86))
		    (amazon-3   . (84))
		    (amazon-4   . (87))))
       (composite-keys (append shaker-keys clave-keys cow-keys tambourine-keys drum-keys))) 
  (defun percussion-2 (&key (parent procussion)(channel nil))
    (let ((p2 (make-instrument 'percussion-2
			       :parent parent
			       :channel channel
			       :remarks "Contains several layerd samples."
			       :program (procussion-program 'percussion2)
			       :keynumber-map (procussion-keymap 36 101 composite-keys))))
      (defparameter p2-shaker (make-instrument 'p2-shaker
					       :parent p2
					       :remarks ""
					       :keynumber-map (procussion-subkey-map shaker-keys)))
      (defparameter p2-clave (make-instrument 'p2-clave
					      :parent p2
					      :remarks ""
					      :keynumber-map (procussion-subkey-map clave-keys)))
      (defparameter p2-cow (make-instrument 'p2-cow
					    :parent p2
					    :remarks ""
					    :keynumber-map (procussion-subkey-map cow-keys)))
      (defparameter p2-tambourine (make-instrument 'p2-tambourine
						   :parent p2
						   :remarks ""
						   :keynumber-map (procussion-subkey-map tambourine-keys)))
      (defparameter p2-drum (make-instrument 'p2-drum
					     :parent p2
					     :remarks ""
					     :keynumber-map (procussion-subkey-map drum-keys)))
      (param percussion-2 p2)
      p2)))
    
