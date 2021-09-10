;;;; CYCO plugins ion emu procussion latin-drums.lisp
;;;;
;;;; Zone Stack               Key Range 
;;;; 01   398 Timbale 1A       36  38
;;;; 02   399 Timbale 1B       39  41
;;;; 03   400 Timbale 1C       42  44
;;;; 04   401 Timbale 2A       45  47
;;;; 05   402 Timbale 2B       48  50
;;;; 06   403 Timbale 2C       51  53
;;;; 07   404 Tumba Tone       54  56
;;;; 08   405 Tumba Rim        57  59
;;;; 09   406 Quinto Tone      60  62
;;;; 10   407 Quinto opn slap  63  65
;;;; 11   408 Quinto cls slap  66  68
;;;; 12   409 Quinto Tip       69  71
;;;; 13   410 Quinto Heel      72  74
;;;; 14   411 Hembra Tone      75  77
;;;; 15   412 Hembra Slap      78  80
;;;; 16   413 Macho Tip L      81  83
;;;; 17   414 Macho Tip R      84  86
;;;; 18   415 Macho Rim        87  89
;;;; 19   416 Macho Tone       90  92
;;;; 20   417 Macho Slap       93  96
;;;; 21   418 Slide Conga      98  98
;;;; 22   418 Slide Conga     100 102
;;;; 23   419 Shake It         78  78
;;;; 24   419 Shake It         66  66
;;;;
;;;; LATIN-DRUMS
;;;;    |
;;;;    +-- TIMBAL-1
;;;;    +-- TIMBAL-2
;;;;    +-- TUMBA
;;;;    +-- QUINTO
;;;;    +-- HEMBRA
;;;;    +-- MACHO
;;;;    +-- CONGA
;;;;    +-- SHAKEIT
;;;;


(let* ((tim1-list '((A  . (36 ""))
		    (B  . (39 ""))
		    (C  . (42 ""))
		    (A2 . (37 ""))
		    (B2 . (40 ""))
		    (C2 . (43 ""))
		    (A3 . (38 ""))
		    (B3 . (41 ""))
		    (C3 . (44 ""))))
       (tim2-list '((A  . (45 ""))
		    (B  . (48 ""))
		    (C  . (51 ""))
		    (A2 . (46 ""))
		    (B2 . (49 ""))
		    (C2 . (52 ""))
		    (A3 . (41 ""))
		    (B3 . (44 ""))
		    (C3 . (44 ""))))
       (tumba-list '((tone    . (54 ""))
		     (rim     . (57 ""))
		     (tone-2  . (55 ""))
		     (rim-2   . (58 ""))
		     (tone-3  . (56 ""))
		     (rim-3   . (59 ""))))
       (quinto-list '((tone      . (60 ""))
		      (oslap     . (63 "Open Slap"))
		      (cslap     . (66 "Closed Slap"))
		      (tip       . (69 ""))
		      (heal      . (72 ""))
		      (tone-2    . (61 ""))
		      (oslap-2   . (64 "Open Slap"))
		      (cslap-2   . (67 "Closed Slap"))
		      (tip-2     . (70 ""))
		      (heal-2    . (73 ""))
		      (tone-3    . (62 ""))
		      (oslap-3   . (65 "Open Slap"))
		      (cslap-3   . (68 "Closed Slap"))
		      (tip-3     . (71 ""))
		      (heal-3    . (74 ""))))
       (hembra-list '((tone   . (75 ""))
		      (slap   . (78 ""))
		      (tone-2 . (76 ""))
		      (slap-2 . (79 ""))
		      (tone-3 . (77 ""))
		      (slap-3 . (80 ""))))
       (macho-list '((tone    . (90 ""))
		     (slap    . (93 ""))
		     (tipl    . (81 "Left tip"))
		     (tipr    . (84 "Right tip"))
		     (rim     . (87 ""))
		     (tone-2  . (91 ""))
		     (slap-2  . (94 ""))
		     (tipl-2  . (82 "Left tip"))
		     (tipr-2  . (85 "Right tip"))
		     (rim-2   . (88 ""))
		     (tone-3  . (92 ""))
		     (slap-3  . (95 ""))
		     (tipl-3  . (83 "Left tip"))
		     (tipr-3  . (86 "Right tip"))
		     (rim-3   . (89 ""))
		     (slap-4  . (96 ""))))
       (conga-list '((a  . ( 98 "Slide Conga 1"))
		     (b  . (100 "Slide Conga 2"))
		     (c  . (101 "Slide Conga 2"))
		     (d  . (102 "Slide Conga 2"))))
       (shaker-list '((a . (78 ""))
		      (b . (66 ""))))
       (composite-list '((TIMBALE-A    . (36 ""))
			 (TIMBALE-B    . (40 ""))
			 (TIMBALE-C    . (42 ""))
			 (TIMBALE-D    . (37 ""))
			 (TIMBALE-E    . (39 ""))
			 (TIMBALE-F    . (43 ""))
			 (TIMBALE-G    . (38 ""))
			 (TIMBALE-H    . (41 ""))
			 (TIMBALE-I    . (44 ""))
			 (TIMBALE-J    . (45 ""))
			 (TIMBALE-K    . (48 ""))
			 (TIMBALE-L    . (51 ""))
			 (TIMBALE-M    . (46 ""))
			 (TIMBALE-N    . (49 ""))
			 (TIMBALE-O    . (52 ""))
			 (TIMBALE-P    . (47 ""))
			 (TIMBALE-Q    . (50 ""))
			 (TIMBALE-R    . (53 ""))
			 (TUMBA-A      . (54 ""))
			 (TUMBA-B      . (57 ""))
			 (TUMBA-C      . (55 ""))
			 (TUMBA-D      . (58 ""))
			 (TUMBA-E      . (56 ""))
			 (TUMBA-F      . (59 ""))
			 (QUINTO-A     . (60 ""))
			 (QUINTO-B     . (63 ""))
			 (QUINTO-C     . (66 ""))
			 (QUINTO-D     . (69 ""))
			 (QUINTO-E     . (72 ""))
			 (QUINTO-F     . (61 ""))
			 (QUINTO-G     . (64 ""))
			 (QUINTO-H     . (67 ""))
			 (QUINTO-I     . (70 ""))
			 (QUINTO-J     . (73 ""))
			 (QUINTO-K     . (62 ""))
			 (QUINTO-L     . (65 ""))
			 (QUINTO-M     . (68 ""))
			 (QUINTO-N     . (71 ""))
			 (QUINTO-O     . (74 ""))
			 (HEMBRA-A     . (75 ""))
			 (HEMBRA-B     . (78 ""))
			 (HEMBRA-C     . (76 ""))
			 (HEMBRA-D     . (79 ""))
			 (HEMBRA-E     . (77 ""))
			 (HEMBRA-F     . (80 ""))
			 (MACHO-A      . (90 ""))
			 (MACHO-B      . (93 ""))
			 (MACHO-C      . (81 ""))
			 (MACHO-D      . (84 ""))
			 (MACHO-E      . (87 ""))
			 (MACHO-F      . (91 ""))
			 (MACHO-G      . (94 ""))
			 (MACHO-H      . (82 ""))
			 (MACHO-I      . (85 ""))
			 (MACHO-J      . (88 ""))
			 (MACHO-K      . (92 ""))
			 (MACHO-L      . (95 ""))
			 (MACHO-M      . (83 ""))
			 (MACHO-N      . (86 ""))
			 (MACHO-O      . (89 ""))
			 (MACHO-P      . (96 ""))
			 (CONGA-A      . (98 ""))
			 (CONGA-B      . (100 ""))
			 (CONGA-C      . (99 ""))
			 (CONGA-D      . (101 ""))
			 (CONGA-E      . (102 ""))
			 (SHAKER-A     . (78 ""))
			 (SHAKER-B     . (66 "")) )))
  
  (defun latin-drums (&key (parent procussion)(channel nil))
    (let* ((latdrm (instrument latin-drums
  			       :parent parent
			       :remarks "Procussion latin-drums parent instrument."
  			       :channel channel
  			       :program (procussion-program 'latin-drums)
			       :keynumber-map (procussion-keymap 36 102 composite-list)
  			       :transient t)))
      (instrument ld-timbal-1
		  :parent latdrm
		  :remarks "Procussion latin-drums Timbales 1"
		  :transient t
		  :keynumber-map (procussion-subkey-map tim1-list))

      (instrument ld-timbal-2 
		  :parent latdrm
		  :remarks "Procussion latin-drums Timbales 2"
		  :transient t
		  :keynumber-map (procussion-subkey-map tim2-list))

      (instrument ld-tumba 
		  :parent latdrm
		  :remarks "Procussion latin-drums Tumba drum"
		  :transient t
		  :keynumber-map (procussion-subkey-map tumba-list))

      (instrument ld-quinto 
		  :parent latdrm
		  :remarks "Procussion latin-drums Quinto drum"
		  :transient t
		  :keynumber-map (procussion-subkey-map quinto-list))
      
      (instrument ld-hembra 
		  :parent latdrm
		  :remarks "Procussion latin-drums Hembra drum"
		  :transient t
		  :keynumber-map (procussion-subkey-map hembra-list))

      (instrument ld-macho 
		  :parent latdrm
		  :remarks "Procussion latin-drums macho drum"
		  :transient t
		  :keynumber-map (procussion-subkey-map macho-list))

      (instrument ld-conga 
		  :parent latdrm
		  :remarks "Procussion latin-drums Conga"
		  :transient t
		  :keynumber-map (procussion-subkey-map conga-list))

      (instrument ld-shakeit 
		  :parent latdrm
		  :remarks "Procussion latin-drums Shakers"
		  :transient t
		  :keynumber-map (procussion-subkey-map shaker-list))
      (defparameter latin-drums latdrm) 
      latdrm)))
