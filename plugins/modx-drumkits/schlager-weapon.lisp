;;;; CYCO MODX drumkit instrument
;;;; schlager-weapon 1 2 & 3
;;;;
;;;; 24 : 5457 Sd TB Comp1          
;;;; 25 : 5456 Sd TB Q              
;;;; 26 : 2531 Abstract Fx1         
;;;; 27 : 2534 Abstract Fx4         
;;;; 28 : 2537 Damage Fx1           
;;;; 29 : 2536 Bazer Fx             
;;;; 30 : 2539 Damage Fx3           
;;;; 31 : 2532 Abstract Fx2         
;;;; 32 : 2563 Electric Fx Perc8    
;;;; 33 : 4688 Dub BD 2 St          
;;;; 34 : 5451 Sd ShortDry2         
;;;; 35 : 4688 Dub BD 2 St          
;;;; 36 : 4688 Dub BD 2 St          
;;;; 37 : 5452 Sd ShortGate1        
;;;; 38 : 5456 Sd TB Q              
;;;; 39 : 4190 RX11 Clap            
;;;; 40 : 5445 Sd Clap1             
;;;; 41 : 6138 Comp EleTom Low      
;;;; 42 : 5830 Trap HH 3 St         
;;;; 43 : 6138 Comp EleTom Low      
;;;; 44 : 5822 HH TecClosed2        
;;;; 45 : 6138 Comp EleTom Low      
;;;; 46 : 5819 HH OpenNine          
;;;; 47 : 6137 Comp EleTom High     
;;;; 48 : 6137 Comp EleTom High     
;;;; 49 : 6199 PI Crash 15          
;;;; 50 : 6137 Comp EleTom High     
;;;; 51 : 6262 Fine Ride Bow        
;;;; 52 : 6292 China4               
;;;; 53 : 6264 Fine Ride Cup        
;;;; 54 : 3088 Pandeiro1 Tip        
;;;; 55 : 6200 PI Crash 16          
;;;; 56 : 2513 Hi Q2                
;;;; 57 : 6200 PI Crash 16          
;;;; 58 : 2513 Hi Q2                
;;;; 59 : 6338 Electro Ride Mn      
;;;; 60 : 2790 Acoustic Bass FX07   
;;;; 61 : 2281 Mini Brip6           
;;;; 62 : 2844 Tambourine1 Shake    
;;;; 63 : 2277 Mini Brip2           
;;;; 64 : 2967 Quinto1 Slap         
;;;; 65 : 3288 Tabla1 Bya           
;;;; 66 : 3272 Djembe1 Edge         
;;;; 67 : 3289 Tabla1 Nah           
;;;; 68 : 3545 Barasim Tak          
;;;; 69 : 4188 Electric Clap1       
;;;; 70 : 2820 ShakerB Sw           
;;;; 71 : 2392 Hop FX1              
;;;; 72 : 2550 Noize Fx1            
;;;; 73 : 2552 Noize Rize1          
;;;; 74 : 2553 Noize Rize2          
;;;; 75 : 2533 Abstract Fx3         
;;;; 76 : 2535 Abstract Fx5         
;;;; 77 : 4286 Analog Click         
;;;; 78 : 5780 HH Closed T8-1       
;;;; 79 : 5431 Sd FM                
;;;; 80 : 5570 HH Half Open2 RL     
;;;; 81 : 4276 Claves T8-1          
;;;; 82 : 5781 HH Open T8-1         
;;;; 83 : 6346 Reverse Cymbal1      
;;;; 84 : 4558 Bd DanceGate1        
;;;; 85 : 4681 Bd EDM Gate          
;;;; 86 : 4559 Bd DanceGate2        
;;;; 87 : 2547 Damage Fx11          
;;;; 88 : 4662 Bd Stack3            
;;;; 89 : 4680 Bd EDM10             
;;;; 90 : 5178 Sd BreakRim          
;;;; 91 : 2570 Electric Fx Perc15   
;;;; 92 : 2551 Noize Fx2            
;;;; 93 : 2544 Damage Fx8           
;;;; 94 : 2561 Electric Fx Perc6    
;;;; 95 : 2559 Electric Fx Perc4    
;;;; 96 : 2564 Electric Fx Perc9
;;;;

(let ((common-klist (general-midi-drum-keylist
		     '((snare3            . (24))
		       (snare4            . (25))
		       (fx1               . (26))
		       (noise01           . (27))
		       (noise02           . (28))
		       (noise03           . (29))
		       (noise04           . (30))
		       (fx2               . (31))
		       (fx3               . (32))
		       (kick-dub          . (33))
		       (snare-dry         . (34))
		       (snare-gate        . (37))
		       (tambourine-tip    . (54))
		       (crash             . (55))
		       (hi-q              . (56))
		       (hi-q2             . (58))
		       (click             . (60))
		       (brip              . (61))
		       (tambourine-shake  . (62))
		       (birb2             . (63))
		       (quinto-slap       . (64))
		       (tabla-bya         . (65))
		       (djembe            . (66))
		       (tabla-nah         . (67))
		       (barasim-tak       . (68))
		       (clap              . (69))
		       (fx4               . (70))
		       (noise05           . (71))
		       (noise06           . (72))
		       (noise07           . (73))
		       (noise08           . (74))
		       (fx5               . (75))
		       (noise09           . (76))
		       (click2            . (77))
		       (hat-closed        . (78))
		       (snare-fm          . (79))
		       (hat-op            . (80))
		       (clave             . (81))
		       (hat-opn           . (82))
		       (cym               . (83))
		       (kick-gate         . (84))
		       (kick-gate2        . (85))
		       (kick-gate3        . (86))
		       (fx6               . (87))
		       (kick-stack        . (88))
		       (kick-edm          . (89))
		       (snare-rim         . (90))
		       (noise10           . (91))
		       (noise11           . (92))
		       (noise12           . (93))
		       (noise13           . (94))
		       (noise14           . (95))
		       (noise15           . (96)))
		     '(STICK TAMBOURINE SPLASH COW VIBRASLAP BONGO-HIGH BONGO-LOW
			     CONGA-HIGH CONGA-OPEN CONGA-LOW TIMBALE-HIGH TIMBALE-LOW
			     AGOGO-HIGH AGOGO-LOW CABASA MARACAS WHISTLE1 WHISTLE2
			     GUIRO-SHORT GUIRO-LONG CLAVE BLOCK-HIGH BLOCK-LOW
			     CUICA-MUTE CUICA-OPEN)))
      (kick-klist '((A          . (35))
		    (B          . (36))
		    (DUB        . (33))
		    (GATE       . (84))
		    (GATE2      . (85))
		    (GATE3      . (86))
		    (STACK      . (88))
		    (EDM        . (89))))
      (snare-klist '((A         . (38))
		     (B         . (40))
		     (C         . (24))
		     (D         . (25))
		     (DRY       . (34))
		     (GATE      . (37))
		     (FM        . (79))
		     (RIM       . (90))))
      (tom-klist '((A     . (41))
		   (B     . (43))
		   (C     . (45))
		   (D     . (47))
		   (E     . (48))
		   (F     . (50))))
      (hat-klist '((X     . (78))
		   (OP    . (80))
		   (OPN   . (82))
		   (OPEN  . (46))
		   (PED   . (44))))
      (cymbal-klist '((RIDE       . (51))
		      (RIDE2      . (59))		
		      (BELL       . (53))
		      (CRASH      . (55))
		      (CRASH2     . (49))
		      (CRASH3     . (57))
		      (CHINESE    . (52))
		      (reverse    . (83))))
      (click-klist '((x        . (60))
		     (x2       . (77))
		     (CLAVE    . (81))
		     (CLAP     . (69))))
      (drums-klist '(65 67 64 66 68))
      (fx-klist '(26 31 32 70 75 87))
      (noise-klist '(27 28 29 30 71 72 73 74 76 91 92 93 94 95)) )

  (defun schlager-weapon (channel &key (performance *current-modx-performance*))
    (let ((scwep (make-instrument 'schlager-weapon
				  :channel channel
				  :parent performance
				  :keynumber-map (symbolic-keynumber-map common-klist)
				  :remarks "May be used for Shhlager Weapon 1, 2 & 3.")))
      (defparameter schlager-weapon scwep)
      (instrument sw-kick :parent scwep :keynumber-map (symbolic-keynumber-map kick-klist))
      (instrument sw-snare :parent scwep :keynumber-map (symbolic-keynumber-map snare-klist))
      (instrument sw-tom :parent scwep :keynumber-map (symbolic-keynumber-map tom-klist))
      (instrument sw-hat :parent scwep :keynumber-map (symbolic-keynumber-map hat-klist))
      (instrument sw-cymbal :parent scwep :keynumber-map (symbolic-keynumber-map cymbal-klist))
      (instrument sw-click :parent scwep :keynumber-map (symbolic-keynumber-map click-klist))
      (instrument sw-drums :parent scwep :keynumber-map (circular-list-keynumber-map drums-klist))
      (instrument sw-fx :parent scwep :keynumber-map (circular-list-keynumber-map fx-klist))
      (instrument sw-noise :parent scwep :keynumber-map (circular-list-keynumber-map noise-klist))
      (instrument sw-birb :parent scwep :keynumber-map (circular-list-keynumber-map '(61 63)))
      (instrument sw-triangle :parent scwep :keynumber-map (circular-list-keynumber-map '(80 81)))
      (instrument sw-hiq :parent scwep :keynumber-map (circular-list-keynumber-map '(56 58)))
      (instrument sw-tambourine :parent scwep :keynumber-map (circular-list-keynumber-map '(54 62)))
      scwep)) )
    
(register-modx-drumkit-info 'schlager-weapon)
      

