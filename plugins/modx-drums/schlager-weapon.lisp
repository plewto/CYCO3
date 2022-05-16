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

(in-package :modx)
(let*  ((general-klist '((KICK-A       . (33 "4688 Dub BD 2 St"))
			 (KICK-B       . (35 "4688 Dub BD 2 St"))
			 (KICK-C       . (36 "4688 Dub BD 2 St"))
			 (KICK-GATE    . (84 "4558 Bd DanceGate1"))
			 (KICK-EDM     . (85 "4681 Bd EDM Gate"))
			 (KICK-GATE2   . (86 "4559 Bd DanceGate2"))
			 (KICK-STACK   . (88 "4662 Bd Stack3"))
			 (KICK-EDM2    . (89 "4680 Bd EDM10"))
			 (SNARE-X      . (34 "5451 Sd ShortDry2"))
			 (SNARE-TB     . (24 "5457 Sd TB Comp1"))
			 (SNARE-TBQ    . (25 "5456 Sd TB Q"))
			 (SNARE-GATE   . (37 "5452 Sd ShortGate1"))
			 (SNARE-TBQ2   . (38 "5456 Sd TB Q"))
			 (SNARE-CLAP   . (40 "5445 Sd Clap1"))
			 (SNARE-FM     . (79 "5431 Sd FM"))
			 (SNARE-RIM    . (90 "5178 Sd BreakRim"))
			 (TOM-A      . (41 "6138 Comp EleTom Low"))
			 (TOM-B      . (43 "6138 Comp EleTom Low"))
			 (TOM-C      . (45 "6138 Comp EleTom Low"))
			 (TOM-D      . (47 "6137 Comp EleTom High"))
			 (TOM-E      . (48 "6137 Comp EleTom High"))
			 (TOM-F      . (50 "6137 Comp EleTom High"))
			 (HAT-X       . (42 "5830 Trap HH 3 St"))
			 (HAT-TEC     . (44 "5822 HH TecClosed2"))
			 (HAT-OPEN    . (46 "5819 HH OpenNine"))
			 (HAT-TB-X    . (78 "5780 HH Closed T8-1"))
			 (HAT-TB-OP   . (80 "5570 HH Half Open2 RL"))
			 (HAT-TB-OPEN . (82 "5781 HH Open T8-1"))
			 (CYM-RIDE    . (51 "6262 Fine Ride Bow"))
			 (CYM-CUP     . (53 "6264 Fine Ride Cup"))
			 (CYM-ERIDE   . (59 "6338 Electro Ride Mn"))
			 (CYM-CRASH   . (55 "6200 PI Crash 16"))
			 (CYM-CRASH2  . (49 "6199 PI Crash 15"))
			 (CYM-CRASH3  . (57 "6200 PI Crash 16"))
			 (CYM-CHINA   . (52 "6292 China4"))
			 (CYM-REVERSE . (83 "6346 Reverse Cymbal1"))
			 (CLAP-X        . (39 "4190 RX11 Clap"))
			 (CLAP-ELECTRO  . (69 "4188 Electric Clap1"))
			 (CLAP-ANALOG   . (77 "4286 Analog Click"))
			 (CLAVE         . (81 "4276 Claves T8-1"))
			 (SHAKER                . (70 "2820 ShakerB Sw"))
			 (SHAKER-TAMBOURINE     . (62 "2844 Tambourine1 Shake"))
			 (SHAKER-TAMBOURINE-TIP . (54 "3088 Pandeiro1 Tip hh"))
			 (DRUM-TABLA   . (65 "3288 Tabla1 Bya"))
			 (DRUM-TABLE2  . (67 "3289 Tabla1 Nah"))
			 (DRUM-QUINTO  . (64 "2967 Quinto1 Slap"))
			 (DRUM-DJAMBE  . (66 "3272 Djembe1 Edge"))
			 (DRUM-BARASIM . (68 "3545 Barasim Tak"))
			 (HIQ-A  . (56 "2513 Hi Q2"))
			 (HIQ-B  . (58 "2513 Hi Q2"))
			 (HIQ-C  . (61 "2281 Mini Brip6"))
			 (HIQ-D  . (63 "2277 Mini Brip2"))
			 (FXPERC-A . (32 "2563 Electric Fx Perc8"))
			 (FXPERC-B . (91 "2570 Electric Fx Perc15"))
			 (FXPERC-C . (94 "2561 Electric Fx Perc6"))
			 (FXPERC-D . (95 "2559 Electric Fx Perc4"))
			 (FXPERC-E . (96 "2564 Electric Fx Perc9"))
			 (DAMGE-A . (28 "2537 Damage Fx1"))
			 (DAMGE-B . (30 "2539 Damage Fx3"))
			 (DAMGE-C . (87 "2547 Damage Fx11"))
			 (DAMGE-D . (93 "2544 Damage Fx8"))
			 (NOISE-A . (92 "2551 Noize Fx2"))
			 (NOISE-B . (72 "2550 Noize Fx1"))
			 (NOISE-C . (73 "2552 Noize Rize1"))
			 (NOISE-D . (74 "2553 Noize Rize2"))
			 (FX-A . (29 "2536 Bazer Fx"))
			 (FX-B . (60 "2790 Acoustic Bass FX07 (VELOCITY)"))
			 (FX-C . (71 "2392 Hop FX1"))
			 (FX-D . (26 "2531 Abstract Fx1"))
			 (FX-E . (27 "2534 Abstract Fx4"))
			 (FX-F . (31 "2532 Abstract Fx2"))
			 (FX-G . (75 "2533 Abstract Fx3"))
			 (FX-H . (76 "2535 Abstract Fx5"))))
	(clave-klist '((x . (81 "4276 Claves T8-1"))))
	(kick-klist (extract-sub-symbolic-keylist 'kick general-klist))
	(snare-klist (extract-sub-symbolic-keylist 'snare general-klist))
	(tom-klist (extract-sub-symbolic-keylist 'tom general-klist))
	(hat-klist (extract-sub-symbolic-keylist 'hat general-klist))
	(cym-klist (extract-sub-symbolic-keylist 'cym general-klist))
	(clap-klist (extract-sub-symbolic-keylist 'clap general-klist))
	(shaker-klist (extract-sub-symbolic-keylist 'shaker general-klist))
	(drum-klist (extract-sub-symbolic-keylist 'drum general-klist))
	(hiq-klist (extract-sub-symbolic-keylist 'hiq general-klist))
	(fxperc-klist (extract-sub-symbolic-keylist 'fxperc general-klist))
	(damage-klist (extract-sub-symbolic-keylist 'damage general-klist))
	(noise-klist (extract-sub-symbolic-keylist 'noise general-klist))
	(fx-klist (extract-sub-symbolic-keylist 'fx general-klist))
	(sw (make-main-instrument schlager-weapon general-klist)) )

  (make-sub sw-kick sw kick-klist)
  (make-sub sw-snare sw snare-klist)
  (make-sub sw-tom sw tom-klist)
  (make-sub sw-hat sw hat-klist)
  (make-sub sw-cym sw cym-klist)
  (make-sub sw-clap sw clap-klist)
  (make-sub sw-clave sw clave-klist)
  (make-sub sw-shaker sw shaker-klist)
  (make-sub sw-drum sw drum-klist)
  (make-sub sw-hiq sw hiq-klist)
  (make-sub sw-fxperc sw fxperc-klist)
  (make-sub sw-damage sw damage-klist)
  (make-sub sw-noise sw noise-klist)
  (make-sub sw-fx sw fx-klist) )

(export '(sw-kick
	  sw-snare
	  sw-tom
	  sw-hat
	  sw-cym
	  sw-clap
	  sw-clave
	  sw-shaker
	  sw-drum
	  sw-hiq
	  sw-fxperc
	  sw-damage
	  sw-noise
	  sw-fx) :modx)

(import '(modx:sw-kick
	  modx:sw-snare
	  modx:sw-tom
	  modx:sw-hat
	  modx:sw-cym
	  modx:sw-clap
	  modx:sw-clave
	  modx:sw-shaker
	  modx:sw-drum
	  modx:sw-hiq
	  modx:sw-fxperc
	  modx:sw-damage
	  modx:sw-noise
	  modx:sw-fx) :cyco)
