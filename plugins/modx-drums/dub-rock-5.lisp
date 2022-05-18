;;;; CYCO modx-drums DUB-ROCK-5
;;;;
;;;; Proxy for drum kit on channel 5 for MODX factory performance "Dub Rock Bass"
;;;;

(in-package :modx)

(let* ((general-klist '((KICK-A     . (36 "4662 Bd Stack3"))
			(KICK-B     . (33 "4685 Bd Body2"))
			(KICK-C     . (35 "4682 Bd LongBass"))
			(KICK-GATE  . (84 "4558 Bd DanceGate1"))
			(KICK-EDM   . (85 "4681 Bd EDM Gate"))
			(KICK-GATE2 . (86 "4559 Bd DanceGate2"))
			(KICK-STACK . (88 "4662 Bd Stack3"))
			(KICK-EDM2  . (89 "4680 Bd EDM10"))
			
			(SNARE-A     . (34 "5451 Sd ShortDry2"))
			(SNARE-B     . (24 "5457 Sd TB Comp1"))
			(SNARE-C     . (37 "5452 Sd ShortGate1"))
			(SNARE-D     . (38 "5456 Sd TB Q"))
			(SNARE-E     . (40 "5445 Sd Clap1"))
			(SNARE-F     . (42 "5504 Sd HipHop16"))
			(SNARE-G     . (79 "5431 Sd FM"))
			(SNARE-H     . (90 "5178 Sd BreakRim"))
			
			(TOM-A . (41 "6138 Comp EleTom Low"))
			(TOM-B . (43 "6138 Comp EleTom Low"))
			(TOM-C . (45 "6138 Comp EleTom Low"))
			(TOM-D . (47 "6137 Comp EleTom High"))
			(TOM-E . (48 "6137 Comp EleTom High"))
			(TOM-F . (50 "6137 Comp EleTom High"))
			
			(DRUM-QUINTO   . (64 "2967 Quinto1 Slap"))
			(DRUM-DJEMBE   . (66 "3272 Djembe1 Edge"))
			(DRUM-TABLA    . (67 "3289 Tabla1 Nah"))
			(DRUM-TABLA2   . (65 "3288 Tabla1 Bya"))
			(DRUM-BARASIM  . (68 "3545 Barasim Tak"))
			
			(HAT-X    . (44 "5822 HH TecClosed2"))
			(HAT-OP   . (46 "5823 HH TecOpen2"))
			(HAT-X2   . (78 "5780 HH Closed T8-1"))
			(HAT-OPEN . (80 "5570 HH Half Open2 RL"))
			(HAT-OPEN . (82 "5781 HH Open T8-1"))
			(HAT-X3   . (31 "5784 T8 HHCl"))
			
			(CYM-RIDE-EDGE . (51 "6262 Fine Ride Bow"))
			(CYM-RIDE-CUP  . (53 "6264 Fine Ride Cup"))
			(CYM-RIDE-EDGE . (59 "6263 Fine Ride Edge"))
			(CYM-CRASH     . (49 "6199 PI Crash 15"))
			(CYM-CRASH2    . (55 "6200 PI Crash 16"))
			(CYM-CRASH3    . (57 "6200 PI Crash 16"))
			(CYM-CHINA     . (52 "6292 China4"))
			(CYM-REVERSE   . (83 "6346 Reverse Cymbal1"))
			
			(ABSTRACT-A . (26 "2531 Abstract Fx1"))
			(ABSTRACT-B . (27 "2534 Abstract Fx4"))
			(ABSTRACT-C . (75 "2533 Abstract Fx3"))
			(ABSTRACT-D . (76 "2535 Abstract Fx5"))
			
			(DAMAGE-A . (28 "2537 Damage Fx1"))
			(DAMAGE-B . (30 "2539 Damage Fx3"))
			(DAMAGE-C . (87 "2547 Damage Fx11"))
			(DAMAGE-D . (93 "2544 Damage Fx8"))
			
			(EPERC-A .(32 "2563 Electric Fx Perc8"))
			(EPERC-B .(91 "2570 Electric Fx Perc15"))
			(EPERC-C .(94 "2561 Electric Fx Perc6"))
			(EPERC-D .(95 "2559 Electric Fx Perc4"))
			(EPERC-E .(96 "2564 Electric Fx Perc9"))
			
			(NOISE-A . (72 "2550 Noize Fx1"))
			(NOISE-B . (92 "2551 Noize Fx2"))
			(NOISE-C . (73 "2552 Noize Rize1"))
			(NOISE-D . (74 "2553 Noize Rize2"))
			(NOISE-E . (29 "2536 Bazer Fx"))
			(NOISE-F . (60 "2790 Acoustic Bass FX07 (velocity)"))
			(NOISE-G . (71 "2392 Hop FX1"))
			
			(CLAP-RX . (39 "4190 RX11 Clap"))
			(CLAP-E  . (69 "4188 Electric Clap1"))
			
			(HIQ-A . (56 "2513 Hi Q2"))
			(HIQ-B . (58 "2513 Hi Q2"))
			(HIQ-C . (62 " 276 Mini Brip1"))
			(HIQ-D . (63 "2277 Mini Brip2"))
			(HIQ-E . (61 "2281 Mini Brip6"))
			
			(CLAVE-A . (81 "4276 Claves T8-1"))
			(CLAVE-B . (54 "3088 Pandeiro1 Tip"))
			(CLAVE-C . (77 "4286 Analog Click"))))

       (kick-klist (extract-sub-symbolic-keylist 'kick general-klist))
       (snare-klist (extract-sub-symbolic-keylist 'snare general-klist))
       (tom-klist (extract-sub-symbolic-keylist 'tom general-klist))
       (drum-klist (extract-sub-symbolic-keylist 'drum general-klist))
       (hat-klist (extract-sub-symbolic-keylist 'hat general-klist))
       (cym-klist (extract-sub-symbolic-keylist 'cym general-klist))
       (abstract-klist (extract-sub-symbolic-keylist 'abstract general-klist))
       (damage-klist (extract-sub-symbolic-keylist 'damage general-klist))
       (eperc-klist (extract-sub-symbolic-keylist 'eperc general-klist))
       (noise-klist (extract-sub-symbolic-keylist 'noise general-klist))
       (clap-klist (extract-sub-symbolic-keylist 'clap general-klist))
       (hiq-klist (extract-sub-symbolic-keylist 'hiq general-klist))
       (clave-klist (extract-sub-symbolic-keylist 'clave general-klist))
       (dr5 (make-main-instrument dub-rock-5 general-klist)) )
  (make-sub dr5-kick dr5 kick-klist)
  (make-sub dr5-snare dr5 snare-klist)
  (make-sub dr5-tom dr5 tom-klist)
  (make-sub dr5-drum dr5 drum-klist)
  (make-sub dr5-hat dr5 hat-klist)
  (make-sub dr5-cym dr5 cym-klist)
  (make-sub dr5-abstract dr5 abstract-klist)
  (make-sub dr5-damage dr5 damage-klist)
  (make-sub dr5-eperc dr5 eperc-klist)
  (make-sub dr5-noise dr5 noise-klist)
  (make-sub dr5-clap dr5 clap-klist)
  (make-sub dr5-hiq dr5 hiq-klist)
  (make-sub dr5-clave dr5 clave-klist) )

(export '(dr5-kick
	  dr5-snare
	  dr5-tom
	  dr5-drum
	  dr5-hat
	  dr5-cym
	  dr5-abstract
	  dr5-damage
	  dr5-eperc
	  dr5-noise
	  dr5-clap
	  dr5-hiq
	  dr5-clave) :modx)

(import '(modx:dr5-kick
	  modx:dr5-snare
	  modx:dr5-tom
	  modx:dr5-drum
	  modx:dr5-hat
	  modx:dr5-cym
	  modx:dr5-abstract
	  modx:dr5-damage
	  modx:dr5-eperc
	  modx:dr5-noise
	  modx:dr5-clap
	  modx:dr5-hiq
	  modx:dr5-clave) :cyco)

