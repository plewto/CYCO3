;;;; CYCO modx-drums plugin  arabic-mixed-kit-2
;;;;

(in-package :modx)

(let* ((general-klist '((KICK   . (29 "4650 Bd Techno L"))
			(KICK-B . (33 "4651 Bd Techno Q"))
			(KICK-C . (36 "4652 Bd NeoKick"))
			
			(SNARE . (40 "5216 Std Snare"))
			(SNARE-TECHNO . (31 "5441 Snare Techno"))
			(SNARE-NEO  . (37 "5442 NeoSnare1"))
			(SNARE-NEO2 . (35 "5443 NeoSnare2"))
			(SNARE-NEO3 . (38 "5444 NeoSnare3"))
			(SNARE-RIM  . (34 "5218 Std Open Rimshot"))
			(SNARE-STICK . (30 "5440 Side Stick Analog"))
			
			(SNAP-A    . (69 "2904 Finger Snap2"))
			(SNAP-B    . (71 "2905 Finger Snap3"))
			(SNAP-CLAP . (39 "2896 Hand Clap4"))
			
			(TOM-A . (41 "6064 Std Tom Floor M"))
			(TOM-B . (43 "6062 Std Tom Low L"))
			(TOM-C . (45 "6061 Std Tom Low H"))
			(TOM-D . (47 "6060 Std Tom Mid"))
			(TOM-E . (48 "6060 Std Tom Mid"))
			(TOM-F . (50 "6059 Std Tom High"))
			
			(CONGA-LO   . (58 "2955 Conga3 Lo Open"))
			(CONGA-MUTE . (56 "2954 Conga3 Hi Mute"))
			(CONGA-HI   . (54 "2953 Conga3 Hi Open"))
			(CONGA-A1   . (25 "4241 Analog Conga"))
			(CONGA-A2   . (26 "4241 Analog Conga"))
			(CONGA-A3   . (27 "4241 Analog Conga"))
			
			(BONGO-LO . (63 "3006 Bongo3 Lo Open"))
			(BONGO-HI . (61 "3005 Bongo3 Hi Open"))
			
			(TIMBALE-LO . (53 "3048 Timbale4 Lo"))
			(TIMBALE-HI . (55 "3048 Timbale4 Lo"))
			
			(DOHOL-LO . (65 "3357 Kurdish Dohol1 Lo"))
			(DOHOL-HI . (67 "3358 Kurdish Dohol1 Hi"))
			
			(TABLAH-SMALL-DOM  . (77 "3316 Tablah Small Dom"))
			(TABLAH-SMALL-SAK  . (81 "3318 Tablah Small Sak"))
			(TABLAH-SMALL-SNAP . (79 "3317 Tablah Small Snap"))
			(TABLAH-SMALL-TIK  . (83 "3319 Tablah Small Tik"))
			(TABLAH-BIG-DOM . (84 "3327 Tablah Big Dom"))
			(TABLAH-BIG-SAK . (88 "3329 Tablah Big Sak"))
			
			(TABLAH2-DOM   . (89 "3320 Tablah Dom"))
			(TABLAH2-SAK   . (94 "3326 Tablah Sak"))
			(TABLAH2-TAK   . (90 "3321 Tablah Tak1"))
			(TABLAH2-TIK   . (93 "3323 Tablah Tik1"))
			(TABLAH2-DOM2  . (89 "3320 Tablah Dom"))
			(TABLAH2-TAK2  . (92 "3322 Tablah Tak2"))
			(TABLAH2-TIK2  . (95 "3325 Tablah Tik3"))
			(TABLAH2-TIK3  . (96 "3324 Tablah Tik2"))
			(TABLAH2-ROLL  . (91 "3330 Tablah Tremolo"))
			
			(TAR-DOM   . (78 "3560 Tar Barashim Dom"))
			(TAR-SAK   . (80 "3561 Tar Barashim Sak"))
			(TAR-TAK   . (82 "3562 Tar Barashim Tak"))
			
			(MEWAS     . (86 "3395 Merwas Sak"))
			
			(SEGAL-DOM . (85 "3563 Segal Dom"))
			(SEGAL-TAK . (87 "3564 Segal Tak"))
			
			(HAT      . (42 "5753 Neo HiHat Closed"))
			(HAT-OP   . (44 "5754 Neo HiHat HalfOpen"))
			(HAT-OPN  . (46 "5755 Neo HiHat Open"))
			(HAT-OPEN . (51 "5756 Dance HiHat Open"))
			
			(CYMBAL-CRASH   . (49 "6197 Std Crash1"))
			(CYMBAL-REVERSE . (52 "6347 Reverse Cymbal2"))
			
			(TWAISAT-A . (72 "3567 Twaisat3  (finger cymbal)"))
			(TWAISAT-B . (73 "3565 Twaisat1"))
			(TWAISAT-C . (74 "3566 Twaisat2"))
			(TWAISAT-D . (75 "3567 Twaisat3"))
			(TWAISAT-E . (76 "3568 Twaisat4"))
			
			(SHAKER            . (68 "2829 Std Shaker"))
			(SHAKER-CABASA     . (66 "3072 Cabasa4"))
			(SHAKER-MARACAS    . (70 "3061 Maracas2"))
			(SHAKER-TAMBOURINE . (57 "2849 Tambourine3"))
			(SHAKER-VIBRASLAP  . (28 "2881 Vibraslap3"))
			(SHAKER-GUIRO      . (32 "3115 Guiro2 Long"))
			
			(BLOCK-LO    . (64 "2872 Wood Block"))
			(BLOCK-HI    . (62 "2872 Wood Block"))
			(BLOCK-CLAVE . (60 "3219 Claves3"))
			(BLOCK-COW   . (59 "3228 Cowbell4"))))
       
       (kick-klist (extract-sub-symbolic-keylist 'kick general-klist))
       (snare-klist (extract-sub-symbolic-keylist 'snare general-klist))
       (snap-klist (extract-sub-symbolic-keylist 'snap general-klist))
       (tom-klist (extract-sub-symbolic-keylist 'tom general-klist))
       (conga-klist (extract-sub-symbolic-keylist 'conga general-klist))
       (bongo-klist (extract-sub-symbolic-keylist 'bongo general-klist))
       (timbale-klist (extract-sub-symbolic-keylist 'timbale general-klist))
       (tablah-klist (extract-sub-symbolic-keylist 'tablah general-klist))
       (tablah2-klist (extract-sub-symbolic-keylist 'tablah2 general-klist))
       (tar-klist (extract-sub-symbolic-keylist 'tar general-klist))
       (mewas-klist (extract-sub-symbolic-keylist 'mewas general-klist))
       (segal-klist (extract-sub-symbolic-keylist 'segal general-klist))
       (hat-klist (extract-sub-symbolic-keylist 'hat general-klist))
       (cymbal-klist (extract-sub-symbolic-keylist 'cymbal general-klist))
       (twaisat-klist (extract-sub-symbolic-keylist 'twaisat general-klist))
       (shaker-klist (extract-sub-symbolic-keylist 'shaker general-klist))
       (block-klist (extract-sub-symbolic-keylist 'block general-klist))
       (amk2 (make-main-instrument arabic-mixed-kit-2 general-klist)))

  (make-sub amk2-kick amk2 kick-klist)
  (make-sub amk2-snare amk2 snare-klist)
  (make-sub amk2-snap amk2 snap-klist)
  (make-sub amk2-tom amk2 tom-klist)
  (make-sub amk2-conga amk2 conga-klist)
  (make-sub amk2-bongo amk2 bongo-klist)
  (make-sub amk2-timbale amk2 timbale-klist)
  (make-sub amk2-tablah amk2 tablah-klist)
  (make-sub amk2-tablah2 amk2 tablah2-klist)
  (make-sub amk2-tar amk2 tar-klist)
  (make-sub amk2-mewas amk2 mewas-klist)
  (make-sub amk2-segal amk2 segal-klist)
  (make-sub amk2-hat amk2 hat-klist)
  (make-sub amk2-cymbal amk2 cymbal-klist)
  (make-sub amk2-twaisat amk2 twaisat-klist)
  (make-sub amk2-shaker amk2 shaker-klist)
  (make-sub amk2-block amk2 block-klist)) 
 
(export '(amk2-kick
	  amk2-snare
	  amk2-snap
	  amk2-tom
	  amk2-conga
	  amk2-bongo
	  amk2-timbale
	  amk2-tablah
	  amk2-tablah2
	  amk2-tar
	  amk2-mewas
	  amk2-segal
	  amk2-hat
	  amk2-cymbal
	  amk2-twaisat
	  amk2-shaker
	  amk2-block) :modx)

(import '(modx:amk2-kick
	  modx:amk2-snare
	  modx:amk2-snap
	  modx:amk2-tom
	  modx:amk2-conga
	  modx:amk2-bongo
	  modx:amk2-timbale
	  modx:amk2-tablah
	  modx:amk2-tablah2
	  modx:amk2-tar
	  modx:amk2-mewas
	  modx:amk2-segal
	  modx:amk2-hat
	  modx:amk2-cymbal
	  modx:amk2-twaisat
	  modx:amk2-shaker
	  modx:amk2-block) :cyco)
  


      
     
