;;;; CYCO MODX Plugin arabic-mixed-kit-2
;;;;

(let ((general-key-list '((kick    . (29 "4650 Bd Techno L"))
			  (kick-2  . (33 "4651 Bd Techno Q"))
			  (kick-3 . (36 "4652 Bd NeoKick"))
			  (snare   . (31 "5441 Snare Techno"))
			  (snare-2 . (35 "5443 NeoSnare2"))
			  (snare-3 . (37 "5442 NeoSnare1"))
			  (snare-4 . (38 "5444 NeoSnare3"))
			  (snare-5 . (40 "5216 Std Snare"))
			  (snare-rim  . (34 "5218 Std Open Rimshot"))
			  (snare-stick . (30 "5440 Side Stick Analog"))
			  (clap   . (39 "2896 Hand Clap4"))
			  (snap   . (69 "2904 Finger Snap2"))
			  (snap-2 . (71 "2905 Finger Snap3"))
			  (tom   . (41 "6064 Std Tom Floor M"))
			  (tom-2 . (43 "6062 Std Tom Low L"))
			  (tom-3 . (45 "6061 Std Tom Low H"))
			  (tom-4 . (47 "6060 Std Tom Mid"))
			  (tom-5 . (48 "6060 Std Tom Mid"))
			  (tom-6 . (50 "6059 Std Tom High"))
			  (Aconga . (25 "4241 Analog Conga"))
			  (Aconga-2 . (26 "4241 Analog Conga"))
			  (Aconga-3 . (27 "4241 Analog Conga"))
			  (conga-hi . (54 "2953 Conga3 Hi Open"))
			  (conga-mute . (56 "2954 Conga3 Hi Mute"))
			  (conga-lo . (58 "2955 Conga3 Lo Open"))
			  (bongo-lo . (o63 "3006 Bongo3 Lo Open"))
			  (bongo-hi . (61 "3005 Bongo3 Hi Open"))
			  (timbale-lo  . (53 "3048 Timbale4 Lo"))
			  (timbale-hi . (55 "3048 Timbale4 Lo"))
			  (dohol-lo . (65 "3357 Kurdish Dohol1 Lo"))
			  (dohol-hi . (67 "3358 Kurdish Dohol1 Hi"))
			  (tablah-small-dom . (77 "3316 Tablah Small Dom"))
			  (tablah-small-snap . (79 "3317 Tablah Small Snap"))
			  (tablah-small-sak . (81 "3318 Tablah Small Sak"))
			  (tablah-small-tik . (83 "3319 Tablah Small Tik"))
			  (tablah-big-dom . (84 "3327 Tablah Big Dom"))
			  (tablah-big-sak . (88 "3329 Tablah Big Sak"))
			  (tablah-dom . (89 "3320 Tablah Dom"))
			  (tablah-Dom  . (89 "3320 Tablah Dom"))
			  (tablah-Tak1 . (90 "3321 Tablah Tak1"))
			  (tablah-Tik2 . (93 "3323 Tablah Tik1"))
			  (tablah-Tak3 . (92 "3322 Tablah Tak2"))
			  (tablah-Tik4 . (95 "3325 Tablah Tik3"))
			  (tablah-Tik5 . (96 "3324 Tablah Tik2"))
			  (tablah-roll . (91 "3330 Tablah Tremolo"))
			  (tablah-Sak  . (94 "3326 Tablah Sak"))
			  (tar-dom . (78 "3560 Tar Barashim Dom"))
			  (tar-sak . (80 "3561 Tar Barashim Sak"))
			  (tar-tak . (82 "3562 Tar Barashim Tak"))
			  (mewas . (86 "3395 Merwas Sak"))
			  (segal-dom . (85 "3563 Segal Dom"))
			  (degal-tak . (87 "3564 Segal Tak"))
			  (hh-x . (42 "5753 Neo HiHat Closed"))
			  (hh-op . (44 "5754 Neo HiHat HalfOpen"))
			  (hh-opn . (46 "5755 Neo HiHat Open"))
			  (hh-open . (51 "5756 Dance HiHat Open"))
			  (reverse-cymbal . (52 "6347 Reverse Cymbal2"))
			  (crash . (49 "6197 Std Crash1"))
			  (twaisat . (72 "3567 Twaisat3  (finger cymbal)"))
			  (twaisat-2 . (73 "3565 Twaisat1"))
			  (twaisat-3 . (74 "3566 Twaisat2"))
			  (twaisat-4 . (75 "3567 Twaisat3"))
			  (twaisat-5 . (76 "3568 Twaisat4"))
			  (cabasa . (66 "3072 Cabasa4"))
			  (shaker . (68 "2829 Std Shaker"))
			  (maracas . (70 "3061 Maracas2"))
			  (tambourine . (57 "2849 Tambourine3"))
			  (vibraslap . (28 "2881 Vibraslap3"))
			  (guiro . (32 "3115 Guiro2 Long"))
			  (block-lo . (64 "2872 Wood Block"))
			  (block-hi . (62 "2872 Wood Block"))
			  (clave . (60 "3219 Claves3"))
			  (cow . (59 "3228 Cowbell4"))))
      (kick-klist '((A . (29 "4650 Bd Techno L"))
		    (B . (33 "4651 Bd Techno Q"))
		    (C . (36 "4652 Bd NeoKick"))))
      (snare-klist '((x . (31 "5441 Snare Techno"))
		     (neo . (snare-"3 37 5442 NeoSnare1"))
		     (neo-2 . (35 "5443 NeoSnare2"))
		     (neo-3 . (38 "5444 NeoSnare3"))
		     (x2 . (5216 "Std Snare"))
		     (rim . (5218 "Std Open Rimshot"))
		     (stick . (5440 "Side Stick Analog"))))
      (clap-klist '((x . (39 "2896 Hand Clap4"))
		    (snap . (69 "2904 Finger Snap2"))
		    (snap-2 . (71 "2905 Finger Snap3"))))
      (tom-klist '((A . (41 "6064 Std Tom Floor M"))
		   (B . (43 "6062 Std Tom Low L"))
		   (C . (45 "6061 Std Tom Low H"))
		   (D . (47 "6060 Std Tom Mid"))
		   (E . (48 "6060 Std Tom Mid"))
		   (F . (50 "6059 Std Tom High"))))
      (analog-conga-klist '((A . (25 "4241 Analog Conga"))
			    (B . (26 "4241 Analog Conga"))
			    (C . (27 "4241 Analog Conga"))))
      (conga-klist '((lo . (58 "2955 Conga3 Lo Open"))
		     (hi . (54 "2953 Conga3 Hi Open"))
		     (mute . (56 "2954 Conga3 Hi Mute"))))
      (bongo-klist '((lo . (63 "3006 Bongo3 Lo Open"))
		     (hi . (61 "3005 Bongo3 Hi Open"))))
      (timbale-klist '((lo . (53 "3048 Timbale4 Lo"))
		       (hi . (55 "3048 Timbale4 Lo"))))
      (dohol-klist '((lo . (65 "3357 Kurdish Dohol1 Lo"))
		     (hi . (67 "3358 Kurdish Dohol1 Hi"))))
      (small-tablah-klist '((dom . (77 "3316 Tablah Small Dom"))
			    (snap . (79 "3317 Tablah Small Snap"))
			    (sak . (81 "3318 Tablah Small Sak"))
			    (tik . (83 "3319 Tablah Small Tik"))))
      (big-tablah-klist '((dom . (84 "3327 Tablah Big Dom"))
			 (sak . (88 "3329 Tablah Big Sak"))))
      (tablah-klist '((dom . (89 "3320 Tablah Dom"))
		      (tak-1 . (90 "3321 Tablah Tak1"))
		      (tak-2 . (92 "3322 Tablah Tak2"))
		      (tik-1 . (93 "3323 Tablah Tik1"))
		      (tik-2 . (96 "3324 Tablah Tik2"))
		      (tik-3 . (95 "3325 Tablah Tik3"))
		      (sak . (94 "3326 Tablah Sak"))
		      (roll . (91 "3330 Tablah Tremolo"))))
      (tar-klist '((dom . (78 "3560 Tar Barashim Dom"))
		   (sak . (80 "3561 Tar Barashim Sak"))
		   (tak . (82 "3562 Tar Barashim Tak"))))
      (merwas-klist '((sak . (86 "3395 Merwas Sak"))))
      (segal-klist '((dom . (85 "3563 Segal Dom"))
		     (tak . (87 "3564 Segal Tak"))))
      (hat-klist '((x . (42 "5753 Neo HiHat Closed"))
		   (op . (44 "5754 Neo HiHat HalfOpen"))
		   (opn . (46 "5755 Neo HiHat Open"))
		   (open . (51 "5756 Dance HiHat Open"))))
      (cymbal-klist '((crash . (49 "6197 Std Crash1"))
		      (reverse . (52 "6347 Reverse Cymbal2"))))
      (twaisat-klist '((A . (72 "3567 Twaisat3  (finger cymbal)"))
		       (B . (73 "3565 Twaisat1"))
		       (C . (74 "3566 Twaisat2"))
		       (D . (75 "3567 Twaisat3"))
		       (E . (76 "3568 Twaisat4"))))
      (shaker-klist '((A . (68 "2829 Std Shaker"))
		      (B . (66 "3072 Cabasa4"))
		      (C . (3061 "Maracas2"))
		      (tambourine . (57 "2849 Tambourine3"))
		      (vibraslap . (28 "2881 Vibraslap3"))
		      (guirao . (32 "3115 Guiro2 Long"))))
      (wood-klist '((lo . (64 "2872 Wood Block"))
		    (hi . (62 "2872 Wood Block"))
		    (clave . (60 "3219 Claves3"))))
      (cow-klist '((x . (58 "3228 Cowbell4")))) )

  (defun arabic-mixed-kit-2 (channel &key (performance *current-modx-performance*))
    (let ((amk (modx-instrument arabic-mixed-kit-2 channel
				:performance performance
				:keynumber-map (symbolic-keynumber-map general-key-list))))
      (defparameter arabic-mixed-kit-2 amk)
      (instrument amk-kick :parent amk :keynumber-map (symbolic-keynumber-map kick-klist))
      (instrument amk-snare :parent amk :keynumber-map (symbolic-keynumber-map snare-klist))
      (instrument amk-clap :parent amk :keynumber-map (symbolic-keynumber-map clap-klist))
      (instrument amk-tom :parent amk :keynumber-map (symbolic-keynumber-map tom-klist))
      (instrument amk-analog-conga :parent amk :keynumber-map (symbolic-keynumber-map analog-conga-klist))
      (instrument amk-conga :parent amk :keynumber-map (symbolic-keynumber-map conga-klist))
      (instrument amk-bongo :parent amk :keynumber-map (symbolic-keynumber-map bongo-klist))
      (instrument amk-timbale :parent amk :keynumber-map (symbolic-keynumber-map timbale-klist))
      (instrument amk-dohol :parent amk :keynumber-map (symbolic-keynumber-map dohol-klist))
      (instrument amk-small-tablah :parent amk :keynumber-map (symbolic-keynumber-map small-tablah-klist))
      (instrument amk-big-tablah :parent amk :keynumber-map (symbolic-keynumber-map big-tablah-klist))
      (instrument amk-tablah :parent amk :keynumber-map (symbolic-keynumber-map tablah-klist))
      (instrument amk-tar :parent amk :keynumber-map (symbolic-keynumber-map tar-klist))
      (instrument amk-merwas :parent amk :keynumber-map (symbolic-keynumber-map merwas-klist))
      (instrument amk-segal :parent amk :keynumber-map (symbolic-keynumber-map segal-klist))
      (instrument amk-hat :parent amk :keynumber-map (symbolic-keynumber-map hat-klist))
      (instrument amk-cymbal :parent amk :keynumber-map (symbolic-keynumber-map cymbal-klist))
      (instrument amk-twaisat :parent amk :keynumber-map (symbolic-keynumber-map twaisat-klist))
      (instrument amk-shaker :parent amk :keynumber-map (symbolic-keynumber-map shaker-klist))
      (instrument amk-wood :parent amk :keynumber-map (symbolic-keynumber-map wood-klist))
      (instrument amk-cow :parent amk :keynumber-map (symbolic-keynumber-map cow-klist))
      amk))) 
      
									
