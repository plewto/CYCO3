;;;; CYCO Yamaha MODX plugin
;;;; iranian-mix-kit

(in-package :cyco)

(let ((kmap (symbolic-keynumber-map
	     '((bongo-hi          . (25 "3005 Bongo3 Hi Open"))
	       (bongo-lo          . (26 "3006 Bongo3 Lo Open"))
	       (conga-hi          . (27 "2953 Conga3 Hi Open"))
	       (conga-lo          . (28 "2955 Conga3 Lo Open"))
	       (timbale-1         . (95 "3048 Timbale4 Lo"))
	       (timbale-2         . (96 "3048 Timbale4 Lo"))
	       (tom-a             . (29 "6065 Std Tom Floor L"))
	       (tom-b             . (31 "6063 Std Tom Floor H"))
	       (tom-c             . (33 "6063 Std Tom Floor H"))
	       (snare             . (38 "5216 Std Snare"))
	       (snare-tight       . (40 "5217 Std Snare Tight"))
	       (snare-rim         . (37 "5203 Sd Closed Rim4"))
	       (kick              . (36 "4550 Bd Std Kick"))
	       (clap              . (56 "2902 Iran Hand Clap"))
	       (finger            . (57 "2904 Finger Snap2"))
	       (cym-ride          . (35 "6261 Std Ride"))
	       (cym-crash         . (39 "6198 Std Crash2"))
	       (hh                . (30 "5750 HH Std Closed"))
	       (hh-open           . (34 "5751 HH Std Open"))
	       (hh-ped            . (32 "5752 HH Std Pedal"))
	       (daf-tom           . (48 "3791 Daf Tom bb"))
	       (daf-back          . (50 "3796 Daf Back2"))
	       (daf-chap          . (52 "3794 Daf Chap"))
	       (daf-roll          . (53 "3805 Daf Full Roll"))
	       (daf-whip-roll     . (55 "3807 Daf Whipping Roll"))
	       (daf-chain-roll    . (54 "3806 Daf Chain Roll"))
	       (daf-chain         . (49 "3799 Daf Chain Up2"))
	       (daf-chain-2       . (51 "3800 Daf Chain Down2"))
	       (daholla-dom       . (45 "4036 Daholla2 Dom"))
	       (daholla-sak       . (44 "4039 Daholla2 Sak"))
	       (daholla-tak-2     . (46 "4038 Daholla2 Tak 2"))
	       (daholla-tak       . (47 "4037 Daholla2 Tak 1"))
	       (dayereh-tom       . (86 "3819 Dayereh2 Tom"))
	       (dayereh-back      . (88 "3821 Dayereh2 Back"))
	       (dayereh-snap      . (89 "3822 Dayereh2 Snap"))
	       (dayereh-eshareh   . (87 "3820 Dayereh2 Eshareh"))
	       (dayereh-roll      . (85 "3818 Dayereh1 Roll"))
	       (dayereh-roll-2    . (90 "3825 Dayereh2 Edge Roll"))
	       (dohol-1-lo        . (64 "3359 Kurdish Dohol2 Lo"))
	       (dohol-1-hi        . (65 "3360 Kurdish Dohol2 Hi"))
	       (dohol-1-buzz      . (66 "3361 Kurdish Dohol2LoBuzz"))
	       (dohol-2-lo        . (67 "3362 Lurish Dohol Lo"))
	       (dohol-2-hi        . (69 "3364 Lurish Dohol Hi"))
	       (dohol-2-buzz      . (68 "3363 Lurish Dohol Lo Buzz"))
	       (neghareh-tom      . (59 "3458 Neghareh Tom"))
	       (neghareh-chap     . (61 "3459 Neghareh Chap"))
	       (neghareh-snap     . (60 "3461 Neghareh Snap"))
	       (neghareh-back     . (63 "3460 Neghareh Back"))
	       (neghareh-eshareh  . (62 "3462 Neghareh Eshareh"))
	       (neghareh-roll     . (58 "3463 Neghareh Roll"))
	       (tablah-dom        . (41 "3327 Tablah Big Dom"))
	       (tablah-sak        . (42 "3329 Tablah Big Sak"))
	       (tablah-tak        . (43 "3328 Tablah Big Tak"))
	       (tombak-tom        . (72 "3869 Tombak Tom2"))
	       (tombak-back       . (74 "3870 Tombak Back"))
	       (tombak-snap       . (76 "3871 Tombak Snap"))
	       (tombak-eshareh-1  . (73 "3873 Tombak Eshareh Right"))
	       (tombak-eshareh-2  . (75 "3872 Tombak Eshareh Left"))
	       (tombak-roll       . (70 "3874 Tombak Full Roll"))
	       (tombak-roll       . (71 "3875 Tombak Timapni Roll"))
	       (twaisat-a         . (91 "3568 Twaisat4"))
	       (twaisat-c         . (93 "3569 Twaisat5"))
	       (twaisat-d         . (94 "3565 Twaisat1"))
	       (twaisat-b         . (92 "3566 Twaisat2"))
	       (zarb-tom          . (77 "4016 Zarb2 Tom"))
	       (zarb-back         . (79 "4019 Zarb2 Back"))
	       (zarb-chap         . (81 "4018 Zarb2 Chap"))
	       (zarb-snap         . (83 "4021 Zarb2 Snap"))
	       (zarb-whip         . (78 "4020 Zarb2 Whipping"))
	       (zarb-eshareh      . (80 "4017 Zarb2 Eshareh"))
	       (zarb-roll         . (82 "4022 Zarb2 Full Roll"))
	       (zarb-whip-roll    . (84 "4023 Zarb2 Whipping Roll")) )))
	       

      (mixed-kmap (symbolic-keynumber-map
		    '((bongo-hi          . (25))
		      (bongo-lo          . (26))
		      (conga-hi          . (27))
		      (conga-lo          . (28))
		      (timbale-1         . (95))
		      (timbale-2         . (96))
		      (tom-a             . (29))
		      (tom-b             . (31))
		      (tom-c             . (33))
		      (snare             . (38))
		      (snare-tight       . (40))
		      (snare-rim         . (37))
		      (kick              . (36))
		      (clap              . (56))
		      (finger            . (57)))))

      (cymbals-kmap (symbolic-keynumber-map
		     '((ride          . (35))
		       (crash         . (39))
		       (hat           . (30))
		       (open          . (34))
		       (pedal         . (32)))))
      
      (daf-kmap (symbolic-keynumber-map
		 '((tom           . (48))
		   (back          . (50))
		   (chap          . (52))
		   (roll          . (53))
		   (whip-roll     . (55))
		   (chain-roll    . (54))
		   (chain         . (49))
		   (chain-2       . (51)))))
		   
      (daholla-kmap (symbolic-keynumber-map
		     '((dom       . (45))
		       (sak       . (44))
		       (tak       . (46))
		       (tak-2     . (47)))))

      (dayereh-kmap (symbolic-keynumber-map
		     '((back      . (86))
		       (eshareh   . (88))
		       (roll      . (89))
		       (roll-2    . (87))
		       (snap      . (85))
		       (tom       . (90)))))

      (dohol-kmap (symbolic-keynumber-map
		   '((buzz-1      . (64))
		     (hi-1        . (65))
		     (lo-1        . (66))
		     (buzz-2      . (67))
		     (hi-2        . (69))
		     (lo-2        . (68)))))

      (neghareh-kmap (symbolic-keynumber-map
		      '((back     . (59))
			(chap     . (61))
			(eshareh  . (60))
			(roll     . (63))
			(snap     . (62))
			(tom      . (58)))))

      (tablah-kmap (symbolic-keynumber-map
		    '((dom        . (41))
		      (sak        . (42))
		      (tak        . (43)))))

      (tombak-kmap (symbolic-keynumber-map
		    '((back       . (72))
		      (eshareh-1  . (74))
		      (eshareh-2  . (76))
		      (roll       . (73))
		      (roll-2     . (77))
		      (snap       . (70))
		      (tom        . (71)))))

      (twaisat-kmap (symbolic-keynumber-map
		     '((a . (91))
		       (b . (93))
		       (c . (94))
		       (d . 92))))

      (zarb-kmap (symbolic-keynumber-map
		  '((back         . (77))
		    (chap         . (79))
		    (eshareh      . (81))
		    (roll         . (83))
		    (snap         . (78))
		    (tom          . (80))
		    (whip         . (82))
		    (whip-roll    . (84))))) )

  (defun iranian-mix-kit (channel &key (performance *current-modx-performance*))
    (let ((inst (modx-instrument iranian-mix-kit channel
				:performance performance
				:keynumber-map kmap)))
      (defparameter iran-drums (make-instrument 'iran-drums
      						:parent inst
      						:keynumber-map mixed-kmap))
      (defparameter iran-cymbals (make-instrument 'iran-cymbals
      						  :parent inst
      						  :keynumber-map cymbals-kmap))
      (defparameter iran-daf (make-instrument 'iran-daf
      					      :parent inst
      					      :keynumber-map daf-kmap))
      (defparameter iran-daholla (make-instrument 'iran-daholla
      						  :parent inst
      						  :keynumber-map daholla-kmap))
      (defparameter iran-dayereh (make-instrument 'iran-dayereh
      						  :parent inst
      						  :keynumber-map dayereh-kmap))
      (defparameter iran-dohol (make-instrument 'iran-dohol
      						:parent inst
      						:keynumber-map dohol-kmap))
      (defparameter iran-neghareh (make-instrument 'iran-neghareh
      						   :parent inst
      						   :keynumber-map neghareh-kmap))
      (defparameter iran-tablah (make-instrument 'iran-tablah
      						 :parent inst
      						 :keynumber-map tablah-kmap))
      (defparameter iran-tombak (make-instrument 'iran-tombak
      						  :parent inst
      						  :keynumber-map tombak-kmap))
      (defparameter iran-twaisat (make-instrument 'iran-twaisat
      						  :parent inst
      						  :keynumber-map twaisat-kmap))
      (defparameter iran-zarb (make-instrument 'iran-zarb
      					       :parent inst
      					       :keynumber-map zarb-kmap))
      inst))) 

      
  

