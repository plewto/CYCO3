;;;; CYCO Yamaha MODX plugin
;;;; iranian-mix-kit

(in-package :modx)

(let* ((general-klist '((bongo-hi          . (25 "3005 Bongo3 Hi Open"))
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
			(snap              . (57 "2904 Finger Snap2"))
			(snap-clap         . (56 "2902 Iran Hand Clap"))
			(cymbal-ride       . (35 "6261 Std Ride"))
			(cymbal-crash      . (39 "6198 Std Crash2"))
			(hat               . (30 "5750 HAT Std Closed"))
			(hat-open          . (34 "5751 HH Std Open"))
			(hat-ped           . (32 "5752 HH Std Pedal"))
			(daf-tom           . (48 "3791 Daf Tom bb"))
			(daf-back          . (50 "3796 Daf Back2"))
			(daf-chap          . (52 "3794 Daf Chap"))
			(daf-roll          . (53 "3805 Daf Full Roll"))
			(daf-chain         . (49 "3799 Daf Chain Up2"))
			(daf-chain2        . (51 "3800 Daf Chain Down2"))
			(daf-whip-roll     . (55 "3807 Daf Whipping Roll"))
			(daf-chain-roll    . (54 "3806 Daf Chain Roll"))
			(daholla-dom       . (45 "4036 Daholla2 Dom"))
			(daholla-sak       . (44 "4039 Daholla2 Sak"))
			(daholla-tak       . (47 "4037 Daholla2 Tak 1"))
			(daholla-tak2      . (46 "4038 Daholla2 Tak 2"))
			(dayereh-tom       . (86 "3819 Dayereh2 Tom"))
			(dayereh-back      . (88 "3821 Dayereh2 Back"))
			(dayereh-snap      . (89 "3822 Dayereh2 Snap"))
			(dayereh-eshareh   . (87 "3820 Dayereh2 Eshareh"))
			(dayereh-roll      . (85 "3818 Dayereh1 Roll"))
			(dayereh-roll2     . (90 "3825 Dayereh2 Edge Roll"))
			(dohol-lo1        . (64 "3359 Kurdish Dohol2 Lo"))
			(dohol-hi1        . (65 "3360 Kurdish Dohol2 Hi"))
			(dohol-buzz1      . (66 "3361 Kurdish Dohol2LoBuzz"))
			(dohol-lo2        . (67 "3362 Lurish Dohol Lo"))
			(dohol-hi2        . (69 "3364 Lurish Dohol Hi"))
			(dohol-buzz2      . (68 "3363 Lurish Dohol Lo Buzz"))
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
			(tombak-eshareh    . (73 "3873 Tombak Eshareh Right"))
			(tombak-eshareh2   . (75 "3872 Tombak Eshareh Left"))
			(tombak-roll       . (70 "3874 Tombak Full Roll"))
			(tombak-roll2      . (71 "3875 Tombak Timapni Roll"))
			(twaisat-a         . (91 "3568 Twaisat4"))
			(twaisat-b         . (92 "3566 Twaisat2"))
			(twaisat-c         . (93 "3569 Twaisat5"))
			(twaisat-d         . (94 "3565 Twaisat1"))
			(zarb-tom          . (77 "4016 Zarb2 Tom"))
			(zarb-back         . (79 "4019 Zarb2 Back"))
			(zarb-chap         . (81 "4018 Zarb2 Chap"))
			(zarb-snap         . (83 "4021 Zarb2 Snap"))
			(zarb-whip         . (78 "4020 Zarb2 Whipping"))
			(zarb-eshareh      . (80 "4017 Zarb2 Eshareh"))
			(zarb-roll         . (82 "4022 Zarb2 Full Roll"))
			(zarb-whip-roll    . (84 "4023 Zarb2 Whipping Roll"))))
      (bongo-klist (extract-sub-symbolic-keylist 'bongo general-klist))
      (conga-klist (extract-sub-symbolic-keylist 'conga general-klist))
      (timbale-klist (extract-sub-symbolic-keylist 'timbale general-klist))
      (tom-klist (extract-sub-symbolic-keylist 'tom general-klist))
      (snare-klist (extract-sub-symbolic-keylist 'snare general-klist))
      (kick-klist (extract-sub-symbolic-keylist 'kick general-klist))
      (snap-klist (extract-sub-symbolic-keylist 'snap general-klist))
      (cymbal-klist (extract-sub-symbolic-keylist 'cymbal general-klist))
      (hat-klist (extract-sub-symbolic-keylist 'hat general-klist))
      (daf-klist (extract-sub-symbolic-keylist 'daf general-klist))
      (daholla-klist (extract-sub-symbolic-keylist 'daholla general-klist))
      (dayereh-klist (extract-sub-symbolic-keylist 'dayereh general-klist))
      (dohol-klist (extract-sub-symbolic-keylist 'dohol general-klist))
      (neghareh-klist (extract-sub-symbolic-keylist 'neghareh general-klist))
      (tablah-klist (extract-sub-symbolic-keylist 'tablah general-klist))
      (tombak-klist (extract-sub-symbolic-keylist 'tombak general-klist))
      (twaisat-klist (extract-sub-symbolic-keylist 'twaisat general-klist))
      (zarb-klist (extract-sub-symbolic-keylist 'zarb general-klist))

      (imk (make-main-instrument iranian-mix-kit general-klist)))

  (make-sub imk-bongo imk bongo-klist)
  (make-sub imk-conga imk conga-klist)
  (make-sub imk-timbale imk timbale-klist)
  (make-sub imk-tom imk tom-klist)
  (make-sub imk-snare imk snare-klist)
  (make-sub imk-kick imk kick-klist)
  (make-sub imk-snap imk snap-klist)
  (make-sub imk-cymbal imk cymbal-klist)
  (make-sub imk-hat imk hat-klist)
  (make-sub imk-daf imk daf-klist)
  (make-sub imk-daholla imk daholla-klist)
  (make-sub imk-dayereh imk dayereh-klist)
  (make-sub imk-dohol imk dohol-klist)
  (make-sub imk-neghareh imk neghareh-klist)
  (make-sub imk-tablah imk tablah-klist)
  (make-sub imk-tombak imk tombak-klist)
  (make-sub imk-twaisat imk twaisat-klist)
  (make-sub imk-zarb imk zarb-klist))

(export '(imk-bongo 
	  imk-conga 
	  imk-timbale 
	  imk-tom 
	  imk-snare 
	  imk-kick 
	  imk-snap 
	  imk-cymbal 
	  imk-hat 
	  imk-daf 
	  imk-daholla 
	  imk-dayereh 
	  imk-dohol 
	  imk-neghareh 
	  imk-tablah 
	  imk-tombak 
	  imk-twaisat 
	  imk-zarb) :modx)

(import '(modx:imk-bongo 
	  modx:imk-conga 
	  modx:imk-timbale 
	  modx:imk-tom 
	  modx:imk-snare 
	  modx:imk-kick 
	  modx:imk-snap 
	  modx:imk-cymbal 
	  modx:imk-hat 
	  modx:imk-daf 
	  modx:imk-daholla 
	  modx:imk-dayereh 
	  modx:imk-dohol 
	  modx:imk-neghareh 
	  modx:imk-tablah 
	  modx:imk-tombak 
	  modx:imk-twaisat 
	  modx:imk-zarb ) :cyco)
