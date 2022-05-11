;;;; CYCO Yamaha MODX plugin
;;;; new-iranian-kit-2
;;;;
;;;; key  wave
;;;; [36] 3020 Iran Bongo Lo Roll
;;;; [37] 3025 Iran Bongo Hi Roll
;;;; [38] 3437 Zangooreh Roll
;;;; [39] 3438 Zangooreh Shot
;;;; [40] 2864 Castanet3 Flam1
;;;; [41] 2865 Castanet3 Flam2
;;;; [42] 2866 Castanet3 LH Normal
;;;; [43] 2867 Castanet3 LH Mute
;;;; [44] 2868 Castanet3 LH&RH Nrml
;;;; [45] 2869 Castanet3 LH&RH Mute
;;;; [46] 2870 Castanet3 RH Normal
;;;; [47] 2871 Castanet3 RH Mute
;;;; [48] 3464 Gosha Naghareh Lo
;;;; [49] 3465 Gosha Naghareh LoRim
;;;; [50] 3466 Gosha Naghareh LoFlm
;;;; [51] 3467 Gosha Naghareh Hi1
;;;; [52] 3468 Gosha Naghareh Hi2
;;;; [53] 3469 Gosha Naghareh HiRim
;;;; [54] 3470 Gosha Naghareh HiFlm
;;;; [55] 3432 Finger Cymbal Opn LH
;;;; [56] 3433 Finger Cymbal Mt LH
;;;; [57] 3434 Finger Cymbal Opn RH
;;;; [58] 3435 Finger Cymbal Mt RH
;;;; [59] 3436 Finger Cymbal Mt L&R
;;;; [60] 3700 Iran Bendir2 Tom
;;;; [61] 3702 Iran Bendir2 Chap1
;;;; [62] 3703 Iran Bendir2 Chap2
;;;; [63] 3705 Iran Bendir2 Back
;;;; [64] 3701 Iran Bendir2 Slap
;;;; [65] 3706 Iran Bendir2 Bak Rim
;;;; [66] 3704 Iran Bendir2 Chap Mt
;;;; [67] 3709 Iran Bendir2 FlamBck
;;;; [68] 3707 Iran Bendir2 Flam
;;;; [69] 3708 Iran Bendir2 FlamTom
;;;; [70] 3711 Iran Bendir2 RimRoll
;;;; [71] 3710 Iran Bendir2 Roll
;;;; [72] 3898 Tempo Chap Tom
;;;; [73] 3899 Tempo Chap1
;;;; [74] 3896 Tempo Tom Mute
;;;; [75] 3897 Tempo Tom Slap
;;;; [76] 3900 Tempo Chap2
;;;; [77] 3895 Tempo Tom
;;;; [78] 3903 Tempo Back1
;;;; [79] 3904 Tempo Back2
;;;; [80] 3905 Tempo Back Rim
;;;; [81] 3909 Tempo Flam Bak
;;;; [82] 3907 Tempo Flam
;;;; [83] 3910 Tempo Roll
;;;; [84] 3908 Tempo Flam Tom
;;;; [85] 3901 Tempo Chap Rim
;;;; [86] 3902 Tempo Chap Rim Mute
;;;; [87] 3906 Tempo Snap Nail
;;;; [88] 3913 Tempo Gliss Down
;;;; [89] 3912 Tempo Gliss Up
;;;; [90] 3911 Tempo Rim Roll
;;;; [91] 3018 Iran Bongo Lo Tom
;;;; [92] 3021 Iran Bongo Hi Bk LH
;;;; [93] 3022 Iran Bongo Hi Bk RH
;;;; [94] 3023 Iran Bongo Hi Mt RH
;;;; [95] 3019 Iran Bongo Lo Flm Tm
;;;; [96] 3024 Iran Bongo Hi Flm Bk
;;;; 

(in-package :modx)

(let* ((general-klist '((BONGO-LO-TOM             . (91 "3018 Iran Bongo Lo Tom"))
			(BONGO-LO-ROLL            . (36 "3020 Iran Bongo Lo Roll"))
			(BONGO-LO-FLAM            . (95 "3019 Iran Bongo Lo Flm Tm"))
			(BONGO-HI-LH              . (92 "3021 Iran Bongo Hi Bk LH"))
			(BONGO-HI-RH              . (93 "3022 Iran Bongo Hi Bk RH"))
			(BONGO-HI-MUTE            . (94 "3023 Iran Bongo Hi Mt RH"))
			(BONGO-HI-FLAM            . (96 "3024 Iran Bongo Hi Flm Bk"))
			(BONGO-HI-ROLL            . (37 "3025 Iran Bongo Hi Roll"))
			
			(ZANGOOREH-SHOT           . (39 "3438 Zangooreh Shot"))
			(ZANGOOREH-ROLL           . (38 "3437 Zangooreh Roll"))
			
			(CASTANET-LH              . (42 "2866 Castanet3 LH Normal"))
			(CASTANET-RH              . (46 "2870 Castanet3 RH Normal"))
			(CASTANET-LH-MUTE         . (43 "2867 Castanet3 LH Mute"))
			(CASTANET-RH-MUTE         . (47 "2871 Castanet3 RH Mute"))
			(CASTANET-LH-RH           . (44 "2868 Castanet3 LH&RH Nrml"))
			(CASTANET-LH-RH-MUTE      . (45 "2869 Castanet3 LH&RH Mute"))
			(CASTANET-FLAM            . (40 "2864 Castanet3 Flam1"))
			(CASTANET-FLAM2           . (41 "2865 Castanet3 Flam2"))
			
			(NAGHAREH-LO              . (48 "3464 Gosha Naghareh Lo"))
			(NAGHAREH-LO-RIM          . (49 "3465 Gosha Naghareh LoRim"))
			(NAGHAREH-HI              . (51 "3467 Gosha Naghareh Hi1"))
			(NAGHAREH-HI2             . (52 "3468 Gosha Naghareh Hi2"))
			(NAGHAREH-HI-RIM          . (53 "3469 Gosha Naghareh HiRim"))
			(NAGHAREH-LO-FLAM         . (50 "3466 Gosha Naghareh LoFlm"))
			(NAGHAREH-HI-FLAM         . (54 "3470 Gosha Naghareh HiFlm"))
			
			(FINGER-CYMBAL-LH         . (55 "3432 Finger Cymbal Opn LH"))
			(FINGER-CYMBAL-LH-MUTE    . (56 "3433 Finger Cymbal Mt LH"))
			(FINGER-CYMBAL-RH         . (57 "3434 Finger Cymbal Opn RH"))
			(FINGER-CYMBAL-RH-MUTE    . (58 "3435 Finger Cymbal Mt RH"))
			(FINGER-CYMBAL-LH-RH-MUTE . (59 "3436 Finger Cymbal Mt L&R"))
			
			(BENDIR-TOM               . (60 "3700 Iran Bendir2 Tom"))
			(BENDIR-CHAP              . (61 "3702 Iran Bendir2 Chap1"))
			(BENDIR-CHAP2             . (62 "3703 Iran Bendir2 Chap2"))
			(BENDIR-BACK              . (63 "3705 Iran Bendir2 Back"))
			(BENDIR-SLAP              . (64 "3701 Iran Bendir2 Slap"))
			(BENDIR-BACK-RIM          . (65 "3706 Iran Bendir2 Bak Rim"))
			(BENDIR-CHAP-MUTE         . (66 "3704 Iran Bendir2 Chap Mt"))
			(BENDIR-BACK-FLAM         . (67 "3709 Iran Bendir2 FlamBck"))
			(BENDIR-FLAM              . (68 "3707 Iran Bendir2 Flam"))
			(BENDIR-TOM-FLAM          . (69 "3708 Iran Bendir2 FlamTom"))
			(BENDIR-RIM-ROLL          . (70 "3711 Iran Bendir2 RimRoll"))
			(BENDIR-ROLL              . (71 "3710 Iran Bendir2 Roll"))
			
			(TEMPO-TOM                . (77 "3895 Tempo Tom"))
			(TEMPO-TOM-MUTE           . (74 "3896 Tempo Tom Mute"))
			(TEMPO-TOM-SLAP           . (75 "3897 Tempo Tom Slap"))
			(TEMPO-BACK               . (78 "3903 Tempo Back1"))
			(TEMPO-BACK2              . (79 "3904 Tempo Back2"))
			(TEMPO-BACK-RIM           . (80 "3905 Tempo Back Rim"))
			(TEMPO-CHAP-TOM           . (72 "3898 Tempo Chap Tom"))
			(TEMPO-CHAP               . (73 "3899 Tempo Chap1"))
			(TEMPO-CHAP2              . (76 "3900 Tempo Chap2"))
			(TEMPO-CHAP-RIM           . (85 "3901 Tempo Chap Rim"))
			(TEMPO-CHAP-RIM-MUTE      . (86 "3902 Tempo Chap Rim Mute"))
			(TEMPO-SNAP-NAIL          . (87 "3906 Tempo Snap Nail"))
			(TEMPO-GLISS-DOWN         . (88 "3913 Tempo Gliss Down (vel->pitch)"))
			(TEMPO-GLISS-UP           . (89 "3912 Tempo Gliss Up   (vel->pitch)"))
			(TEMPO-BACK-FLAM          . (81 "3909 Tempo Flam Bak"))
			(TEMPO-FLAM               . (82 "3907 Tempo Flam"))
			(TEMPO-TOM-FLAM           . (84 "3908 Tempo Flam Tom"))
			(TEMPO-RIM-ROLL           . (90 "3911 Tempo Rim Roll"))
			(TEMPO-ROLL               . (83 "3910 Tempo Roll"))))
       
       (finger-cymbal-klist '((LH         . (55 "3432 Finger Cymbal Opn LH"))
			      (LH-MUTE    . (56 "3433 Finger Cymbal Mt LH"))
			      (RH         . (57 "3434 Finger Cymbal Opn RH"))
			      (RH-MUTE    . (58 "3435 Finger Cymbal Mt RH"))
			      (LH-RH-MUTE . (59 "3436 Finger Cymbal Mt L&R"))))


       (bongo-klist (extract-sub-symbolic-keylist 'bongo general-klist))
       (zangooreh-klist (extract-sub-symbolic-keylist 'zangooreh general-klist))
       (castanet-klist (extract-sub-symbolic-keylist 'castanet general-klist))
       (naghareh-klist (extract-sub-symbolic-keylist 'naghareh general-klist))
       (bendir-klist (extract-sub-symbolic-keylist 'bendir general-klist))
       (tempo-klist (extract-sub-symbolic-keylist 'tempo general-klist))

       (nik2 (make-main-instrument new-iranian-kit-2 general-klist)) )
  (make-sub nik2-bongo nik2 bongo-klist)
  (make-sub nik2-zangooreh nik2 zangooreh-klist)
  (make-sub nik2-castanet nik2 castanet-klist)
  (make-sub nik2-naghareh nik2 naghareh-klist)
  (make-sub nik2-bendir nik2 bendir-klist)
  (make-sub nik2-tempo nik2 tempo-klist)
  (make-sub nik2-finger-cymbal nik2 finger-cymbal-klist) )


(export '(nik2-bongo
	  nik2-zangooreh
	  nik2-castanet
	  nik2-naghareh
	  nik2-bendir
	  nik2-tempo
	  nik2-finger-cymbal) :modx)

(import '(modx:nik2-bongo
	  modx:nik2-zangooreh
	  modx:nik2-castanet
	  modx:nik2-naghareh
	  modx:nik2-bendir
	  modx:nik2-tempo
	  modx:nik2-finger-cymbal) :cyco)
