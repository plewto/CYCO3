;;;; CYCO Yamaha modx-drums plugin  new-iranian-kit
;;;; 
;;;;  key  wave
;;;; [ 24] 3528 Udho Tom Hole        : 
;;;; [ 25] 3530 Udho Back1           : 
;;;; [ 26] 3532 Udho Chap Body       : 
;;;; [ 27] 3533 Udho Chap Skin       : 
;;;; [ 28] 3534 Udho Chap Top Hole   : 
;;;; [ 29] 3694 Iran Bendir1 Chap1   : 
;;;; [ 30] 3696 Iran Bendir1 Rim     : 
;;;; [ 31] 3693 Iran Bendir1 Slap    : 
;;;; [ 32] 3698 Iran Bendir1 Back    : 
;;;; [ 33] 3697 Iran Bendir1 Bck&Chp : 
;;;; [ 34] 3695 Iran Bendir1 Chap2   : 
;;;; [ 35] 3699 Iran Bendir1 Roll    : 
;;;; [ 36] 3868 Tombak Tom1          : 
;;;; [ 37] 3877 Tombak Chap1         : 
;;;; [ 38] 3878 Tombak Chap2         : 
;;;; [ 39] 3879 Tombak Back1         : 
;;;; [ 40] 3882 Tombak Nail Swish    : 
;;;; [ 41] 3881 Tombak Pelang        : 
;;;; [ 42] 3886 Tombak Roll 8fingers : 
;;;; [ 43] 3883 Tombak Flam1         : 
;;;; [ 44] 3884 Tombak Flam2         : 
;;;; [ 45] 3885 Tombak Flam & Back   : 
;;;; [ 46] 3880 Tombak Back2         : 
;;;; [ 47] 3876 Tombak Slap          : 
;;;; [ 48] 3790 Daf Tom              : 
;;;; [ 49] 3797 Daf Chain Up1        : 
;;;; [ 50] 3795 Daf Back1            : 
;;;; [ 51] 3798 Daf Chain Down1      : 
;;;; [ 52] 3801 Daf Chap No Chain    : 
;;;; [ 53] 3802 Daf Roll             : 
;;;; [ 54] 3803 Daf Roll Chain       : 
;;;; [ 55] 3804 Daf Roll Rim         : 
;;;; [ 56] 3792 Daf Slap Open        : 
;;;; [ 57] 3793 Daf Slap Mute        : 
;;;; [ 58] 4047 Naghareh Roll        : 
;;;; [ 59] 4046 Naghareh Tom         : 
;;;; [ 60] 4049 Naghareh Snap        : 
;;;; [ 61] 4048 Naghareh Chap        : 
;;;; [ 62] 4051 Naghareh Flam 3Fing  : 
;;;; [ 63] 4050 Naghareh Back        : 
;;;; [ 64] 3353 Dohol Gorz           : 
;;;; [ 65] 3354 Dohol Gorz & Tarkeh  : 
;;;; [ 66] 3355 Dohol Tarkeh1        : 
;;;; [ 67] 3356 Dohol Tarkeh2        : 
;;;; [ 68] 3786 Req Tom & Zang       : 
;;;; [ 69] 3787 Req Chap Zang Open   : 
;;;; [ 70] 3784 Req Slap             : 
;;;; [ 71] 3894 Tombak Tempo Roll    : 
;;;; [ 72] 3887 Tombak Tempo Tom     : 
;;;; [ 73] 3893 Tombak Tempo ChapFlm : 
;;;; [ 74] 3891 Tombak Tempo Back    : 
;;;; [ 75] 3892 Tombak Tempo Back Mt : 
;;;; [ 76] 3889 Tombak Tempo Chap    : 
;;;; [ 77] 3890 Tombak Tempo Chap Mt : 
;;;; [ 78] 3888 Tombak Tempo Slap    : 
;;;; [ 79] 4013 Zarb1 Back           : 
;;;; [ 80] 4014 Zarb1 Flam Open      : 
;;;; [ 81] 4012 Zarb1 Chap           : 
;;;; [ 82] 4011 Zarb1 Slap           : 
;;;; [ 83] 4015 Zarb1 Roll           : 
;;;; [ 84] 3808 Dayereh1 Tom         : 
;;;; [ 85] 3809 Dayereh1 Tom Mute    : 
;;;; [ 86] 3810 Dayereh1 Back        : 
;;;; [ 87] 3811 Dayereh1 Back Mute1  : 
;;;; [ 88] 3812 Dayereh1 Back Mute2  : 
;;;; [ 89] 3813 Dayereh1 Snap        : 
;;;; [ 90] 3814 Dayereh1 Chap        : 
;;;; [ 91] 3815 Dayereh1 Soft Roll   : 
;;;; [ 92] 3816 Dayereh1 Tom Roll    : 
;;;; [ 93] 3817 Dayereh1 Rim Roll    : 
;;;; [ 94] 3823 Dayereh2 Flam Tom    : 
;;;; [ 95] 3824 Dayereh2 Flam Back   : 
;;;; [ 96] 3692 Iran Bendir1 Tom     :

(in-package :modx)


(let* ((general-klist '((UDHO-TOM-HOLE        . (24 "3528 Udho Tom Hole")) 
			(UDHO-BACK            . (25 "3530 Udho Bac1" ))
			(UDHO-CHAP-BODY       . (26 "3532 Udho Chap Boy")) 
			(UDHO-CHAP-SKIN       . (27 "3533 Udho Chap Skn")) 
			(UDHO-CHAP-TOP-HOLE   . (28 "3534 Udho Chap Top Hoe")) 
			(BENDIR-TOM           . (96 "3692 Iran Bendir1 Tm"))
			(BENDIR-BACK          . (32 "3698 Iran Bendir1 Bak")) 
			(BENDIR-CHAP          . (29 "3694 Iran Bendir1 Cha1")) 
			(BENDIR-CHAP2         . (34 "3695 Iran Bendir1 Cha2")) 
			(BENDIR-SLAP          . (31 "3693 Iran Bendir1 Slp")) 
			(BENDIR-RIM           . (30 "3696 Iran Bendir1 Rim"))
			(BENDIR-BACK-CHAP     . (33 "3697 Iran Bendir1 Bck&Chp")) 
			(BENDIR-ROLL          . (35 "3699 Iran Bendir1 Roll"))
			(TOMBAK-TOM           . (36 "3868 Tombak Tom1")) 
			(TOMBAK-BACK          . (39 "3879 Tombak Back1")) 
			(TOMBAK-CHAP          . (37 "3877 Tombak Chap1")) 
			(TOMBAK-CHAP2         . (38 "3878 Tombak Chap2")) 
			(TOMBAK-SLAP          . (47 "3876 Tombak Slap")) 
			(TOMBAK-NAIL          . (40 "3882 Tombak Nail Swish")) 
			(TOMBAK-BACK2         . (46 "3880 Tombak Back2")) 
			(TOMBAK-PELANG        . (41 "3881 Tombak Pelang")) 
			(TOMBAK-FLAM          . (43 "3883 Tombak Flam1")) 
			(TOMBAK-FLAM2         . (44 "3884 Tombak Flam2")) 
			(TOMBAK-FLAM-BACK     . (45 "3885 Tombak Flam & Back")) 
			(TOMBAK-ROLL          . (42 "3886 Tombak Roll 8fingers")) 
			(DAF-TOM              . (48 "3790 Daf Tom")) 
			(DAF-BACK             . (50 "3795 Daf Back1")) 
			(DAF-CHAP             . (52 "3801 Daf Chap No Chain")) 
			(DAF-SLAP-OPEN        . (56 "3792 Daf Slap Open")) 
			(DAF-SLAP-MUTE        . (57 "3793 Daf Slap Mute")) 
			(DAF-CHAIN-UP         . (49 "3797 Daf Chain Up1")) 
			(DAF-CHAIN-DOWN       . (51 "3798 Daf Chain Down1")) 
			(DAF-ROLL             . (53 "3802 Daf Roll")) 
			(DAF-ROLL-CHAIN       . (54 "3803 Daf Roll Chain")) 
			(DAF-ROLL-RIM         . (55 "3804 Daf Roll Rim")) 
			(NAGHAREH-TOM         . (59 "4046 Naghareh Tom")) 
			(NAGHAREH-BACK        . (63 "4050 Naghareh Back")) 
			(NAGHAREH-SNAP        . (60 "4049 Naghareh Snap")) 
			(NAGHAREH-CHAP        . (61 "4048 Naghareh Chap")) 
			(NAGHAREH-FLAM        . (62 "4051 Naghareh Flam 3Fing")) 
			(NAGHAREH-ROLL        . (58 "4047 Naghareh Roll")) 
			(DOHOL-GORZ           . (64 "3353 Dohol Gorz")) 
			(DOHOL-TARKEH         . (66 "3355 Dohol Tarkeh1")) 
			(DOHOL-GORZ-TARKEH    . (65 "3354 Dohol Gorz & Tarkeh")) 
			(DOHOL-TARKEH2        . (67 "3356 Dohol Tarkeh2")) 
			(REQ-TOM              . (68 "3786 Req Tom & Zang")) 
			(REQ-CHAP             . (69 "3787 Req Chap Zang Open")) 
			(REQ-SLAP             . (70 "3784 Req Slap")) 
			(TEMPO-TOMBAK-TOM     . (72 "3887 Tombak Tempo Tom")) 
			(TEMPO-TOMBAK-BACK    . (74 "3891 Tombak Tempo Back")) 
			(TEMPO-TOMBAK-CHAP    . (76 "3889 Tombak Tempo Chap")) 
			(TEMPO-TOMBAK-SLAP    . (78 "3888 Tombak Tempo Slap")) 
			(TEMPO-TOMBAK-BACK2   . (75 "3892 Tombak Tempo Back Mt")) 
			(TEMPO-TOMBAK-CHAP2   . (77 "3890 Tombak Tempo Chap Mt")) 
			(TEMPO-TOMBAK-FLAM    . (73 "3893 Tombak Tempo ChapFlm")) 
			(TEMPO-TOMBAK-ROLL    . (71 "3894 Tombak Tempo Roll")) 
			(ZARB-BACK            . (79 "4013 Zarb1 Back")) 
			(ZARB-CHAP            . (81 "4012 Zarb1 Chap")) 
			(ZARB-SLAP            . (82 "4011 Zarb1 Slap")) 
			(ZARB-FLAM            . (80 "4014 Zarb1 Flam Open")) 
			(ZARB-ROLL            . (83 "4015 Zarb1 Roll")) 
			(DAYEREH-TOM          . (84 "3808 Dayereh1 Tom")) 
			(DAYEREH-BACK         . (86 "3810 Dayereh1 Back")) 
			(DAYEREH-CHAP         . (90 "3814 Dayereh1 Chap")) 
			(DAYEREH-SNAP         . (89 "3813 Dayereh1 Snap")) 
			(DAYEREH-TOM-MUTE     . (85 "3809 Dayereh1 Tom Mute")) 
			(DAYEREH-BACK-MUTE    . (87 "3811 Dayereh1 Back Mute1")) 
			(DAYEREH-BACK-MUTE2   . (88 "3812 Dayereh1 Back Mute2")) 
			(DAYEREH-ROLL         . (91 "3815 Dayereh1 Soft Roll")) 
			(DAYEREH-TOM-ROLL     . (92 "3816 Dayereh1 Tom Roll")) 
			(DAYEREH-RIM-ROLL     . (93 "3817 Dayereh1 Rim Roll")) 
			(DAYEREH-TOM-FLAM     . (94 "3823 Dayereh2 Flam Tom")) 
			(DAYEREH-BACK-FLAM    . (95 "3824 Dayereh2 Flam Back"))))
       (udho-klist (extract-sub-symbolic-keylist 'udho general-klist))
       (bendir-klist (extract-sub-symbolic-keylist 'bendir general-klist))
       (tombak-klist (extract-sub-symbolic-keylist 'tombak general-klist))
       (daf-klist (extract-sub-symbolic-keylist 'daf general-klist))
       (naghareh-klist (extract-sub-symbolic-keylist 'naghareh general-klist))
       (dohol-klist (extract-sub-symbolic-keylist 'dohol general-klist))
       (req-klist (extract-sub-symbolic-keylist 'req general-klist))
       (tempo-tombak-klist (extract-sub-symbolic-keylist 'tempo-tombak general-klist))
       (zarb-klist (extract-sub-symbolic-keylist 'zarb general-klist))
       (dayereh-klist (extract-sub-symbolic-keylist 'dayereh general-klist))
       (nik  (make-main-instrument new-iranian-kit    general-klist)) )
      
  (make-sub nik-udho nik udho-klist)
  (make-sub nik-bendir nik bendir-klist)
  (make-sub nik-tombak nik tombak-klist)
  (make-sub nik-daf nik daf-klist)
  (make-sub nik-naghareh nik naghareh-klist)
  (make-sub nik-dohol nik dohol-klist)
  (make-sub nik-req nik req-klist)
  (make-sub nik-tempo-tombak nik tempo-tombak-klist)
  (make-sub nik-zarb nik zarb-klist)
  (make-sub nik-dayereh nik dayereh-klist))

(export '(nik-udho
	  nik-bendir
	  nik-tombak
	  nik-daf
	  nik-naghareh
	  nik-dohol
	  nik-req
	  nik-tempo-tombak
	  nik-zarb
	  nik-dayereh) :modx)

(import '(modx:nik-udho
	  modx:nik-bendir
	  modx:nik-tombak
	  modx:nik-daf
	  modx:nik-naghareh
	  modx:nik-dohol
	  modx:nik-req
	  modx:nik-tempo-tombak
	  modx:nik-zarb
	  modx:nik-dayereh) :cyco)
