;;;; CYCO Yamaha MODX plugin
;;;; new-iranian-kit
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

(in-package :cyco)


(let ((general-kmap (symbolic-keynumber-map 
		     '((UDHO-TOM-HOLE        . (24 "3528 Udho Tom Hole")) 
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
		       (DAYEREH-BACK-FLAM    . (95 "3824 Dayereh2 Flam Back")))))
      
      (udho-kmap (symbolic-keynumber-map
		  '((TOM         . (24 ))
		    (BACK        . (25 ))
		    (CHAP-BODY   . (26)) 
		    (CHAP-SKIN   . (27)) 
		    (CHAP-HOLE   . (28)))))
      
      (bendir-kmap (symbolic-keynumber-map
		    '((TOM           . (96))
		      (BACK          . (32)) 
		      (CHAP          . (29)) 
		      (CHAP2         . (34)) 
		      (SLAP          . (31)) 
		      (RIM           . (30))
		      (BACK-CHAP     . (33)) 
		      (ROLL          . (35)))))
      
      (tombak-kmap (symbolic-keynumber-map
		    '((TOM           . (36)) 
		      (BACK          . (39)) 
		      (CHAP          . (37)) 
		      (CHAP2         . (38)) 
		      (SLAP          . (47)) 
		      (NAIL          . (40)) 
		      (BACK2         . (46)) 
		      (PELANG        . (41)) 
		      (FLAM          . (43)) 
		      (FLAM2         . (44)) 
		      (FLAM-BACK     . (45)) 
		      (ROLL          . (42)))))
      
      (daf-kmap (symbolic-keynumber-map
		 '((TOM              . (48)) 
		   (BACK             . (50)) 
		   (CHAP             . (52)) 
		   (SLAP-OPEN        . (56)) 
		   (SLAP-MUTE        . (57)) 
		   (CHAIN-UP         . (49)) 
		   (CHAIN-DOWN       . (51)) 
		   (ROLL             . (53)) 
		   (ROLL-CHAIN       . (54)) 
		   (ROLL-RIM         . (55)))))
      
      
      (naghareh-kmap (symbolic-keynumber-map 
		      '((TOM         . (59)) 
			(BACK        . (63)) 
			(SNAP        . (60)) 
			(CHAP        . (61)) 
			(FLAM        . (62)) 
			(ROLL        . (58)))))
      
      (dohol-kmap (symbolic-keynumber-map
		   '((GORZ           . (64)) 
		     (TARKEH         . (66)) 
		     (GORZ-TARKEH    . (65)) 
		     (TARKEH2        . (67)))))
      
      (req-kmap (symbolic-keynumber-map
		 '((TOM              . (68)) 
		   (CHAP             . (69)) 
		   (SLAP             . (70)))))
      
      (tempo-tombak-kmap (symbolic-keynumber-map 
			  '((TOM     . (72)) 
			    (BACK    . (74)) 
			    (CHAP    . (76)) 
			    (SLAP    . (78)) 
			    (BACK2   . (75)) 
			    (CHAP2   . (77)) 
			    (FLAM    . (73)) 
			    (ROLL    . (71)))))
      
      (zarb-kmap (symbolic-keynumber-map 
		  '((BACK            . (79)) 
		    (CHAP            . (81)) 
		    (SLAP            . (82)) 
		    (FLAM            . (80)) 
		    (ROLL            . (83)))))
      
      (dayereh-kmap (symbolic-keynumber-map 
		     '((TOM          . (84)) 
		       (BACK         . (86)) 
		       (CHAP         . (90)) 
		       (SNAP         . (89)) 
		       (TOM-MUTE     . (85)) 
		       (BACK-MUTE    . (87)) 
		       (BACK-MUTE2   . (88)) 
		       (ROLL         . (91)) 
		       (TOM-ROLL     . (92)) 
		       (RIM-ROLL     . (93)) 
		       (TOM-FLAM     . (94)) 
		       (BACK-FLAM    . (95))))) )
  (defun new-iranian-kit (channel &key (performance *current-modx-performance*))
    (let ((nik (modx-instrument new-iranian-kit channel
				:performance performance
				:keynumber-map general-kmap)))
    
      (instrument nik-udho :parent nik :keynumber-map udho-kmap
		  :remarks "Clay drum")
      (instrument nik-bendir :parent nik :keynumber-map bendir-kmap
		  :remarks "Single head frame drum")
      (instrument nik-tombak :parent nik :keynumber-map tombak-kmap
		  :remarks "Goblet drum")
      (instrument nik-daf :parent nik :keynumber-map daf-kmap
		  :remarks "Frame drum  ~dayereh")
      (instrument nik-naghareh :parent nik :keynumber-map naghareh-kmap
		  :remarks "Piar small kettle drum")
      (instrument nik-dohol :parent nik :keynumber-map dohol-kmap
		  :remarks "Large barrow drum, usually spelled 'Dhol'")
      (instrument nik-req :parent nik :keynumber-map req-kmap
		  :remarks "Frame drum with jingles ~Tambourine")
      (instrument nik-tempo-tombak :parent nik :keynumber-map tempo-tombak-kmap
		  :remarks "")
      (instrument nik-zarb :parent nik :keynumber-map zarb-kmap
		  :remarks "Goblet drum, type of Tombak")
      (instrument nik-dayereh :parent nik :keynumber-map dayereh-kmap
		  :remarks "Frame drum ~daf")
      nik)))

(register-modx-drumkit-info 'new-iranian-kit)					     
		       
