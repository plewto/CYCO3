;;;; CYCO modx-drumkits plugin midnight-funk
;;;;
;;;; 24 4671 Bd EDM01            :              : kick-edm
;;;; 25 5393 Sd AnCR             :              : snare-anc
;;;; 26 5353 Sd T9-5             :              : snare-t9A
;;;; 27 4190 RX11 Clap           :              : clap2
;;;; 28 5354 Sd T9Gate           :              : snare-gate
;;;; 29 5206 Sd RockRoll St      :              : snare-roll
;;;; 30 5351 Sd T9-3             :              : snare-t9B
;;;; 31 5352 Sd T9-4             :              : snare-t9C
;;;; 32 5354 Sd T9Gate           :              : snare-gate2
;;;; 33 4563 Bd T9-4             :              : kick-t9A
;;;; 34 5353 Sd T9-5             :              : kick-t9B
;;;; 35 4560 Bd T9-1             : kick1        : 
;;;; 36 4671 Bd EDM01            : kick2        :
;;;; 37 5355 Sd T9Rim            : stick        :
;;;; 38 5349 Sd T9-1             : snare1       :
;;;; 39 4190 RX11 Clap           : clap         :
;;;; 40 5350 Sd T9-2             : snare2       :
;;;; 41 6107 Tom T9 Lo           : tom1         :
;;;; 42 5795 HH Closed RX11      : hat-closed   :
;;;; 43 6107 Tom T9 Lo           : tom2         :
;;;; 44 5767 HH Open T9          : hat-ped      : hat-opn
;;;; 45 6107 Tom T9 Lo           : tom3         :
;;;; 46 5796 HH Open RX11        : hat-open     : hat-open
;;;; 47 6106 Tom T9 Hi           : tom4         :
;;;; 48 3062 Cabasa1A 1-2        : tom5         : shaker
;;;; 49 6303 Crash T9-1          : crash1       :
;;;; 50 3858 Tombek Tek Dead Sw  : tom6         : 
;;;; 51 4247 Cowbell RX11        : ride1        :
;;;; 52 6287 China2              : chinese      :
;;;; 53 6281 Ride Cup Mono       : ride-bell    :
;;;; 54 4258 Tambourine RX5      : tambourine   : shaker4
;;;; 55 6179 Crash3              : splash       : crash3
;;;; 56 3223 Cowbell1            : cow          : 
;;;; 57 6180 Crash4              : crash2       :
;;;; 58 4242 Cowbell T8          : vibraslap    : cow2
;;;; 59 6231 Ride2 R             : ride2        :
;;;; 60 4231 Conga T8            : bongo-high   : conga1
;;;; 61 4231 Conga T8            : bongo-low    : conga2
;;;; 62 2922 Conga1 Hi Tip       : conga-high   : conga-tip
;;;; 63 2928 Conga1 Hi Slap Open : conga-open   : conga-slap
;;;; 64 2926 Conga1 Hi Open 2    : conga-low    : conga-open
;;;; 65 3026 Timbale1 Hi 1-3     : timbale-high :
;;;; 66 3030 Timbale1 Lo 1-2     : timbale-low  :
;;;; 67 4286 Analog Click        : agogo-high   : click
;;;; 68 4231 Conga T8            : agogo-low    : conga4
;;;; 69 3062 Cabasa1A 1-2        : cabasa       : shaker2
;;;; 70 3059 Maracas1 Slur       : maracas      : shaker3
;;;; 71 2613 FxGun2              : whistle1     : fx1
;;;; 72 2612 FxGun1              : whistle2     : fx2
;;;; 73 2629 Scratch H           : guiro-short  : scratch1
;;;; 74 2631 Scratch Down        : guiro-long   : scratch2
;;;; 75 2512 Hi Q1               : clave        : hiq1
;;;; 76 2512 Hi Q1               : block-high   : hiq2
;;;; 77 2513 Hi Q2               : block-low    : hiq3
;;;; 78 2630 Scratch L           : cuica-mute   : scratch3
;;;; 79 2630 Scratch L           : cuica-open   : scratch4
;;;; 80 2884 Triangle1 Mute      : triangle-mute:
;;;; 81 2883 Triangle1 Open      : triangle     :
;;;; 82 4250 Analog Shaker       :              : shaker5
;;;; 83 2908 Sleigh Bell1        :              : shaker6
;;;; 84 2912 Wind Chime 1        :              : shaker7
;;;; 85 4959 Sd Piccolo          :              : snare-piccolo
;;;; 86 5380 Sd T8-5             :              : snare-t8
;;;; 87 5209 Sd RockRollD St     :              : snare-roll2
;;;; 88 5227 Sd Brush Med St     :              : snare-brush
;;;; 89 4596 Bd BlpHd            :              : kick-B
;;;; 90 5397 Sd Jungle1          :              : snare-jungle
;;;; 91 4629 Bd Sustain          :              : kick-sustain
;;;; 92 5401 Sd D&B1             :              : snare-DB1
;;;; 93 4607 Bd Break2           :              : kick-break
;;;; 94 5427 Sd Dist             :              : snare-dist
;;;; 95 4630 Bd TekPower         :              : kick-power
;;;; 96 4615 Bd Distortion R     :              : kick-dist




(let ((common-klist (general-midi-drum-keylist
		     '((kick-edm . (24))
		       (snare-anc . (25))
		       (snare-t9A . (26))
		       (clap2 . (27))
		       (snare-gate . (28))
		       (snare-roll . (29))
		       (snare-t9B . (30))
		       (snare-t9C . (31))
		       (snare-gate2 . (32))
		       (kick-t9A . (33))
		       (kick-t9B . (34))
		       (hat-opn . (44))
		       (hat-open . (46))
		       (shaker . (48))
		       (shaker4 . (54))
		       (crash3 . (55))
		       (cow2 . (58))
		       (conga1 . (60))
		       (conga2 . (61))
		       (conga-tip . (62))
		       (conga-slap . (63))
		       (conga-open . (64))
		       (click . (67))
		       (conga4 . (68))
		       (shaker2 . (69))
		       (shaker3 . (70))
		       (fx1 . (71))
		       (fx2 . (72))
		       (scratch1 . (73))
		       (scratch2 . (74))
		       (hiq1 . (75))
		       (hiq2 . (76))
		       (hiq3 . (77))
		       (scratch3 . (78))
		       (scratch4 . (79))
		       (shaker5 . (82))
		       (shaker6 . (83))
		       (shaker7 . (84))
		       (snare-piccolo . (85))
		       (snare-t8 . (86))
		       (snare-roll2 . (87))
		       (snare-brush . (88))
		       (kick-B . (89))
		       (snare-jungle . (90))
		       (kick-sustain . (91))
		       (snare-DB1 . (92))
		       (kick-break . (93))
		       (snare-dist . (94))
		       (kick-power . (95))
		       (kick-dist . (96)))
		     '(hat-ped tom5 splash vibraslap bongo-high bongo-low
			       conga-high conga-open conga-low agogo-high
			       agogo-low whistle1 whistle2 guiro-short
			       guiro-long clave block-high block-low
			       cuica-mute cuica-open)))
      (KICK-KMAP  (symbolic-keynumber-map '((A          . (35))
					    (B          . (89))
					    (C          . (36))
					    (EDM        . (24))
					    (T9A        . (33))
					    (T9B        . (34))
					    (SUSTAIN    . (91))
					    (BREAK      . (93))
					    (POWER      . (95)))))
      (SNARE-KMAP  (symbolic-keynumber-map '((X         . (38))
					     (X2        . (40))
					     (ANC       . (25))
					     (T9A       . (26))
					     (GATE      . (28))
					     (GATE2     . (32))
					     (T9B       . (30))
					     (T9C       . (31))
					     (T8        . (86))
					     (PICCOLO   . (85))
					     (ROLL2     . (87))
					     (JUNGLE    . (90))
					     (DB1       . (92))
					     (DIST      . (94))
					     (STICK     . (37))
					     (BRUSH     . (88))
					     (ROLL      . (29)))))
      (TOM-KMAP  (symbolic-keynumber-map '((A         . (41))
					   (B         . (43))
					   (C         . (45))
					   (D         . (47))
					   (E         . (50)))))
      (CONGA-KMAP  (symbolic-keynumber-map '((A         . (60))
					     (B         . (61))
					     (C         . (68))
					     (TIP       . (62))
					     (SLAP      . (63))
					     (OPEN      . (64)))))
      (HAT-KMAP  (symbolic-keynumber-map '((X      . (42))
					   (OPN    . (44))
					   (OPEN   . (46)))))
      (CYM-KMAP  (symbolic-keynumber-map '((RIDE            . (51))
					   (BELL            . (53))
					   (RIDE2           . (59))
					   (CRASH           . (49))
					   (CRASH2          . (57))
					   (CRASH3          . (55))
					   (CHINESE         . (52)))))
      (TIMBALE-KMAP  (circular-list-keynumber-map '(66 65)))
      (SHAKER-KMAP  (circular-list-keynumber-map '(48 54 69 70 82 83 84 69 70 54)))
      (COW-KMAP  (circular-list-keynumber-map '(56 58 80 81)))
      (HIQ-KMAP  (circular-list-keynumber-map '(75 76 77)))
      (CLAP-KMAP  (circular-list-keynumber-map '(39 27 67)))
      (SCRATCH-KMAP  (circular-list-keynumber-map '(73 74 78 79 71 72))) )
  (defun midnight-funk (channel &key (performance *current-modx-performance*))
    (let ((mfnk (make-instrument 'midnight-funk
				 :channel channel
				 :parent performance
				 :keynumber-map (symbolic-keynumber-map common-klist))))
      (defparameter midnight-funk mfnk)
      (instrument mfnk-kick :parent mfnk :keynumber-map kick-kmap)
      (instrument mfnk-snare :parent mfnk :keynumber-map snare-kmap)
      (instrument mfnk-tom :parent mfnk :keynumber-map tom-kmap)
      (instrument mfnk-conga :parent mfnk :keynumber-map conga-kmap)
      (instrument mfnk-hat :parent mfnk :keynumber-map hat-kmap)
      (instrument mfnk-cym :parent mfnk :keynumber-map cym-kmap)
      (instrument mfnk-timbale :parent mfnk :keynumber-map timbale-kmap)
      (instrument mfnk-shaker :parent mfnk :keynumber-map shaker-kmap)
      (instrument mfnk-cow :parent mfnk :keynumber-map cow-kmap)
      (instrument mfnk-hiq :parent mfnk :keynumber-map hiq-kmap)
      (instrument mfnk-clap :parent mfnk :keynumber-map clap-kmap)
      (instrument mfnk-scratch :parent mfnk :keynumber-map scratch-kmap)
      mfnk))) 
  


