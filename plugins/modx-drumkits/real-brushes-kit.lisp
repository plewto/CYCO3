;;;; CYCO Yamaha MODX plugin
;;;; real-brushes-kit

(in-package :cyco)


(let ((kmap (symbolic-keynumber-map 
	     '((agogo-ho       . ( 67 "3105 Agogo1 Hi"))
	       (agogo-lo       . ( 68 "3106 Agogo1 Lo"))
	       (block-hi       . ( 76 "2872 Wood Block"))
	       (block-lo       . ( 77 "2872 Wood Block"))
	       (bongo-hi       . ( 60 "2979 Bongo1 Hi 1Fingr 1-2"))
	       (bongo-lo       . ( 61 "2989 Bongo1 Lo 1Fingr 1-2"))
	       (clap           . ( 39 "2895 Hand Clap3 Sw"))
	       (clave          . ( 75 "3217 Claves1"))
	       (conga-mute     . ( 62 "2930 Conga1 Hi SlpMute1-2"))
	       (conga-open-hi  . ( 63 "2924 Conga1 Hi Open 1-2"))
	       (conga-open-lo  . ( 64 "2935 Conga1 Lo Open 1-2"))
	       (cow            . ( 56 "3229 Cowbell5"))
	       (cuica-hi       . ( 78 "2975 Cuica1 Hi"))
	       (cuica-lo       . ( 79 "2974 Cuica1 Lo"))
	       (cym-crash-1    . ( 49 "6204 Jazz Crash1 Sw"))
	       (cym-crash-2    . ( 52 "6218 ZJ Crash Brush Sw"))
	       (cym-crash-3    . ( 57 "6205 Jazz Crash2 Sw"))
	       (cym-ride-1     . ( 51 "6273 Ride Brush3 Tip Sw"))
	       (cym-ride-2     . ( 53 "6274 Ride Brush3 Cup Sw"))
	       (cym-ride-3     . ( 59 "6275 Ride Brush4 Cup Sw"))
	       (cym-splash     . ( 55 "6302 ZJ Splash2 Sw"))
	       (guiro-long     . ( 74 "3112 Guiro1 Long"))
	       (guiro-short    . ( 73 "3113 Guiro1 Short"))
	       (hh             . ( 42 "5763 HH IB Brush Cls"))
	       (hh-open        . ( 46 "5764 HH IB Brush Opn"))
	       (hh-ped         . ( 44 "5765 HH IB Brush Pdl"))
	       (kick-a         . ( 33 "4554 PL Kick1 Sw"))
	       (kick-b         . ( 35 "4555 PL Kick2 Sw"))
	       (kick-c         . ( 36 "4453 Bd PL Jazz HiSna Sw"))
	       (kick-ambiant-1   . ( 94 "4367 Bd Snap 1-2"))
	       (kick-ambiant-2   . ( 95 "4376 Bd Tight 1-2"))
	       (kick-ambiant-3   . ( 96 "4403 Bd Funk St1-2"))
	       (shaker         . ( 82 "2816 ShakerA 13"))
	       (shaker-bell    . ( 83 "2908 Sleigh Bell1"))
	       (shaker-cabasa  . ( 69 "3062 Cabasa1A 1-2"))
	       (shaker-chime   . ( 84 "2912 Wind Chime 1"))
	       (shaker-maracas . ( 70 "3059 Maracas1 Slur"))
	       (shaker-tamb    . ( 54 "2843 Tambourine1 Sw"))
	       (snare          . ( 38 "5243 Sd LW Brush Tap2 Sw"))
	       (snare-0        . ( 93 "4894 Sd BluesHeart St1-3"))
	       (snare-1        . ( 85 "4726 Sd Bld St1-4"))
	       (snare-2        . ( 86 "4885 Sd Blues 3St"))
	       (snare-3        . ( 87 "4929 Sd Hip St1-2"))
	       (snare-4        . ( 88 "4826 Sd Funk St1-3"))
	       (snare-5        . ( 89 "4746 Sd Soul St1-4"))
	       (snare-6        . ( 90 "5177 Sd LdwHMono"))
	       (snare-7        . ( 91 "5180 Sd Heavy"))
	       (snare-8        . ( 92 "4909 Sd Tight St1-4"))
	       (snare-brush-1  . ( 25 "5245 Sd LW Brush Combi Sw"))
	       (snare-brush-2  . ( 27 "5244 Sd LW Brush Slap Sw"))
	       (snare-brush-3  . ( 28 "5251 Sd LW Brush Swir4"))
	       (snare-brush-4  . ( 29 "5252 Sd LW Brush Swir5"))
	       (snare-brush-5  . ( 31 "5242 Sd LW Brush Tap1 Sw"))
	       (snare-edge-1   . ( 34 "5246 Sd LW Brush Rim1 Sw"))
	       (snare-edge-2   . ( 40 "5247 Sd LW Brush Rim2 Sw"))
	       (snare-rim      . ( 37 "5205 Sd Closed Rim6"))
	       (snare-stick    . ( 32 "2855 Stick1"))
	       (snare-swirl-1  . ( 24 "5249 Sd LW Brush Swir2 Sw"))
	       (snare-swirl-2  . ( 26 "5250 Sd LW Brush Swir3"))
	       (snare-swirl-3  . ( 30 "5253 Sd LW Brush Swir6"))
	       (timbale-hi     . ( 65 "3026 Timbale1 Hi 1-3"))
	       (timbale-lo     . ( 66 "3030 Timbale1 Lo 1-2"))
	       (tom-a          . ( 41 "6090 Tom Jazz FLR Sw"))
	       (tom-b          . ( 43 "6089 Tom Jazz LL Sw"))
	       (tom-c          . ( 45 "6089 Tom Jazz LL Sw"))
	       (tom-d          . ( 47 "6088 Tom Jazz LH Sw"))
	       (tom-e          . ( 48 "6087 Tom Jazz M Sw"))
	       (tom-f          . ( 50 "6086 Tom Jazz H Sw"))
	       (triangle       . ( 81 "2883 Triangle1 Open"))
	       (triangle-mute  . ( 80 "2884 Triangle1 Mute"))
	       (vibraslap      . ( 58 "2879 Vibraslap1"))
	       (whistle-hi     . ( 72 "3220 Whistle"))
	       (whistle-lo     . ( 71 "3220 Whistle")))))
      (metal-kmap (symbolic-keynumber-map '((agogo-ho . (67))
					    (agogo-lo . (68))
					    (cow . (56))
					    (triangle . (81))
					    (triangle-mute . (80))
					    (chime . (84))
					    (whistle-hi . (72))
					    (whistle-lo . (71))
					    (vibraslap . (58)))))
      (wood-kmap (symbolic-keynumber-map '((clave . (75))
					   (block-hi . (76))
					   (block-lo . (77))
					   (guiro-long . (74))
					   (guiro-short . (73))
					   (clap . (39)))))
      (shaker-kmap (symbolic-keynumber-map '((shaker . (82))
					     (cabasa . (69))
					     (maracas . (70))
					     (tambourine . (54))
					     (bells . (83)))))
      (hh-kmap (symbolic-keynumber-map '((x . (42))
					 (open . (46))
					 (ped . (44)))))
      (cym-kmap (symbolic-keynumber-map '((ride-1 . (51))
					  (ride-2 . (53))
					  (ride-3 . (59))
					  (crash-1 . (49))
					  (crash-2 . (52))
					  (crash-3 . (57))
					  (splash . (55)))))

      (kick-kmap (symbolic-keynumber-map '((a . (33))
					   (b . (35))
					   (c . (36))
					   (d . (94))
					   (e . (95))
					   (f . (96)))))
      
      (snare-kmap (symbolic-keynumber-map '((x  . (38))
					    (x1 . (93))
					    (x2 . (85))
					    (x3 . (86))
					    (x4 . (87))
					    (x5 . (88))
					    (x6 . (89))
					    (x7 . (90))
					    (x8 . (91))
					    (x9 . (92))
					    (e1 . (34))
					    (e2 . (40))
					    (rim . (37))
					    (stick . (32))
					    (brush-1 . (25))
					    (brush-2 . (27))
					    (brush-3 . (28))
					    (brush-4 . (29))
					    (brush-5 . (31))
					    (swirl-1 . (24))
					    (swirl-2 . (26))
					    (swirl-3 . (30)))))
      (tom-kmap (symbolic-keynumber-map '((a . (41))
					  (b . (43))
					  (c . (45))
					  (d . (47))
					  (e . (48))
					  (f . (50)))))
      (drums-kmap (symbolic-keynumber-map '((bongo-hi . (60))
					    (bongo-lo . (61))
					    (conga-mute . (62))
					    (conga-open-hi . (63))
					    (conga-open-lo . (64))
					    (timbale-hi . (65))
					    (timbale-lo . (66))
					    (cuica-hi . (78))
					    (cuica-lo . (79))))))
  
  (defun real-brushes-kit (channel &key (performance *current-modx-performance*))
    (let ((inst (modx-instrument real-brushes-kit channel
				 :performance performance
				 :keynumber-map kmap)))
      (defparameter rbk-metal (make-instrument 'rbk-metal
					       :parent inst
					       :keynumber-map metal-kmap))
      (defparameter rbk-wood (make-instrument 'rbk-wood
					      :parent inst
					      :keynumber-map wood-kmap))
      (defparameter rbk-shaker (make-instrument 'rbk-shaker
						:parent inst
						:keynumber-map shaker-kmap))
      (defparameter rbk-hats (make-instrument 'rbk-hats
					      :parent inst
					      :keynumber-map hh-kmap))
      (defparameter rbk-cymbals (make-instrument 'rbk-cymbals
						 :parent inst
						 :keynumber-map cym-kmap))
      (defparameter rbk-kick (make-instrument 'rbk-kick
					      :parent inst
					      :keynumber-map kick-kmap))
      (defparameter rbk-snare (make-instrument 'rbk-snare
      					      :parent inst
      					      :keynumber-map snare-kmap))
      (defparameter rbk-tom (make-instrument 'rbk-tom
      					      :parent inst
      					      :keynumber-map tom-kmap))
       (defparameter rbk-drums (make-instrument 'rbk-drums
      					      :parent inst
      					      :keynumber-map drums-kmap))
      inst)))
    
    
(register-modx-drumkit-info 'real-brushes-kit)	       
	       
