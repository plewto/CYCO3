;;;; CYCO Yamaha MODX plugin
;;;; new-maple-custum-kit
;;;;

(in-package :cyco)

(let ((global-key-list '((hh-ped         . ( 24 "5682 HH Dark Pedal 1-2 St"))  ;; C0
			 (hh-x           . ( 42 "5662 HH Dark Closed 1-4St"))
			 (hh-opn         . ( 44 "5551 HH Open 1-2St"))
			 (hh-open        . ( 46 "5694 HH Dark Open 1-2 St"))
			 (brush-soft     . ( 25 "5224 Sd Brush Soft St"))
			 (brush-swirl    . ( 26 "5236 Sd Brush Swir St"))
			 (brush-med      . ( 27 "5227 Sd Brush Med St"))
			 (brush-swirl-2  . ( 28 "5239 Sd Brush SwirAtt St"))
			 (snare-roll     . ( 29 "5142 Sd Metal SnRoll St   roll loop"))
			 (snare-x        . ( 31 "5061 Sd Oak 2 St"))
			 (snare-rim      . ( 32 "2856 Stick2"))
			 (snare-open     . ( 34 "5136 Sd Metal OpRm 2 St"))
			 (snare-rim-2    . ( 37 "5139 Sd Metal ClRm St"))
			 (snare-x-2      . ( 38 "5112 Sd Metal 1-4 St"))
			 (snare-roll-2   . ( 39 "5142 Sd Metal SnRoll St   roll decay"))
			 (snare-x-3      . ( 40 "5115 Sd Metal 1-OpRm St"))
			 (kick-A         . ( 33 "4479 Bd Maple Close 2 L"))
			 (kick-B         . ( 35 "4481 Bd Maple Open 1-2 St"))
			 (kick-C         . ( 36 "4481 Bd Maple Open 1-2 St"))
			 (tom-A          . ( 41 "5978 Tom Maple Flr 1-2 RL"))
			 (tom-B          . ( 43 "5983 Tom Maple Flr 2 St"))
			 (tom-C          . ( 45 "5971 Tom Maple Mid 2 St"))
			 (tom-D          . ( 47 "5963 Tom Maple Mid 1-2 St"))
			 (tom-E          . ( 48 "5959 Tom Maple Hi 2 St"))
			 (tom-F          . ( 50 "5951 Tom Maple Hi 1-2 St"))
			 (cym-crash      . ( 49 "6181 Crash Brite St"))
			 (cym-ride       . ( 51 "6233 Ride Brite St"))
			 (cym-china      . ( 52 "6288 China3 St"))
			 (cym-cup        . ( 53 "6237 Ride Brite Cup St"))
			 (cym-splash     . ( 55 "6297 Splash2 St"))
			 (cym-crash-2    . ( 57 "6185 Crash Dark St"))
			 (cym-ride-2     . ( 59 "6241 Ride Warm St"))
			 (bongo-macho    . ( 60 "3000 Bongo2 Hi Open 2"))
			 (bongo-hembra   . ( 61 "3004 Bongo2 Lo Open 2"))
			 (conga-slap     . ( 62 "2944 Conga2 Hi Slap 2"))
			 (conga-open-2   . ( 63 "2947 Conga2 Hi Open 2"))
			 (conga-open     . ( 64 "2950 Conga2 Lo Open 1"))
			 (timbale-hi     . ( 65 "3038 Timbale2 Hi 2"))
			 (timbale-lo     . ( 66 "3042 Timbale2 Lo 2"))
			 (agogo-hi       . ( 67 "3105 Agogo1 Hi"))
			 (agogo-lo       . ( 68 "3106 Agogo1 Lo"))
			 (cow            . ( 56 "3225 Cowbell3 Sw"))
			 (tambourine     . ( 54 "2846 Tambourine2 Sw"))
			 (shaker         . ( 69 "3065 Cabasa1B 1-4"))
			 (shaker-2       . ( 70 "3059 Maracas1 Slur"))
			 (shaker-3       . ( 82 "2820 ShakerB Sw"))
			 (guiro-short    . ( 73 "3118 MetalGuiro Short 1-2"))
			 (guiro-long     . ( 74 "3121 MetalGuiro Long"))
			 (clave          . ( 75 "3218 Claves2"))
			 (block-hi       . ( 76 "2914 Resin Block Small"))
			 (block-lo       . ( 77 "2915 Resin Block Large"))
			 (cuica-hi       . ( 78 "2975 Cuica1 Hi"))
			 (cuica-lo       . ( 79 "2974 Cuica1 Lo"))
			 (triangle-mute  . ( 80 "2884 Triangle1 Mute"))
			 (triangle-open  . ( 81 "2883 Triangle1 Open"))
			 (clap           . ( 85 "2889 Hand Clap1 St"))
			 (surdo          . ( 86 "3126 Surdo2 Open"))
			 (conga-tip      . ( 87 "2941 Conga2 Hi Tip"))
			 (conga-heel     . ( 88 "2940 Conga2 Hi Heel"))
			 (conga-slap     . ( 89 "2942 Conga2 Hi Slap 1-2"))
			 (conga-mute     . ( 90 "2948 Conga2 Hi Mute"))
			 (conga-open     . ( 91 "2945 Conga2 Hi Open 1-2"))
			 (conga-lo-slide . ( 92 "2952 Conga2 Lo Slide"))
			 (conga-lo-open  . ( 93 "2949 Conga2 Lo Open 1-2"))
			 (djembe-mute    . ( 94 "3278 Djembe2 Mute"))
			 (djembe-lo      . ( 95 "3276 Djembe2 Lo"))
			 (djembe-slap    . ( 96 "3277 Djembe2 Slap"))
			 (sleigh-bell    . ( 83 "2909 Sleigh Bell2"))
			 (wind-chime     . ( 84 "2912 Wind Chime 1"))
			 (whistle-hi     . ( 71 "3220 Whistle"))
			 (whistle-lo     . ( 72 "3220 Whistle"))
			 (castanete      . ( 30 "2860 Castanet1"))
			 (vibraslap      . ( 58 "2880 Vibraslap2")))))

  (defun new-maple-kit (channel &key (performance *current-modx-performance*))
    (let ((inst (modx-instrument new-maple-kit channel
				 :performance performance
				 :keynumber-map (symbolic-keynumber-map global-key-list)
				 :remarks
				 "May sub for following kits:
New Oak Custom
BeachwoodSnare"
	)))			 
      (defparameter new-maple-kit inst)
      (instrument maple-kick :parent inst
		  :keynumber-map (symbolic-keynumber-map
				  '((A         . ( 33 "4479 Bd Maple Close 2 L"))
				    (B         . ( 35 "4481 Bd Maple Open 1-2 St"))
				    (C         . ( 36 "4481 Bd Maple Open 1-2 St")))))
      (instrument maple-snare :parent inst
		  :keynumber-map (symbolic-keynumber-map
				  '((x        . ( 31 "5061 Sd Oak 2 St"))
				    (x-2      . ( 38 "5112 Sd Metal 1-4 St"))
				    (x-3      . ( 40 "5115 Sd Metal 1-OpRm St"))
				    (open     . ( 34 "5136 Sd Metal OpRm 2 St"))
				    (rim      . ( 32 "2856 Stick2"))
				    (rim-2    . ( 37 "5139 Sd Metal ClRm St"))
				    (roll     . ( 29 "5142 Sd Metal SnRoll St roll loop"))
				    (roll-2   . ( 39 "5142 Sd Metal SnRoll St roll decay"))
				    (soft     . ( 25 "5224 Sd Brush Soft St"))
				    (med      . ( 27 "5227 Sd Brush Med St"))
				    (swirl    . ( 26 "5236 Sd Brush Swir St"))
				    (swirl-2  . ( 28 "5239 Sd Brush SwirAtt St"))
				    (clap     . ( 85 "2889 Hand Clap1 St")))))
      (instrument maple-tom :parent inst
		  :keynumber-map (symbolic-keynumber-map
				  '((A          . ( 41 "5978 Tom Maple Flr 1-2 RL"))
				    (B          . ( 43 "5983 Tom Maple Flr 2 St"))
				    (C          . ( 45 "5971 Tom Maple Mid 2 St"))
				    (D          . ( 47 "5963 Tom Maple Mid 1-2 St"))
				    (E          . ( 48 "5959 Tom Maple Hi 2 St"))
				    (F          . ( 50 "5951 Tom Maple Hi 1-2 St")))))
      (instrument maple-latin :parent inst
		  :keynumber-map (symbolic-keynumber-map
				  '((conga-open     . ( 64 "2950 Conga2 Lo Open 1"))
				    (conga-mute     . ( 90 "2948 Conga2 Hi Mute"))
				    (conga-slap     . ( 62 "2944 Conga2 Hi Slap 2"))
				    (conga-tip      . ( 87 "2941 Conga2 Hi Tip"))
				    (conga-heel     . ( 88 "2940 Conga2 Hi Heel"))
				    (conga-slap     . ( 89 "2942 Conga2 Hi Slap 1-2"))
				    (conga-open-2   . ( 63 "2947 Conga2 Hi Open 2"))
				    (conga-open-3   . ( 91 "2945 Conga2 Hi Open 1-2"))
				    (conga-lo-slide . ( 92 "2952 Conga2 Lo Slide"))
				    (conga-lo-open  . ( 93 "2949 Conga2 Lo Open 1-2"))
				    (bongo-macho    . ( 60 "3000 Bongo2 Hi Open 2"))
				    (bongo-hembra   . ( 61 "3004 Bongo2 Lo Open 2"))
				    (timbale-lo     . ( 66 "3042 Timbale2 Lo 2"))
				    (timbale-hi     . ( 65 "3038 Timbale2 Hi 2"))
				    (surdo          . ( 86 "3126 Surdo2 Open"))
				    (cuica-hi       . ( 78 "2975 Cuica1 Hi"))
				    (cuica-lo       . ( 79 "2974 Cuica1 Lo")))))
      (instrument maple-djembe :parent inst
		  :keynumber-map (symbolic-keynumber-map
				  '((lo      . ( 95 "3276 Djembe2 Lo"))
				    (mute    . ( 94 "3278 Djembe2 Mute"))
				    (slap    . ( 96 "3277 Djembe2 Slap")))))
      (instrument maple-hats :parent inst
		  :keynumber-map (symbolic-keynumber-map
				  '((x           . ( 42 "5662 HH Dark Closed 1-4St"))
				    (opn         . ( 44 "5551 HH Open 1-2St"))
				    (open        . ( 46 "5694 HH Dark Open 1-2 St"))
				    (ped         . ( 24 "5682 HH Dark Pedal 1-2 St")))))
      (instrument maple-cym :parent inst
		  :keynumber-map (symbolic-keynumber-map
				  '((ride       . ( 51 "6233 Ride Brite St"))
				    (ride-2     . ( 59 "6241 Ride Warm St"))
				    (cup        . ( 53 "6237 Ride Brite Cup St"))
				    (crash      . ( 49 "6181 Crash Brite St"))
				    (crash-2    . ( 57 "6185 Crash Dark St"))
				    (china      . ( 52 "6288 China3 St"))
				    (splash     . ( 55 "6297 Splash2 St")))))
      (instrument maple-metal :parent inst
		  :keynumber-map (symbolic-keynumber-map
				  '((agogo-hi       . ( 67 "3105 Agogo1 Hi"))
				    (agogo-lo       . ( 68 "3106 Agogo1 Lo"))
				    (cow            . ( 56 "3225 Cowbell3 Sw"))
				    (triangle-mute  . ( 80 "2884 Triangle1 Mute"))
				    (triangle-open  . ( 81 "2883 Triangle1 Open")))))
      (instrument maple-block :parent inst
		  :keynumber-map (symbolic-keynumber-map
				  '((clave          . ( 75 "3218 Claves2"))
				    (block-hi       . ( 76 "2914 Resin Block Small"))
				    (block-lo       . ( 77 "2915 Resin Block Large"))
				    (castanete      . ( 30 "2860 Castanet1")))))
      (instrument maple-shaker :parent inst
		  :keynumber-map (symbolic-keynumber-map
				  '((shaker         . ( 69 "3065 Cabasa1B 1-4"))
				    (shaker-2       . ( 70 "3059 Maracas1 Slur"))
				    (shaker-3       . ( 82 "2820 ShakerB Sw"))
				    (tambourine     . ( 54 "2846 Tambourine2 Sw"))
				    (sleigh-bell    . ( 83 "2909 Sleigh Bell2"))
				    (guiro-short    . ( 73 "3118 MetalGuiro Short 1-2"))
				    (guiro-long     . ( 74 "3121 MetalGuiro Long")))))
      
      inst)))
  
	  
