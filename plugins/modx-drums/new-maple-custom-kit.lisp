;;;; CYCO Yamaha MODX plugin
;;;; new-maple-custom-kit
;;;; May stand in for following kits 'New Oak Kit'
;;;;                                 'Beechwood Snare Kit'

(in-package :modx)

(let* ((general-klist '((hat-x    . ( 42 "5662 HH Dark Closed 1-4St"))
			(hat-opn  . ( 44 "5551 HH Open 1-2St"))
			(hat-open . ( 46 "5694 HH Dark Open 1-2 St"))
			(hat-ped  . ( 24 "5682 HH Dark Pedal 1-2 St"))  ;; C0
			(snare-x            . ( 31 "5061 Sd Oak 2 St"))
			(snare-x2           . ( 38 "5112 Sd Metal 1-4 St"))
			(snare-x3           . ( 40 "5115 Sd Metal 1-OpRm St"))
			(snare-open         . ( 34 "5136 Sd Metal OpRm 2 St"))
			(snare-rim          . ( 32 "2856 Stick2"))
			(snare-rim2         . ( 37 "5139 Sd Metal ClRm St"))
			(snare-brush-soft   . ( 25 "5224 Sd Brush Soft St"))
			(snare-brush-swirl  . ( 26 "5236 Sd Brush Swir St"))
			(snare-brush-med    . ( 27 "5227 Sd Brush Med St"))
			(snare-brush-swirl2 . ( 28 "5239 Sd Brush SwirAtt St"))
			(snare-roll         . ( 29 "5142 Sd Metal SnRoll St   roll loop"))
			(snare-roll2        . ( 39 "5142 Sd Metal SnRoll St   roll decay"))
			(kick-A     . ( 33 "4479 Bd Maple Close 2 L"))
			(kick-B     . ( 35 "4481 Bd Maple Open 1-2 St"))
			(kick-C     . ( 36 "4481 Bd Maple Open 1-2 St"))
			(tom-A      . ( 41 "5978 Tom Maple Flr 1-2 RL"))
			(tom-B      . ( 43 "5983 Tom Maple Flr 2 St"))
			(tom-C      . ( 45 "5971 Tom Maple Mid 2 St"))
			(tom-D      . ( 47 "5963 Tom Maple Mid 1-2 St"))
			(tom-E      . ( 48 "5959 Tom Maple Hi 2 St"))
			(tom-F      . ( 50 "5951 Tom Maple Hi 1-2 St"))
			(cym-ride   . ( 51 "6233 Ride Brite St"))
			(cym-ride2  . ( 59 "6241 Ride Warm St"))
			(cym-cup    . ( 53 "6237 Ride Brite Cup St"))
			(cym-crash  . ( 49 "6181 Crash Brite St"))
			(cym-crash2 . ( 57 "6185 Crash Dark St"))
			(cym-splash . ( 55 "6297 Splash2 St"))
			(cym-china  . ( 52 "6288 China3 St"))
			(bongo-lo   . ( 61 "3004 Bongo2 Lo Open 2"))
			(bongo-hi   . ( 60 "3000 Bongo2 Hi Open 2"))
			(conga-open     . ( 64 "2950 Conga2 Lo Open 1"))
			(conga-heel     . ( 88 "2940 Conga2 Hi Heel"))
			(conga-tip      . ( 87 "2941 Conga2 Hi Tip"))
			(conga-slap     . ( 62 "2944 Conga2 Hi Slap 2"))
			(conga-slap2    . ( 89 "2942 Conga2 Hi Slap 1-2"))
			(conga-mute     . ( 90 "2948 Conga2 Hi Mute"))
			(conga-open2    . ( 63 "2947 Conga2 Hi Open 2"))
			(conga-open3    . ( 91 "2945 Conga2 Hi Open 1-2"))
			(conga-lo-slide . ( 92 "2952 Conga2 Lo Slide"))
			(conga-lo-open  . ( 93 "2949 Conga2 Lo Open 1-2"))
			(timbale-hi     . ( 65 "3038 Timbale2 Hi 2"))
			(timbale-lo     . ( 66 "3042 Timbale2 Lo 2"))
			(agogo-hi       . ( 67 "3105 Agogo1 Hi"))
			(agogo-lo       . ( 68 "3106 Agogo1 Lo"))
			(cow            . ( 56 "3225 Cowbell3 Sw"))		       
			(shaker-x          . ( 82 "2820 ShakerB Sw"))
			(shaker-cabasa     . ( 69 "3065 Cabasa1B 1-4"))
			(shaker-maracas    . ( 70 "3059 Maracas1 Slur"))
			(shaker-tambourine . ( 54 "2846 Tambourine2 Sw"))
			(guiro-short       . ( 73 "3118 MetalGuiro Short 1-2"))
			(guiro-long        . ( 74 "3121 MetalGuiro Long"))
			(block-lo          . ( 77 "2915 Resin Block Large"))
			(block-hi          . ( 76 "2914 Resin Block Small"))
			(block-clave       . ( 75 "3218 Claves2"))
			(block-castanete   . ( 30 "2860 Castanet1"))
			(cuica-hi       . ( 78 "2975 Cuica1 Hi"))
			(cuica-lo       . ( 79 "2974 Cuica1 Lo"))
			(triangle-mute  . ( 80 "2884 Triangle1 Mute"))
			(triangle-open  . ( 81 "2883 Triangle1 Open"))
			(djembe-lo      . ( 95 "3276 Djembe2 Lo"))
			(djembe-slap    . ( 96 "3277 Djembe2 Slap"))
			(djembe-mute    . ( 94 "3278 Djembe2 Mute"))
			(surdo          . ( 86 "3126 Surdo2 Open"))
			(clap           . ( 85 "2889 Hand Clap1 St"))
			(sleigh-bell    . ( 83 "2909 Sleigh Bell2"))
			(wind-chime     . ( 84 "2912 Wind Chime 1"))
			(whistle-hi     . ( 71 "3220 Whistle"))
			(whistle-lo     . ( 72 "3220 Whistle"))
			(vibraslap      . ( 58 "2880 Vibraslap2"))))
       (hat-klist (extract-sub-symbolic-keylist 'hat general-klist))
       (snare-klist (extract-sub-symbolic-keylist 'snare general-klist))
       (kick-klist (extract-sub-symbolic-keylist 'kick general-klist))
       (tom-klist (extract-sub-symbolic-keylist 'tom general-klist))
       (cym-klist (extract-sub-symbolic-keylist 'cym general-klist))
       (bongo-klist (extract-sub-symbolic-keylist 'bongo general-klist))
       (conga-klist (extract-sub-symbolic-keylist 'conga general-klist))
       (timbale-klist (extract-sub-symbolic-keylist 'timbale general-klist))
       (cow-klist (extract-sub-symbolic-keylist 'cow general-klist))
       (shaker-klist (extract-sub-symbolic-keylist 'shaker general-klist))
       (guiro-klist (extract-sub-symbolic-keylist 'guiro general-klist))
       (block-klist (extract-sub-symbolic-keylist 'block general-klist))
       (cuica-klist (extract-sub-symbolic-keylist 'cuica general-klist))
       (triangle-klist (extract-sub-symbolic-keylist 'triangle general-klist))
       (djembe-klist (extract-sub-symbolic-keylist 'djembe general-klist))
       (surdo-klist (extract-sub-symbolic-keylist 'surdo general-klist))
       (nmk (make-main-instrument new-maple-custom-kit general-klist)) )

  (make-sub nmk-hat nmk hat-klist)
  (make-sub nmk-snare nmk snare-klist)
  (make-sub nmk-kick nmk kick-klist)
  (make-sub nmk-tom nmk tom-klist)
  (make-sub nmk-cym nmk cym-klist)
  (make-sub nmk-bongo nmk bongo-klist)
  (make-sub nmk-conga nmk conga-klist)
  (make-sub nmk-timbale nmk timbale-klist)
  (make-sub nmk-cow nmk cow-klist)
  (make-sub nmk-shaker nmk shaker-klist)
  (make-sub nmk-guiro nmk guiro-klist)
  (make-sub nmk-block nmk block-klist)
  (make-sub nmk-cuica nmk cuica-klist)
  (make-sub nmk-triangle nmk triangle-klist)
  (make-sub nmk-djembe nmk djembe-klist)
  (make-sub nmk-surdo nmk surdo-klist))


 (export '(nmk-hat
	   nmk-snare
	   nmk-kick
	   nmk-tom
	   nmk-cym
	   nmk-bongo
	   nmk-conga
	   nmk-timbale
	   nmk-cow
	   nmk-shaker
	   nmk-guiro
	   nmk-block
	   nmk-cuica
	   nmk-triangle
	   nmk-djembe
	   nmk-surdo) :modx)

(import '(modx:nmk-hat
	  modx:nmk-snare
	  modx:nmk-kick
	  modx:nmk-tom
	  modx:nmk-cym
	  modx:nmk-bongo
	  modx:nmk-conga
	  modx:nmk-timbale
	  modx:nmk-cow
	  modx:nmk-shaker
	  modx:nmk-guiro
	  modx:nmk-block
	  modx:nmk-cuica
	  modx:nmk-triangle
	  modx:nmk-djembe
	  modx:nmk-surdo) :cyco)

