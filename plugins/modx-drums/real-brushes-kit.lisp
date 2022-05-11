;;;; CYCO Yamaha MODX plugin
;;;; real-brushes-kit

(in-package :modx)


(let* ((general-klist '((agogo-ho       . ( 67 "3105 Agogo1 Hi"))
			(agogo-lo       . ( 68 "3106 Agogo1 Lo"))
			(block-lo       . ( 77 "2872 Wood Block"))
			(block-hi       . ( 76 "2872 Wood Block"))
			(block-clave    . ( 75 "3217 Claves1"))
			(bongo-lo       . ( 61 "2989 Bongo1 Lo 1Fingr 1-2"))
			(bongo-hi       . ( 60 "2979 Bongo1 Hi 1Fingr 1-2"))
			(clap           . ( 39 "2895 Hand Clap3 Sw"))
			(conga-lo       . ( 64 "2935 Conga1 Lo Open 1-2"))
			(conga-hi       . ( 63 "2924 Conga1 Hi Open 1-2"))
			(conga-mute     . ( 62 "2930 Conga1 Hi SlpMute1-2"))
			(cow            . ( 56 "3229 Cowbell5"))
			(cuica-lo       . ( 79 "2974 Cuica1 Lo"))
			(cuica-hi       . ( 78 "2975 Cuica1 Hi"))
			(cym-ride      . ( 51 "6273 Ride Brush3 Tip Sw"))
			(cym-ride2     . ( 53 "6274 Ride Brush3 Cup Sw"))
			(cym-ride3     . ( 59 "6275 Ride Brush4 Cup Sw"))
			(cym-crash     . ( 49 "6204 Jazz Crash1 Sw"))
			(cym-crash2    . ( 52 "6218 ZJ Crash Brush Sw"))
			(cym-crash3    . ( 57 "6205 Jazz Crash2 Sw"))
			(cym-splash    . ( 55 "6302 ZJ Splash2 Sw"))
			(guiro-long     . ( 74 "3112 Guiro1 Long"))
			(guiro-short    . ( 73 "3113 Guiro1 Short"))
			(hat-x           . ( 42 "5763 HH IB Brush Cls"))
			(hat-open        . ( 46 "5764 HH IB Brush Opn"))
			(hat-ped         . ( 44 "5765 HH IB Brush Pdl"))
			(kick-a          . ( 33 "4554 PL Kick1 Sw"))
			(kick-b          . ( 35 "4555 PL Kick2 Sw"))
			(kick-c          . ( 36 "4453 Bd PL Jazz HiSna Sw"))
			(kick-ambiant    . ( 94 "4367 Bd Snap 1-2"))
			(kick-ambiant2   . ( 95 "4376 Bd Tight 1-2"))
			(kick-ambiant3   . ( 96 "4403 Bd Funk St1-2"))
			(shaker            . ( 82 "2816 ShakerA 13"))
			(shaker-cabasa     . ( 69 "3062 Cabasa1A 1-2"))
			(shaker-maracas    . ( 70 "3059 Maracas1 Slur"))
			(shaker-tambourine . ( 54 "2843 Tambourine1 Sw"))
			(shaker-bells      . ( 83 "2908 Sleigh Bell1"))
			(shaker-chimes     . ( 84 "2912 Wind Chime 1"))
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
			(whistle-lo     . ( 71 "3220 Whistle"))))
       (agogo-klist (extract-sub-symbolic-keylist 'agogo general-klist))
       (block-klist (extract-sub-symbolic-keylist 'block general-klist))
       (bongo-klist (extract-sub-symbolic-keylist 'bongo general-klist))
       (conga-klist (extract-sub-symbolic-keylist 'conga general-klist))
       (clap-klist (extract-sub-symbolic-keylist 'clap general-klist))
       (cow-klist (extract-sub-symbolic-keylist 'cow general-klist))
       (cuica-klist (extract-sub-symbolic-keylist 'cuica general-klist))
       (cym-klist (extract-sub-symbolic-keylist 'cym general-klist))
       (guiro-klist (extract-sub-symbolic-keylist 'guiro general-klist))
       (hat-klist (extract-sub-symbolic-keylist 'hat general-klist))
       (kick-klist (extract-sub-symbolic-keylist 'kick general-klist))
       (shaker-klist (extract-sub-symbolic-keylist 'shaker general-klist))
       (snare-klist (extract-sub-symbolic-keylist 'snare general-klist))
       (tom-klist (extract-sub-symbolic-keylist 'tom general-klist))
       (triangle-klist (extract-sub-symbolic-keylist 'triangle general-klist))
       (rbk (make-main-instrument real-brushes-kit general-klist)) )
  (make-sub rbk-agogo rbk agogo-klist)
  (make-sub rbk-block rbk block-klist)
  (make-sub rbk-bongo rbk bongo-klist)
  (make-sub rbk-conga rbk conga-klist)
  (make-sub rbk-clap rbk clap-klist)
  (make-sub rbk-cow rbk cow-klist)
  (make-sub rbk-cuica rbk cuica-klist)
  (make-sub rbk-cym rbk cym-klist)
  (make-sub rbk-guiro rbk guiro-klist)
  (make-sub rbk-hat rbk hat-klist)
  (make-sub rbk-kick rbk kick-klist)
  (make-sub rbk-shaker rbk shaker-klist)
  (make-sub rbk-snare rbk snare-klist)
  (make-sub rbk-tom rbk tom-klist)
  (make-sub rbk-triangle rbk triangle-klist))

(export '(rbk-agogo
	  rbk-block
	  rbk-bongo
	  rbk-conga
	  rbk-clap
	  rbk-cow
	  rbk-cuica
	  rbk-cym
	  rbk-guiro
	  rbk-hat
	  rbk-kick
	  rbk-shaker
	  rbk-snare
	  rbk-tom
	  rbk-triangle) :modx)

(import '(modx:rbk-agogo
	  modx:rbk-block
	  modx:rbk-bongo
	  modx:rbk-conga
	  modx:rbk-clap
	  modx:rbk-cow
	  modx:rbk-cuica
	  modx:rbk-cym
	  modx:rbk-guiro
	  modx:rbk-hat
	  modx:rbk-kick
	  modx:rbk-shaker
	  modx:rbk-snare
	  modx:rbk-tom
	  modx:rbk-triangle) :cyco)
