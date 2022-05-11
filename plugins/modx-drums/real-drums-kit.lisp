;;;; CYCO MODX drumkit   real-drums-kit
;;;;
;;;; Modified general MIDI key assignments
;;;;
;;;; 24 3217 Claves1              :
;;;; 25 5224 Sd Brush Soft St     :
;;;; 26 5236 Sd Brush Swir St     :
;;;; 27 5227 Sd Brush Med St      :
;;;; 28 5239 Sd Brush SwirAtt St  :
;;;; 29 5223 Bright Snare Roll    :
;;;; 30 2860 Castanet1            :
;;;; 31 5219 LW Snare Sw          :
;;;; 32 2855 Stick1               :
;;;; 33 4551 AT Kick Sw           :
;;;; 34 5220 DG Snare1 Sw         :
;;;; 35 4552 GT Kick1 Sw          : 
;;;; 36 4553 GT Kick2 Sw          : 
;;;; 37 5204 Sd Closed Rim5       : 
;;;; 38 5221 DG Snare2 Sw         : 
;;;; 39 2895 Hand Clap3 Sw        : 
;;;; 40 5222 TM Snare Sw          : 
;;;; 41 6069 CP Tom Floor Sw      : 
;;;; 42 5757 PT HiHat Closed      : 
;;;; 43 6069 CP Tom Floor Sw      : 
;;;; 44 5759 PT HiHat Pedal       : 
;;;; 45 6068 CP Tom Low Sw        : 
;;;; 46 5758 PT HiHat Open        : 
;;;; 47 6067 CP Tom Mid Sw        : 
;;;; 48 6066 CP Tom High Sw       : 
;;;; 49 6201 ZJ Crash1 Sw         : 
;;;; 50 6066 CP Tom High Sw       : 
;;;; 51 6258 ZJ Ride1 Tip Sw      : 
;;;; 52 6202 ZJ Crash2 Sw         : 
;;;; 53 6259 ZJ Ride1 Cup Sw      : 
;;;; 54 2843 Tambourine1 Sw       : 
;;;; 55 6301 ZJ Splash1           : 
;;;; 56 3229 Cowbell5             : 
;;;; 57 6203 ZJ Crash3 Sw         : 
;;;; 58 2879 Vibraslap1           : 
;;;; 59 6260 ZJ Ride2 Tip Sw      : 
;;;; 60 2979 Bongo1 Hi 1Fingr 1-2 : 
;;;; 61 2989 Bongo1 Lo 1Fingr 1-2 : 
;;;; 62 2930 Conga1 Hi SlpMute1-2 : 
;;;; 63 2924 Conga1 Hi Open 1-2   : 
;;;; 64 2935 Conga1 Lo Open 1-2   : 
;;;; 65 3026 Timbale1 Hi 1-3      : 
;;;; 66 3030 Timbale1 Lo 1-2      : 
;;;; 67 3105 Agogo1 Hi            : 
;;;; 68 3106 Agogo1 Lo            : 
;;;; 69 3062 Cabasa1A 1-2         : 
;;;; 70 3059 Maracas1 Slur        : 
;;;; 71 3220 Whistle              : 
;;;; 72 3220 Whistle              : 
;;;; 73 3113 Guiro1 Short         : 
;;;; 74 3112 Guiro1 Long          : 
;;;; 75 3217 Claves1              : 
;;;; 76 2872 Wood Block           : 
;;;; 77 2872 Wood Block           : 
;;;; 78 2975 Cuica1 Hi            : 
;;;; 79 2974 Cuica1 Lo            : 
;;;; 80 2884 Triangle1 Mute       : 
;;;; 81 2883 Triangle1 Open       : 
;;;; 82 2816 ShakerA 13           :
;;;; 83 2908 Sleigh Bell1         :
;;;; 84 2912 Wind Chime 1         :
;;;; 85 4726 Sd Bld St1-4         : 
;;;; 86 4885 Sd Blues 3St         :
;;;; 87 4929 Sd Hip St1-2         :
;;;; 88 4826 Sd Funk St1-3        :
;;;; 89 4746 Sd Soul St1-4        :
;;;; 90 5177 Sd LdwHMono          :
;;;; 91 5180 Sd Heavy             :
;;;; 92 4909 Sd Tight St1-4       :
;;;; 93 4894 Sd BluesHeart        :
;;;; 94 4367 Bd Snap 1-2          :
;;;; 95 4376 Bd Tight 1-2         :
;;;; 96 4403 Bd Funk St1-2        :

(in-package :modx)

(let* ((general-klist '((KICK-A     . (33 "4551 AT Kick S"))
			(KICK-B     . (35 "4552 GT Kick1 S"))
			(KICK-C     . (36 "4553 GT Kick2 S"))
			(KICK-SNAP  . (94 "4367 Bd Snap 1-2"))
			(KICK-TIGHT . (95 "4376 Bd Tight 1-2"))
			(KICK-FUNK  . (96 "4403 Bd Funk St1-2"))
			(SNARE-A . (37 "5204 Sd Closed R"))
			(SNARE-B . (31 "5219 LW Snare S"))
			(SNARE-C . (34 "5220 DG Snare1 S"))
			(SNARE-D . (38 "5221 DG Snare2 S"))
			(SNARE-E . (40 "5222 TM Snare S"))
			(SNARE-F . (85 "4726 Sd Bld St1-4"))
			(SNARE-G     . (90 "5177 Sd L"))
			(SNARE-H     . (91 "5180 Sd H"))
			(SNARE-I     . (93 "4894 Sd B"))
			(SNARE-BRIGHT . (29 "5223 Bright Snare R"))
			(SNARE-BLUES . (86 "4885 Sd Blues 3"))
			(SNARE-HIP   . (87 "4929 Sd Hip St1-2"))
			(SNARE-FUNK  . (88 "4826 Sd Funk St1-3"))
			(SNARE-SOUL  . (89 "4746 Sd Soul St1-4"))
			(SNARE-TIGHT . (92 "4909 Sd Tight St1-4"))
			(SNARE-STICK . (32 "2855 Stick"))
			(SNARE-CLAP  . (39 "2895 Hand Clap3 S"))
			(SNARE-BRUSH        . (27 "5227 Sd Brush Med S"))
			(SNARE-BRUSH-SOFT   . (25 "5224 Sd Brush Soft S"))
			(SNARE-BRUSH-SWIRL  . (26 "5236 Sd Brush Swir S"))
			(SNARE-BRUSH-SWIRL2 . (28 "5239 Sd Brush SwirAtt S"))
			(TOM-A . (41 "6069 CP Tom Floor S"))
			(TOM-B . (43 "6069 CP Tom Floor S"))
			(TOM-C . (45 "6068 CP Tom Low S"))
			(TOM-D . (47 "6067 CP Tom Mid S"))
			(TOM-E . (48 "6066 CP Tom High S"))
			(TOM-F . (50 "6066 CP Tom High S"))
			(HAT-X    . (42 "5757 PT HiHat C"))
			(HAT-OPEN . (46 "5758 PT HiHat O"))
			(HAT-PED  . (44 "5759 PT HiHat P"))
			(CYM-RIDE     . (51 "6258 ZJ Ride1 Tip S"))
			(CYM-RIDE2    . (59 "6260 ZJ Ride2 Tip S"))
			(CYM-RIDE-CUP . (53 "6259 ZJ Ride1 Cup S"))
			(CYM-CRASH    . (49 "6201 ZJ Crash1 S"))
			(CYM-CRASH2   . (52 "6202 ZJ Crash2 S"))
			(CYM-CRASH3   . (57 "6203 ZJ Crash3 S"))
			(CYM-SPLASH   . (55 "6301 ZJ Splash"))
			(BONGO-LO   . (61 "2989 Bongo1 Lo 1Fingr 1-2"))
			(BONGO-HI   . (60 "2979 Bongo1 Hi 1Fingr 1-2"))
			(CONGA-LO   . (64 "2935 Conga1 Lo Open 1-2"))
			(CONGA-HI   . (63 "2924 Conga1 Hi Open 1-2"))
			(CONGA-SLAP . (62 "2930 Conga1 Hi SlpMute1-2"))
			(TIMBAL-LO  . (66 "3030 Timbale1 Lo 1-2"))
			(TIMBAL-HI  . (65 "3026 Timbale1 Hi 1-3"))
			(SHAKER-X          . (82 "2816 ShakerA 1"))
			(SHAKER-CABASA     . (69 "3062 Cabasa1A 1-2"))
			(SHAKER-MARACAS    . (70 "3059 Maracas1 S"))
			(SHAKER-TAMBOURINE . (54 "2843 Tambourine1 S"))
			(SHAKER-BELLS      . (83 "2908 Sleigh B"))
			(SHAKER-CHIMES     . (84 "2912 Wind Chime 1"))
			(BLOCK-A         . (77 "2872 Wood B"))
			(BLOCK-B         . (76 "2872 Wood B"))
			(BLOCK-CLAVE     . (75 "3217 Claves1"))
			(BLOCK-CLAVE2    . (24 "3217 Claves1"))
			(BLOCK-CASTANETE . (30 "2860 Castanete"))
			(COW       . (56 "3229 Cowbell5"))
			(VIBRASLAP . (58 "2879 Vibraslap1"))
			(AGOGO-LO  . (68 "3106 Agogo1 Lo"))
			(AGOGO-HI  . (67 "3105 Agogo1 Hi"))
			(WHISTLE-A . (71 "3220 Whistle"))
			(WHISTLE-B . (72 "3220 Whistle"))
			(GUIRO-SHORT . (73 "3113 Guiro1 Short"))
			(GUIRO-LONG  . (74 "3112 Guiro1 Long"))
			(CUICA-LO . (79 "2974 Cuica1 Lo"))
			(CUICA-HI . (78 "2975 Cuica1 Hi"))
			(TRIANGLE-OPEN . (81 "2883 Triangle1 Open"))
			(TRIANGLE-MUTE . (80 "2884 Triangle1 Mute"))))
       (kick-klist (extract-sub-symbolic-keylist 'kick general-klist))
       (snare-klist (extract-sub-symbolic-keylist 'snare general-klist))
       (tom-klist (extract-sub-symbolic-keylist 'tom general-klist))
       (hat-klist (extract-sub-symbolic-keylist 'hat general-klist))
       (cym-klist (extract-sub-symbolic-keylist 'cym general-klist))
       (bongo-klist (extract-sub-symbolic-keylist 'bongo general-klist))
       (conga-klist (extract-sub-symbolic-keylist 'conga general-klist))
       (timbale-klist (extract-sub-symbolic-keylist 'timbale general-klist))
       (shaker-klist (extract-sub-symbolic-keylist 'shaker general-klist))
       (block-klist (extract-sub-symbolic-keylist 'block general-klist))
       (cow-klist (extract-sub-symbolic-keylist 'cow general-klist))
       (agogo-klist (extract-sub-symbolic-keylist 'agogo general-klist))
       (vibraslap-klist (extract-sub-symbolic-keylist 'vibraslap general-klist))
       (whistle-klist (extract-sub-symbolic-keylist 'whistle general-klist))
       (guiro-klist (extract-sub-symbolic-keylist 'guiro general-klist))
       (cuica-klist (extract-sub-symbolic-keylist 'cuica general-klist))
       (triangle-klist (extract-sub-symbolic-keylist 'triangle general-klist))
       (rdk (make-main-instrument real-drums-kit general-klist)))
  (make-sub rdk-kick rdk kick-klist)
  (make-sub rdk-snare rdk snare-klist)
  (make-sub rdk-tom rdk tom-klist)
  (make-sub rdk-hat rdk hat-klist)
  (make-sub rdk-cym rdk cym-klist)
  (make-sub rdk-bongo rdk bongo-klist)
  (make-sub rdk-conga rdk conga-klist)
  (make-sub rdk-timbale rdk timbale-klist)
  (make-sub rdk-shaker rdk shaker-klist)
  (make-sub rdk-block rdk block-klist)
  (make-sub rdk-cow rdk cow-klist)
  (make-sub rdk-agogo rdk agogo-klist)
  (make-sub rdk-vibraslap rdk vibraslap-klist)
  (make-sub rdk-whistle rdk whistle-klist)
  (make-sub rdk-guiro rdk guiro-klist)
  (make-sub rdk-cuica rdk cuica-klist)
  (make-sub rdk-triangle rdk triangle-klist))


(export '(rdk-kick
	  rdk-snare
	  rdk-tom
	  rdk-hat
	  rdk-cym
	  rdk-bongo
	  rdk-conga
	  rdk-timbale
	  rdk-shaker
	  rdk-block
	  rdk-cow
	  rdk-agogo
	  rdk-vibraslap
	  rdk-whistle
	  rdk-guiro
	  rdk-cuica
	  rdk-triangle) :modx)

(import '(modx:rdk-kick
	  modx:rdk-snare
	  modx:rdk-tom
	  modx:rdk-hat
	  modx:rdk-cym
	  modx:rdk-bongo
	  modx:rdk-conga
	  modx:rdk-timbale
	  modx:rdk-shaker
	  modx:rdk-block
	  modx:rdk-cow
	  modx:rdk-agogo
	  modx:rdk-vibraslap
	  modx:rdk-whistle
	  modx:rdk-guiro
	  modx:rdk-cuica
	  modx:rdk-triangle) :cyco)
