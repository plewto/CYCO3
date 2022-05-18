;;;; CYCO MODX drum kits   'T3 Uber Funk'
;;;;
;;;; 24 C0  4286 Analog Click          :
;;;; 25 C#0 5224 Sd Brush Soft St      :
;;;; 26 D0  5236 Sd Brush Swir St      :
;;;; 27 D#0 5227 Sd Brush Med St       :
;;;; 28 E0  5239 Sd Brush SwirAtt St   :
;;;; 29 F0  5206 Sd RockRoll St        :
;;;; 30 F#0 2860 Castanet1             :
;;;; 31 G0  4947 Sd Rock Open2 St      :
;;;; 32 G#0 2855 Stick1                :
;;;; 33 A0  4440 Bd Room3              :
;;;; 34 A#0 4944 Sd Rock Rim1 St       :
;;;; 35 B0  4430 Bd Rock2 St           :
;;;; 36 C1  4427 Bd Rock1 St           :
;;;; 37 C#1 5182 Sd Rock Stick1 St     :
;;;; 38 D1  4938 Sd Rock Open1 St      :
;;;; 39 D#1 4213 HipHop Clap3          :
;;;; 40 E1  4941 Sd Rock Mute1 St      :
;;;; 41 F1  5899 Tom Rock Low St       :
;;;; 42 F#1 5575 HH Closed Rock Sw St  :
;;;; 43 G1  5899 Tom Rock Low St       :
;;;; 44 G#1 5587 HH Pedal Rock Sw St   :
;;;; 45 A1  5895 Tom Rock Mid St       :
;;;; 46 A#1 5599 HH Open Rock Sw St    :
;;;; 47 B1  5895 Tom Rock Mid St       :
;;;; 48 C2  5891 Tom Rock Hi St        :
;;;; 49 C#2 6174 Crash1 RL             :
;;;; 50 D2  5891 Tom Rock Hi St        :
;;;; 51 D#2 6229 Ride2 St              :
;;;; 52 E2  6283 China1 St             :
;;;; 53 F2  6276 Ride Cup2 St          :
;;;; 54 F#2 4258 Tambourine RX5        :
;;;; 55 G2  6293 Splash1 St            :
;;;; 56 G#2 3223 Cowbell1              :
;;;; 57 A2  6171 Crash1 St             :
;;;; 58 A#2 2879 Vibraslap1            :
;;;; 59 B2  6232 Ride2 RL              :
;;;; 60 C3  2979 Bongo1 Hi 1Fingr 1-2  :
;;;; 61 C#3 2992 Bongo1 Lo 3Fingr 1-2  :
;;;; 62 D3  2922 Conga1 Hi Tip         :
;;;; 63 D#3 2926 Conga1 Hi Open 2      :
;;;; 64 E3  2970 Tumba Open            :
;;;; 65 F3  3026 Timbale1 Hi 1-3       :
;;;; 66 F#3 3030 Timbale1 Lo 1-2       :
;;;; 67 G3  3105 Agogo1 Hi             :
;;;; 68 G#3 3106 Agogo1 Lo             :
;;;; 69 A3  3062 Cabasa1A 1-2          :
;;;; 70 A#3 3059 Maracas1 Slur         :
;;;; 71 B3  3220 Whistle               :
;;;; 72 C4  3220 Whistle               :
;;;; 73 C#4 3113 Guiro1 Short          :
;;;; 74 D4  3112 Guiro1 Long           :
;;;; 75 D#4 3217 Claves1               :
;;;; 76 E4  2872 Wood Block            :
;;;; 77 F4  2872 Wood Block            :
;;;; 78 F#4 2975 Cuica1 Hi             :
;;;; 79 G4  2974 Cuica1 Lo             :
;;;; 80 G#4 2884 Triangle1 Mute        :
;;;; 81 A4  2883 Triangle1 Open        :
;;;; 82 A#4 2817 ShakerA 1             :
;;;; 83 B4  2908 Sleigh Bell1          :
;;;; 84 C5  2912 Wind Chime 1          :
;;;; 85 C#5 4956 Sd Rock Flam St       :
;;;; 86 D5  4947 Sd Rock Open2 St      :
;;;; 87 D#5 4953 Sd Rock Rim2 St       :
;;;; 88 E5  4950 Sd Rock Mute2 St      :
;;;; 89 F5  4404 Bd Funk 1St           :
;;;; 90 F#5 5209 Sd RockRollD St       :
;;;; 91 G5  4418 Bd H Son St           :
;;;; 92 G#5 4909 Sd Tight St1-4        :
;;;; 93 A5  4424 Bd Funk Son St        :
;;;; 94 A#5 4867 Sd SonA 3St           :
;;;; 95 B5  4407 Bd Funk R1-2          :
;;;; 96 C6  4418 Bd H Son S            :

(in-package :modx)

(let* ((general-klist '((KICK-A . (33 "4440 Bd Room3"))
			(KICK-B . (35 "4430 Bd Rock2 St"))
			(KICK-C . (36 "4427 Bd Rock1 St"))
			(KICK-D . (89 "4404 Bd Funk 1St"))
			(KICK-E . (91 "4418 Bd H Son St"))
			(KICK-F . (93 "4424 Bd Funk Son St"))
			(KICK-G . (95 "4407 Bd Funk R1-2"))
			(KICK-H . (96 "4418 Bd H Son S"))
			(SNARE-X       . (31 "4947 Sd Rock Open2 St"))
			(SNARE-OPEN    . (38 "4938 Sd Rock Open1 St"))
			(SNARE-MUTE    . (40 "4941 Sd Rock Mute1 St"))
			(SNARE-RIM     . (34 "4944 Sd Rock Rim1 St"))
			(SNARE-STICK   . (37 "5182 Sd Rock Stick1 St"))
			(SNARE-OPEN2   . (86 "4947 Sd Rock Open2 St"))
			(SNARE-FLAM    . (85 "4956 Sd Rock Flam St"))
			(SNARE-RIM2    . (87 "4953 Sd Rock Rim2 St"))
			(SNARE-MUTE2   . (88 "4950 Sd Rock Mute2 St"))
			(SNARE-TIGHT   . (92 "4909 Sd Tight St1-4"))
			(SNARE-SON     . (94 "4867 Sd SonA 3St"))
			(SNARE-BRUSH   . (25 "5224 Sd Brush Soft St"))
			(SNARE-BRUSH2  . (27 "5227 Sd Brush Med St"))
			(SNARE-BRUSH3  . (26 "5236 Sd Brush Swir St"))
			(SNARE-BRUSH4  . (28 "5239 Sd Brush SwirAtt St"))
			(SNARE-ROLL    . (29 "5206 Sd RockRoll St"))
			(SNARE-ROLL2   . (90 "5209 Sd RockRollD St"))
			(TOM-A . (41 "5899 Tom Rock Low St"))
			(TOM-B . (43 "5899 Tom Rock Low St"))
			(TOM-C . (45 "5895 Tom Rock Mid St"))
			(TOM-D . (47 "5895 Tom Rock Mid St"))
			(TOM-E . (48 "5891 Tom Rock Hi St"))
			(TOM-F . (50 "5891 Tom Rock Hi St"))
			(CONGA       . (63 "2926 Conga1 Hi Open 2"))
			(CONGA-TIP   . (62 "2922 Conga1 Hi Tip"))
			(CONGA-TUMBA . (64 "2970 Tumba Open"))
			(BONGO-LO . (61 "2992 Bongo1 Lo 3Fingr 1-2"))
			(BONGO-HI . (60 "2979 Bongo1 Hi 1Fingr 1-2"))
			(TIMBALE-LO . (66 "3030 Timbale1 Lo 1-2"))
			(TIMBALE-HI . (65 "3026 Timbale1 Hi 1-3"))
			(HAT-X    . (42 "5575 HH Closed Rock Sw St"))
			(HAT-OPEN . (46 "5599 HH Open Rock Sw St"))
			(HAT-PED  . (44 "5587 HH Pedal Rock Sw St"))
			(cym-RIDE   . (51 "6229 Ride2 St"))
			(cym-RIDE2  . (59 "6232 Ride2 RL"))
			(cym-CUP    . (53 "6276 Ride Cup2 St"))
			(cym-CRASH  . (49 "6174 Crash1 RL"))
			(cym-CRASH2 . (57 "6171 Crash1 St"))
			(cym-CHINA  . (52 "6283 China1 St"))
			(cym-SPLASH . (55 "6293 Splash1 St"))
			(COW . (56 "3223 Cowbell1"))
			(AGOGO-LO . (68 "3106 Agogo1 Lo"))
			(AGOGO-HI . (67 "3105 Agogo1 Hi"))
			(TRIANGLE-OPEN . (81 "2883 Triangle1 Open"))
			(TRIANGLE-MUTE . (80 "2884 Triangle1 Mute"))
			(CLAP . (39 "4213 HipHop Clap3"))
			(CLAVE       . (75 "3217 Claves1"))
			(CLAVE-CLICK . (24 "4286 Analog Click"))
			(CLAVE-STICK . (32 "2855 Stick1"))
			(BLOCK    . (76 "2872 Wood Block"))
			(BLOCK-2  . (77 "2872 Wood Block"))
			(BLOCK-CASTANET . (30 "2860 Castanet1"))
			(GUIRO-SHORT . (73 "3113 Guiro1 Short"))
			(GUIRO-LONG  . (74 "3112 Guiro1 Long"))
			(VIBRASLAP   . (58 "2879 Vibraslap1"))
			(SHAKER            . (82 "2817 ShakerA 1"))
			(SHAKER-CABASA     . (69 "3062 Cabasa1A 1-2"))
			(SHAKER-MARACAS    . (70 "3059 Maracas1 Slur"))
			(SHAKER-TAMBOURINE . (54 "4258 Tambourine RX5"))
			(SHAKER-SLEIGH     . (83 "2908 Sleigh Bell1"))))
       (kick-klist (extract-sub-symbolic-keylist 'kick general-klist))
       (snare-klist (extract-sub-symbolic-keylist 'snare general-klist))
       (tom-klist (extract-sub-symbolic-keylist 'tom general-klist))
       (conga-klist (extract-sub-symbolic-keylist 'conga general-klist))
       (bongo-klist (extract-sub-symbolic-keylist 'bongo general-klist))
       (timbale-klist (extract-sub-symbolic-keylist 'timbale general-klist))
       (hat-klist (extract-sub-symbolic-keylist 'hat general-klist))
       (cym-klist (extract-sub-symbolic-keylist 'cym general-klist))
       (cow-klist (extract-sub-symbolic-keylist 'cow general-klist))
       (agogo-klist (extract-sub-symbolic-keylist 'agogo general-klist))
       (triangle-klist (extract-sub-symbolic-keylist 'triangle general-klist))
       (clap-klist (extract-sub-symbolic-keylist 'clap general-klist))
       (clave-klist (extract-sub-symbolic-keylist 'clave general-klist))
       (block-klist (extract-sub-symbolic-keylist 'block general-klist))
       (guiro-klist (extract-sub-symbolic-keylist 'guiro general-klist))
       (shaker-klist (extract-sub-symbolic-keylist 'shaker general-klist))
       (t3uf (make-main-instrument t3-uber-funk general-klist)) )
  (make-sub t3uf-kick t3uf kick-klist)
  (make-sub t3uf-snare t3uf snare-klist)
  (make-sub t3uf-tom t3uf tom-klist)
  (make-sub t3uf-conga t3uf conga-klist)
  (make-sub t3uf-bongo t3uf bongo-klist)
  (make-sub t3uf-timbale t3uf timbale-klist)
  (make-sub t3uf-hat t3uf hat-klist)
  (make-sub t3uf-cym t3uf cym-klist)
  (make-sub t3uf-cow t3uf cow-klist)
  (make-sub t3uf-agogo t3uf agogo-klist)
  (make-sub t3uf-triangle t3uf triangle-klist)
  (make-sub t3uf-clap t3uf clap-klist)
  (make-sub t3uf-clave t3uf clave-klist)
  (make-sub t3uf-block t3uf block-klist)
  (make-sub t3uf-guiro t3uf guiro-klist)
  (make-sub t3uf-shaker t3uf shaker-klist))

(export '(t3uf-kick
	  t3uf-snare
	  t3uf-tom
	  t3uf-conga
	  t3uf-bongo
	  t3uf-timbale
	  t3uf-hat
	  t3uf-cym
	  t3uf-cow
	  t3uf-agogo
	  t3uf-triangle
	  t3uf-clap
	  t3uf-clave
	  t3uf-block
	  t3uf-guiro
	  t3uf-shaker) :modx)

(import '(modx:t3uf-kick
	  modx:t3uf-snare
	  modx:t3uf-tom
	  modx:t3uf-conga
	  modx:t3uf-bongo
	  modx:t3uf-timbale
	  modx:t3uf-hat
	  modx:t3uf-cym
	  modx:t3uf-cow
	  modx:t3uf-agogo
	  modx:t3uf-triangle
	  modx:t3uf-clap
	  modx:t3uf-clave
	  modx:t3uf-block
	  modx:t3uf-guiro
	  modx:t3uf-shaker) :cyco)
