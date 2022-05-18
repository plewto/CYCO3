;;;; CYCO modx-drums plugin BRAZIL-KIT
;;;; May be used for 'Brazil-kit-1' or 'Brazil-kit-2'
;;;;
;;;; 24 C0  3246 Repique2 St Centr    :
;;;; 25 C#0 3249 Repique2 St RmSht    :
;;;; 26 D0  3247 Repique2 St Hd Slap  :
;;;; 27 D#0 3248 Repique2 St Rim      :
;;;; 28 E0  3250 Repique3 St Centr    :
;;;; 29 F0  3251 Repique3 St RmSht    :
;;;; 30 F#0 3148 Zabmba Mute RH       :
;;;; 31 G0  3147 Zabmba Open RH       :
;;;; 32 G#0 3146 Zabmba STick LH      :
;;;; 33 A0  3136 Surdo5-1 Mute        :
;;;; 34 A#0 3135 Surdo5-1 Open        :
;;;; 35 B0  3134 Surdo5-1 Stop        :
;;;; 36 C1  3137 Surdo5-2 Sdstck      :
;;;; 37 C#1 5313 Caixa1 RimShot       :
;;;; 38 D1  5312 Caixa1 Center        :
;;;; 39 D#1 3085 Tumbo5 Rim Close     :
;;;; 40 E1  3084 Tumbo5 Center Open   :
;;;; 41 F1  3083 Tumbo5 Finger Back   :
;;;; 42 F#1 3082 Tumbo4 Down          :
;;;; 43 G1  3081 Tumbo4 Up            :
;;;; 44 G#1 3133 Surdo4 Mute          :
;;;; 45 A1  3132 Surdo4 Open          :
;;;; 46 A#1 3131 Surdo4 Stop          :
;;;; 47 B1  3130 Surdo3 Mute          :
;;;; 48 C2  3129 Surdo3 Open          :
;;;; 49 C#2 3128 Surdo3 Stop          :
;;;; 50 D2  3145 Tntan2 Close RH      :
;;;; 51 D#2 3144 Tntan2 Open RH       :
;;;; 52 E2  3143 Tntan2 Slap RH       :
;;;; 53 F2  3142 Tntan2 Body LH       :
;;;; 54 F#2 3094 Pandeiro2 LThmb Cls  :
;;;; 55 G2  3093 Pandeiro2 LThmb Opn  :
;;;; 56 G#2 3091 Pandeiro2 LToe Rim   :
;;;; 57 A2  3090 Pandeiro2 LHeel      :
;;;; 58 A#2 3092 Pandeiro2 LSlap      :
;;;; 59 B2  3095 Pandeiro2 LRoll      :
;;;; 60 C3  3122 Reco1                :
;;;; 61 C#3 2911 Chocal Forward       :
;;;; 62 D3  3076 Caxixi 3             :
;;;; 63 D#3 3047 Timbale3 Hi Rim      :
;;;; 64 E3  3046 Timbale3 Lo Rim      :
;;;; 65 F3  3045 Timbale3 Hi          :
;;;; 66 F#3 3044 Timbale3 Lo          :
;;;; 67 G3  3108 Agogo2 Bell1         :
;;;; 68 G#3 3109 Agogo2 Bell2         :
;;;; 69 A3  3110 Agogo3 Click         :
;;;; 70 A#3 3111 Agogo4 Click         :
;;;; 71 B3  3070 Cabasa2              :
;;;; 72 C4  3056 Ganza2 Forward1      :
;;;; 73 C#4 3255 Repique4 Anel Rim LH :
;;;; 74 D4  3254 Repique4 Anel Rim RH :
;;;; 75 D#4 3253 Repique4 Anel Mute   :
;;;; 76 E4  3252 Repique4 Anel Open   :
;;;; 77 F4  2978 Cuica2 Hi            :
;;;; 78 F#4 2977 Cuica2 Med           :
;;;; 79 G4  2976 Cuica2 Low           :
;;;; 80 G#4 2886 Triangle2 Mute       :
;;;; 81 A4  2885 Triangle2 Open       :
;;;; 82 A#4 3258 Repique5 Mao Slap    :
;;;; 83 B4  3257 Repique5 Mao Rim     :
;;;; 84 C5  3256 Repique5 Mao Open    :
;;;; 85 C#5 3222 Apito Hi Short       :
;;;; 86 D5  3221 Apito Lo Short       :

(in-package :modx)

(let* ((general-klist '((repique-center    . (24 "3246 Repique2 St Centr"))
			(repique-rim       . (25 "3249 Repique2 St RmSht"))
			(repique-slap      . (26 "3247 Repique2 St Hd Slap"))
			(repique-rim2      . (27 "3248 Repique2 St Rim"))
			(repique-center2   . (28 "3250 Repique3 St Centr"))
			(repique-rim3      . (29 "3251 Repique3 St RmSht"))
			(repique-anel-lh   . (73 "3255 Repique4 Anel Rim LH"))
			(repique-anel-rh   . (74 "3254 Repique4 Anel Rim RH"))
			(repique-anel-mute . (75 "3253 Repique4 Anel Mute"))
			(repique-anel-open . (76 "3252 Repique4 Anel Open"))
			(repique-mao-open  . (84 "3256 Repique5 Mao Open"))
			(repique-mao-slap  . (82 "3258 Repique5 Mao Slap"))
			(repique-mao-rim   . (83 "3257 Repique5 Mao Rim"))
			(zamba-open        . (31 "3147 Zabmba Open RH"))
			(zamba-stick       . (32 "3146 Zabmba STick LH"))
			(zamba-mute        . (30 "3148 Zabmba Mute RH"))
			(surdo-open        . (34 "3135 Surdo5-1 Open"))
			(surdo-stop        . (35 "3134 Surdo5-1 Stop"))
			(surdo-mute        . (33 "3136 Surdo5-1 Mute"))
			(surdo-stick       . (36 "3137 Surdo5-2 Sdstck"))
			(surdo-mute2       . (44 "3133 Surdo4 Mute"))
			(surdo-open2       . (45 "3132 Surdo4 Open"))
			(surdo-stop2       . (46 "3131 Surdo4 Stop"))
			(surdo-mute2       . (47 "3130 Surdo3 Mute"))
			(surdo-open3       . (48 "3129 Surdo3 Open"))
			(surdo-mute3       . (49 "3128 Surdo3 Stop"))
			(caixa-A           . (37 "5313 Caixa1 RimShot"))
			(caixa-B           . (38 "5312 Caixa1 Center"))
			(tumbo-rim         . (39 "3085 Tumbo5 Rim Close"))
			(tumbo-open        . (40 "3084 Tumbo5 Center Open"))
			(tumbo-finger      . (41 "3083 Tumbo5 Finger Back"))
			(tumbo-down        . (42 "3082 Tumbo4 Down"))
			(tumbo-up          . (43 "3081 Tumbo4 Up"))
			(tntan-open        . (51 "3144 Tntan2 Open RH"))
			(tntan-close       . (50 "3145 Tntan2 Close RH"))
			(tntan-slap        . (52 "3143 Tntan2 Slap RH"))
			(tntan-body        . (53 "3142 Tntan2 Body LH"))
			(pandeiro-open     . (55 "3093 Pandeiro2 LThmb Opn"))
			(pandeiro-closed   . (54 "3094 Pandeiro2 LThmb Cls"))
			(pandeiro-rim      . (56 "3091 Pandeiro2 LToe Rim"))
			(pandeiro-heel     . (57 "3090 Pandeiro2 LHeel"))
			(pandeiro-slap     . (58 "3092 Pandeiro2 LSlap"))
			(pandeiro-roll     . (59 "3095 Pandeiro2 LRoll"))
			(timbale-lo        . (66 "3044 Timbale3 Lo"))
			(timbale-lo-rim    . (64 "3046 Timbale3 Lo Rim"))
			(timbale-hi        . (65 "3045 Timbale3 Hi"))
			(timbale-hi-rim    . (63 "3047 Timbale3 Hi Rim"))
			(agogo-lo          . (68 "3109 Agogo2 Bell2"))
			(agogo-lo-click    . (70 "3111 Agogo4 Click"))
			(agogo-hi          . (67 "3108 Agogo2 Bell1"))
			(agogo-hi-click    . (69 "3110 Agogo3 Click"))
			(cuica-A           . (79 "2976 Cuica2 Low"))
			(cuica-B           . (78 "2977 Cuica2 Med"))
			(cuica-C           . (77 "2978 Cuica2 Hi"))
			(reco              . (60 "3122 Reco1 (Guiro"))
			(shaker-A          . (62 "3076 Caxixi 3"))
			(shaker-B          . (71 "3070 Cabasa2"))
			(shaker-C          . (72 "3056 Ganza2 Forward1"))
			(shaker-D          . (61 "2911 Chocal Forward"))
			(triangle-open     . (81 "2885 Triangle2 Open"))
			(triangle-mute     . (80 "2886 Triangle2 Mute"))
			(whistle-lo        . (86 "3221 Apito Lo Short"))
			(whistle-hi        . (85 "3222 Apito Hi Short"))))
       (repique-klist (extract-sub-symbolic-keylist 'repique general-klist))
       (zamb-klist (extract-sub-symbolic-keylist 'zamb general-klist))
       (surdo-klist (extract-sub-symbolic-keylist 'surdo general-klist))
       (caixa-klist (extract-sub-symbolic-keylist 'caixa general-klist))
       (tumbo-klist (extract-sub-symbolic-keylist 'tumbo general-klist))
       (tntan-klist (extract-sub-symbolic-keylist 'tntan general-klist))
       (pandeiro-klist (extract-sub-symbolic-keylist 'pandeiro general-klist))
       (timbale-klist (extract-sub-symbolic-keylist 'timbale general-klist))
       (agogo-klist (extract-sub-symbolic-keylist 'agogo general-klist))
       (cuica-A-klist (extract-sub-symbolic-keylist 'cuica-A general-klist))
       (reco-klist (extract-sub-symbolic-keylist 'reco general-klist))
       (shaker-klist (extract-sub-symbolic-keylist 'shaker general-klist))
       (triangle-klist (extract-sub-symbolic-keylist 'triangle general-klist))
       (whistle-klist (extract-sub-symbolic-keylist 'whistle general-klist))
       (brz (make-main-instrument brazil-kit general-klist)) )
  (make-sub brz-repique brz repique-klist)
  (make-sub brz-zamb brz zamb-klist)
  (make-sub brz-surdo brz surdo-klist)
  (make-sub brz-caixa brz caixa-klist)
  (make-sub brz-tumbo brz tumbo-klist)
  (make-sub brz-tntan brz tntan-klist)
  (make-sub brz-pandeiro brz pandeiro-klist)
  (make-sub brz-timbale brz timbale-klist)
  (make-sub brz-agogo brz agogo-klist)
  (make-sub brz-cuica-A brz cuica-A-klist)
  (make-sub brz-reco brz reco-klist)
  (make-sub brz-shaker brz shaker-klist)
  (make-sub brz-triangle brz triangle-klist)
  (make-sub brz-whistle brz whistle-klist))


(export '(brz-repique
	  brz-zamb
	  brz-surdo
	  brz-caixa
	  brz-tumbo
	  brz-tntan
	  brz-pandeiro
	  brz-timbale
	  brz-agogo
	  brz-cuica-A
	  brz-reco
	  brz-shaker
	  brz-triangle
	  brz-whistle) :modx)

(import '(modx:brz-repique
	  modx:brz-zamb
	  modx:brz-surdo
	  modx:brz-caixa
	  modx:brz-tumbo
	  modx:brz-tntan
	  modx:brz-pandeiro
	  modx:brz-timbale
	  modx:brz-agogo
	  modx:brz-cuica-A
	  modx:brz-reco
	  modx:brz-shaker
	  modx:brz-triangle
	  modx:brz-whistle) :cyco)
	  
