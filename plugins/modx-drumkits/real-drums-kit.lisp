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



(let ((common-klist (general-midi-drum-keylist
		     '((crash3   . (52))
		       (cuica-hi . (78))
		       (cuica-lo . (79))
		       (clave2   . (24))
		       (snare-brush    . (25))
		       (snare-brush2   . (26))
		       (snare-brush3   . (27))
		       (snare-brush4   . (28))
		       (snare-roll     . (29))
		       (castanet       . (30))
		       (snare3         . (31))
		       (snare-stick    . (32))
		       (kick3          . (33))
		       (snare4         . (34))
		       (shaker         . (69))
		       (shaker2        . (70))
		       (shaker3        . (54))
		       (shaker4        . (82))
		       (sleigh-bells   . (83))
		       (wind-chime     . (84))
		       (snare5         . (85))
		       (snare-blues    . (86))
		       (snare-hip      . (87))
		       (snare-funk     . (88))
		       (snare-soul     . (89))
		       (snare-mono     . (90))
		       (snare-heavy    . (91))
		       (snare-tight    . (92))
		       (snare-blues2   . (93))
		       (kick-snap      . (94))
		       (kick-tight     . (95))
		       (kick-funk      . (96)))
		     '(chinese cuica-mute cuica-open cabasa maracas tambourine)))
      (kick-klist '((A      . (35))
		    (B      . (36))
		    (C      . (33))
		    (SNAP   . (94))
		    (TIGHT  . (95))))
      (snare-klist '((x      . (38))
		     (x2     . (40))
		     (x3     . (31))
		     (STICK  . (32))
		     (x4     . (34))
		     (x5     . (85))
		     (BLUES  . (86))
		     (HIP    . (87))
		     (FUNK   . (88))
		     (SOUL   . (89))
		     (MONO   . (90))
		     (HEAVY  . (91))
		     (TIGHT  . (92))
		     (BLUES2 . (93))
		     (STICK2 . (37))
		     (BRUSH  . (25))
		     (BRUSH2 . (26))
		     (BRUSH3 . (27))
		     (BRUSH4 . (28))
		     (ROLL   . (29))
		     (clap   . (39))))
      (tom-klist '((A  . (41))
		   (B  . (43))
		   (C  . (45))
		   (D  . (47))
		   (E  . (48))
		   (F  . (50))))
      (latin-drums-klist '((CONGA-OPEN   . (63))
			  (CONGA-LOW    . (64))
			  (CONGA-HIGH   . (62))
			  (BONGO-LOW    . (61))
			  (BONGO-HIGH   . (60))
			  (TIMBALE-LOW  . (66))
			  (TIMBALE-HIGH . (65))
			  (CUICA-LO     . (79))
			  (CUICA-HI     . (78))))
      (hat-klist '((x    . (42))
		   (open . (46))
		   (ped  . (44))))
      (cymbals-klist '((RIDE   . (51))
		       (RIDE2  . (59))
		       (BELL   . (53))
		       (CRASH  . (49))
		       (CRASH2 . (57))
		       (CRASH3 . (52))
		       (SPLASH . (55))))
      (woodblock-klist '((BLOCK-LO  . (77))
			 (BLOCK-HI  . (76))
			 (CLAVE     . (75))
			 (CLAVE2    . (24))
			 (CASTANET  . (30))))
      (cowbell-klist '((COW           . (56))
		       (AGOGO-LO      . (68))
		       (AGOGO-HI      . (67))
		       (TRIANGLE      . (81))
		       (TRIANGLE-MUTE . (80))))
      (shaker-klist '((A  . (69))
		      (B  . (70))
		      (C  . (54))
		      (D  . (82))
		      (E  . (73))
		      (F  . (74))
		      (G  . (83))
		      (H  . (58)))) )       

  (defun real-drums-kit (channel &key (performance *CURRENT-MODX-PERFORMANCE*))
    (let ((rdk (make-instrument 'real-drums-kit
				:channel channel
				:parent performance
	    			:keynumber-map (symbolic-keynumber-map common-klist))))
      (defparameter real-drums-kit rdk)
      (instrument rdk-kick :parent rdk :keynumber-map (symbolic-keynumber-map kick-klist))
      (instrument rdk-snare :parent rdk :keynumber-map (symbolic-keynumber-map snare-klist))
      (instrument rdk-tom   :parent rdk :keynumber-map (symbolic-keynumber-map tom-klist))
      (instrument rdk-latin-drums :parent rdk
		  :keynumber-map (symbolic-keynumber-map latin-drums-klist))
      (instrument rdk-hat :parent rdk :keynumber-map (symbolic-keynumber-map hat-klist))
      (instrument rdk-cymbals :parent rdk :keynumber-map (symbolic-keynumber-map cymbals-klist))
      (instrument rdk-woodblock :parent rdk :keynumber-map (symbolic-keynumber-map woodblock-klist))
      (instrument rdk-cowbell :parent rdk :keynumber-map (symbolic-keynumber-map cowbell-klist))
      (instrument rdk-shaker :parent rdk :keynumber-map (symbolic-keynumber-map shaker-klist))
      rdk)))

(register-modx-drumkit-info 'real-drums-kit)
