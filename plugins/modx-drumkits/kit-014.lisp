;;;; CYCO modx-drumkits kit-014 "Real Drums Kit 1"
;;;;                            "Real Drums Kit 2"
;;;;

(defun modx-drumkit-14 (&key (channel 10)(parent +root-instrument+))
  (let* ((drumkit (make-instrument 'modx-drumkit-14 :parent parent :channel channel
				   :remarks "MODX Drum kit 14 'Real Drums Kit 1'  & 'Real Drums kit 2'"
				   :keynumber-map (symbolic-keynumber-map
						   '((KICK-A 009 "AT Kick Sw")
						     (KICK-B 011 "GT Kick1 Sw")
						     (KICK-C 012 "GT Kick2 Sw")
						     (KICK-D 070 "Bd Snap 1-2")
						     (KICK-E 071 "Bd Tight 1-2")
						     (KICK-F 072 "Bd Funk St1-2")
						     (SNARE-A1 007 "LW Snare Sw")
						     (SNARE-A2 010 "DG Snare1 Sw")
						     (SNARE-A3 014 "DG Snare2 Sw")
						     (SNARE-A4 016 "TM Snare Sw")
						     (SNARE-B1 061 "Sd Bld St1-4")re
						     (SNARE-B2 062 "Sd Blues 3St")
						     (SNARE-B3 063 "Sd Hip St1-2")
						     (SNARE-B4 064 "Sd Funk St1-3")
						     (SNARE-B5 065 "Sd Soul St1-4")
						     (SNARE-B6 066 "Sd LdwHMono")
						     (SNARE-B7 067 "Sd Heavy")
						     (SNARE-B8 068 "Sd Tight St1-4")
						     (SNARE-B9 069 "Sd BluesHeart St1-3")
						     (SNARE-ROLL  005 "Bright Snare Roll")
						     (SNARE-RIM   013 "Sd Closed Rim5")
						     (SNARE-CRACK 008 "Stick1")
						     (SNARE-BRUSH-1 001 "Sd Brush Soft St")
						     (SNARE-BRUSH-2 002 "Sd Brush Swir St")
						     (SNARE-BRUSH-3 003 "Sd Brush Med St")
						     (SNARE-BRUSH-4 004 "Sd Brush SwirAtt St")
						     (TOM-A 024 "CP Tom High Sw")
						     (TOM-B 026 "CP Tom High Swr")
						     (TOM-C 021 "CP Tom Low Sw")
						     (TOM-D 023 "CP Tom Mid Sw")
						     (TOM-E 017 "CP Tom Floor Sw")
						     (TOM-F 019 "CP Tom Floor Sw")
						     (BONGO-HI 036 "Bongo1 Hi 1Fingr 1-2")
						     (BONGO-LOW 037 "Bongo1 Lo 1Fingr 1-2")
						     (CONGA-SLAP 038 "Conga1 Hi SlpMute1-2")
						     (CONGA-HI 039 "Conga1 Hi Open 1-2")
						     (CONGA-LOW 040 "Conga1 Lo Open 1-2")
						     (TIMBALE-HI 041 "Timbale1 Hi 1-3")
						     (TIMBALE-LOW 042 "Timbale1 Lo 1-2")
						     (HAT 018 "PT HiHat Closed")
						     (HAT-PED 020 "PT HiHat Pedal")	
						     (HAT-OPEN 022 "PT HiHat Open")
						     (RIDE-1 027 "Ride1 Tip Sw")
						     (RIDE-1-BELL 029 "Ride1 Cup Sw")
						     (RIDE-2 035 "Ride2 Tip Sw")
						     (CRASH-1 025 "Crash1 Sw")
						     (CRASH-2 028 "Crash2 Sw")
						     (CRASH-3 033 "Crash3 Sw")
						     (SPLASH 031 "Splash1")
						     (COW 032 "Cowbell5")
						     (AGOGO-HI 043 "Agogo1 Hi")
						     (AGOGO-LOW 044 "Agogo1 Lo")
						     (TRINAGLE-MUTE 056 "Triangle1 Mute")
						     (TRIANGLE 057 "Triangle1 Open")
						     (CLAVE-1 000 "Claves1")
						     (CLAVE-2 051 "Claves1")
						     (WOODBLOCK-1 052 "Wood Block")
						     (WOODBLOCK-2 053 "Wood Block")
						     (CASTINETS 006 "Castanet1")
						     (SHAKER 058 "ShakerA 13")
						     (CABASA 045 "Cabasa1A 1-2")
						     (MARACAS 046 "Maracas1 Slur")
						     (TAMBOURINE 030 "Tambourine1 Sw")
						     (GUIRO-SHORT 049 "Guiro1 Short")
						     (GUIRO-LONG 050 "Guiro1 Long")
						     (SEIGH 059 "Sleigh Bell1")
						     (CLAP 015 "Hand Clap3 Sw")
						     (VIBRASLAP 034 "Vibraslap1")
						     (WHISTLE-1 047 "Whistle")
						     (WHISTLE-2 048 "Whistle")
						     (CUICA-HI 054 "Cuica1 Hi")
						     (CUICA-LO 055 "Cuica1 Lo")
						     (WINDCHIME 060 "Wind Chime 1")))))
	 
	 (kick (make-instrument 'modx-kick :parent drumkit
	 			:keynumber-map (symbolic-keynumber-map '((A 009 "AT Kick Sw")
	 								 (B 011 "GT Kick1 Sw")
	 								 (C 012 "GT Kick2 Sw")
	 								 (D 070 "Bd Snap 1-2")
	 								 (E 071 "Bd Tight 1-2")
	 								 (F 072 "Bd Funk St1-2")))))
	 (snare (make-instrument 'modx-snare :parent drumkit
	 			 :keynumber-map (symbolic-keynumber-map
	 					 '((A1 007 "LW Snare Sw")
	 					   (A2 010 "DG Snare1 Sw")
	 					   (A3 014 "DG Snare2 Sw")
	 					   (A4 016 "TM Snare Sw")
	 					   (B1 061 "Sd Bld St1-4")
	 					   (B2 062 "Sd Blues 3St")
	 					   (B3 063 "Sd Hip St1-2")
	 					   (B4 064 "Sd Funk St1-3")
	 					   (B5 065 "Sd Soul St1-4")
	 					   (B6 066 "Sd LdwHMono")
	 					   (B7 067 "Sd Heavy")
	 					   (B8 068 "Sd Tight St1-4")
	 					   (B9 069 "Sd BluesHeart St1-3")
	 					   (roll  005 "Bright Snare Roll")
	 					   (rim   013 "Sd Closed Rim5")
	 					   (crack 008 "Stick1")
	 					   (brush-1 001 "Sd Brush Soft St")
	 					   (brush-2 002 "Sd Brush Swir St")
	 					   (brush-3 003 "Sd Brush Med St")
	 					   (brush-4 004 "Sd Brush SwirAtt St")
						   (CLAP 015 "Hand Clap3 Sw")))))

	 (tom (make-instrument 'modx-tom :parent drumkit
	 		       :keynumber-map (symbolic-keynumber-map
	 				       '((A 024 "CP Tom High Sw")
	 					 (B 026 "CP Tom High Swr")
	 					 (C 021 "CP Tom Low Sw")
	 					 (D 023 "CP Tom Mid Sw")
	 					 (E 017 "CP Tom Floor Sw")
	 					 (F 019 "CP Tom Floor Sw")))))
	 
	 (latin-drums (make-instrument 'modx-latin-drums :parent drumkit
	 			       :keynumber-map (symbolic-keynumber-map
	 					       '((bongo-hi 036 "Bongo1 Hi 1Fingr 1-2")
	 						 (bongo-low 037 "Bongo1 Lo 1Fingr 1-2")
	 						 (conga-slap 038 "Conga1 Hi SlpMute1-2")
	 						 (conga-hi 039 "Conga1 Hi Open 1-2")
	 						 (conga-low 040 "Conga1 Lo Open 1-2")
	 						 (timbale-hi 041 "Timbale1 Hi 1-3")
	 						 (timbale-low 042 "Timbale1 Lo 1-2")))))

	 (hat (make-instrument 'modx-hat :parent drumkit
	 		       :keynumber-map (symbolic-keynumber-map
	 				       '((X 018 "PT HiHat Closed")
	 					 (ped 020 "PT HiHat Pedal")
	 					 (open 022 "PT HiHat Open")))))

	 (cym (make-instrument 'modx-cym :parent drumkit
	 		       :keynumber-map (symbolic-keynumber-map
	 				       '((ride-1 027 "Ride1 Tip Sw")
	 					 (ride-1-bell 029 "Ride1 Cup Sw")
	 					 (ride-2 035 "Ride2 Tip Sw")
	 					 (crash-1 025 "Crash1 Sw")
	 					 (crash-2 028 "Crash2 Sw")
	 					 (crash-3 033 "Crash3 Sw")
	 					 (splash 031 "Splash1")))))

	 (cow (make-instrument 'modx-cow :parent drumkit
	 		       :keynumber-map (symbolic-keynumber-map
	 				       '((cow 032 "Cowbell5")
	 					 (agogo-hi 043 "Agogo1 Hi")
	 					 (agogo-low 044 "Agogo1 Lo")
	 					 (trinagle-mute 056 "Triangle1 Mute")
	 					 (triangle 057 "Triangle1 Open")))))
	 
	 (clave (make-instrument 'modx-clave :parent drumkit
	 			 :keynumber-map (symbolic-keynumber-map
	 					 '((clave-1 000 "Claves1")
	 					   (clave-2 051 "Claves1")
	 					   (woodblock-1 052 "Wood Block")
	 					   (woodblock-2 053 "Wood Block")
	 					   (castinets 006 "Castanet1")))))
	
	 (shaker (make-instrument 'modx-shaker :parent drumkit
	 			  :keynumber-map (symbolic-keynumber-map
	 					  '((shaker 058 "ShakerA 13")
	 					    (cabasa 045 "Cabasa1A 1-2")
	 					    (maracas 046 "Maracas1 Slur")
	 					    (tambourine 030 "Tambourine1 Sw")
	 					    (guiro-short 049 "Guiro1 Short")
	 					    (guiro-long 050 "Guiro1 Long")
	 					    (seigh 059 "Sleigh Bell1"))))) )
  (defparameter modx-kick kick)
  (defparameter modx-snare snare)
  (defparameter modx-tom tom)
  (defparameter modx-latin-drums latin-drums)
  (defparameter modx-hat hat)
  (defparameter modx-cym cym)
  (defparameter modx-cow cow)
  (defparameter modx-clave clave)
  (defparameter modx-shaker shaker)
  (defparameter modx-drumkit drumkit)
  drumkit)) 

		  

	 
	 
    
                                             
    
