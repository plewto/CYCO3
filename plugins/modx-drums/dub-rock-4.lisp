;;;; CYCO MODX drumkit Dub-Rock-4
;;;; Proxy for frum kit om channel 4 of MODX factory performance "Dub Rock Bass"
;;;;
;;;; 35 B0  4688 Dub BD 2 St          :
;;;; 36 C1  4690 Dub BD 4 Mn          :
;;;; 37 C#1 5419 Sd Break3            :
;;;; 38 D1  5462 Dub SD 4 Mn          :
;;;; 39 D#1 4195 Dub Clap St          :
;;;; 40 E1  5417 Sd Break1            :
;;;; 41 F1  6139 Dub Tom Mn           :
;;;; 42 F#1 5828 Trap HH 1 St         :
;;;; 43 G1  5486 Flam Mn              :
;;;; 44 G#1 5824 Dub HH 1 Mn          :
;;;; 45 A1  4689 Dub BD 3 Mn          :
;;;; 46 A#1 5826 Dub OHH 1 St         :
;;;; 47 B1  4687 Dub BD 1 Mn          :
;;;; 48 C2  4691 Dub BD 5 St          :
;;;; 49 C#2 4690 Dub BD 4 Mn          :
;;;; 50 D2  4305 Dub Misc 01 St       :
;;;; 51 D#2 6334 Dub Ride St          :
;;;; 52 E2  2574 Dub Fx1 Mn           :
;;;; 53 F2  2575 Dub Fx2 St           :
;;;; 54 F#2 4262 Electric Tambourine3 :
;;;; 55 G2  4263 Electric Tambourine4 :
;;;; 56 G#2 4306 Dub Misc 02 St       :
;;;; 57 A2  4308 Dub Misc 04 St       :
;;;; 58 A#2 4307 Dub Misc 03 St       :
;;;; 59 B2  5838 Trap OHH 2 St        :
;;;; 60 C3  4309 Dub Misc 05 St       :
;;;; 61 C#3 4310 Dub Misc 06 St       :
;;;; 62 D3  4314 Dub Misc 10 St       :
;;;; 63 D#3 4315 Trap Misc 1 St       :
;;;; 64 E3  4313 Dub Misc 09 St       :
;;;; 65 F3  4314 Dub Misc 10 St       :
;;;; 66 F#3 4692 Dub BD 6 St          :
;;;; 67 G3  4360 Tick 01 St           :
;;;; 68 G#3 2653 HipHop Scratch2 St   :
;;;; 69 A3  4253 Elec Shaker1 St      :
;;;; 70 A#3 4254 Elec Shaker2 St      :
;;;; 71 B3  4255 Elec Shaker3 St      :
;;;; 72 C4  4256 Elec Shaker4 St      :
;;;; 73 C#4 4257 Elec Shaker5 St      :
;;;; 74 D4  4357 Rattle Mn            :
;;;; 75 D#4 5461 Dub SD 3 Mn          :
;;;; 76 E4  4267 Elec Wood Block1 St  :
;;;; 77 F4  4269 Elec Wood Block3 St  :
;;;; 78 F#4 4268 Elec Wood Block2 St  :
;;;; 79 G4  4270 Elec Wood Block4 St  :
;;;; 80 G#4 5462 Dub SD 4 Mn          :
;;;; 81 A4  5463 Dub SD 5 St          :

(in-package :modx)

(let* ((general-klist '((KICK-A     . (47 "4687 Dub BD 1 Mn"))
			(KICK-B     . (35 "4688 Dub BD 2 St"))
			(KICK-C     . (45 "4689 Dub BD 3 Mn"))
			(KICK-D     . (36 "4690 Dub BD 4 Mn"))
			(KICK-E     . (49 "4690 Dub BD 4 Mn"))
			(KICK-F     . (48 "4691 Dub BD 5 St"))
			(KICK-G     . (66 "4692 Dub BD 6 St"))
			(SNARE-A    . (39 "4195 Dub Clap St"))
			(SNARE-B    . (37 "5419 Sd Break3"))
			(SNARE-C    . (38 "5462 Dub SD 4 Mn"))
			(SNARE-D    . (40 "5417 Sd Break1"))
			(SNARE-E    . (75 "5461 Dub SD 3 Mn"))
			(SNARE-F    . (80 "5462 Dub SD 4 Mn"))
			(SNARE-G    . (81 "5463 Dub SD 5 St"))
			(SNARE-H    . (43 "5486 Flam Mn"))
			(TOM        . (41 "6139 Dub Tom Mn"))
			(HAT-X      . (42 "5828 Trap HH 1 St"))
			(HAT-X2     . (44 "5824 Dub HH 1 Mn"))
			(HAT-OPEN   . (46 "5826 Dub OHH 1 St"))
			(HAT-OP     . (59 "5838 Trap OHH 2 St"))
			(SHAKER-A   . (69 "4253 Elec Shaker1 St"))
			(SHAKER-B   . (70 "4254 Elec Shaker2 St"))
			(SHAKER-C   . (71 "4255 Elec Shaker3 St"))
			(SHAKER-D   . (72 "4256 Elec Shaker4 St"))
			(SHAKER-E   . (73 "4257 Elec Shaker5 St"))
			(SHAKER-F   . (74 "4357 Rattle Mn"))
			(TAMBOURINE-A . (54 "4262 Electric Tambourine3"))
			(TAMBOURINE-B . (55 "4263 Electric Tambourine4"))
			(BLOCK-A    . (76 "4267 Elec Wood Block1 St"))
			(BLOCK-B    . (77 "4269 Elec Wood Block3 St"))
			(BLOCK-C    . (78 "4268 Elec Wood Block2 St"))
			(BLOCK-D    . (79 "4270 Elec Wood Block4 St"))
			(RIDE       . (51 "6334 Dub Ride St"))
			(DUB-A      . (52 "2574 Dub Fx1 Mn"))
			(DUB-B      . (53 "2575 Dub Fx2 St"))
			(DUB-C      . (50 "4305 Dub Misc 01 St"))
			(DUB-D      . (56 "4306 Dub Misc 02 St"))
			(DUB-E      . (57 "4308 Dub Misc 04 St"))
			(DUB-F      . (58 "4307 Dub Misc 03 St"))
			(DUB-G      . (60 "4309 Dub Misc 05 St"))
			(DUB-H      . (61 "4310 Dub Misc 06 St"))
			(DUB-I      . (62 "4314 Dub Misc 10 St"))
			(DUB-J      . (64 "4313 Dub Misc 09 St"))
			(DUB-K      . (65 "4314 Dub Misc 10 St"))
			(TRAP       . (63 "4315 Trap Misc 1 St"))
			(TICK       . (67 "4360 Tick 01 St"))
			(SCRATCH    . (68 "2653 HipHop Scratch2 St"))))
       (dr4 (make-main-instrument dub-rock-4 general-klist)))
  dr4)
  
