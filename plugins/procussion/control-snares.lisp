;;;; CYCO Emu Procussion plugin: control-snares
;;;;
;;;; Zone Stack             key range : 
;;;;  1 153 GarbageBoy     : 036 041  : 
;;;;  2 148 HouseSnr 6     : 041 046  :
;;;;  3 192 Stereo Snr     : 046 051  :
;;;;  4 159 BrushDance     : 051 056  :
;;;;  5 112 WetSnare 3     : 056 061  :
;;;;  6 163 TamSnare 2     : 061 066  :
;;;;  7 104 Slap Snare     : 066 070  :
;;;;  8                    :          :
;;;;  9                    :          :
;;;; 10                    :          :
;;;; 11                    :          :
;;;; 12                    :          :
;;;; 13 129 HerboSnare     : 071 073  :
;;;; 14 168 BackwrdSnare   : 074 076  :
;;;; 15 118 MobVerbSnare   : 077 079  : 
;;;; 16 172 RevSnare 3     : 080 082  :
;;;; 17 100 Tonal Sanre    : 083 085  :
;;;; 18 115 Ambi-snr 1     : 086 088  :
;;;; 19 132 SnareSmash     : 089 091  :
;;;; 20 120 ModPan Snr     : 092 094  :
;;;; 21 119 ModVerbRim     : 095 097  : 
;;;; 22 121 ReverbaRim     : 098 100  :
;;;; 23                    :          : 
;;;; 24 129 HerboSnare     : 104 106  :
;;;;


(defun control-snares (&key (parent procussion) channel)
  (instrument control-snares
	      :parent parent
	      :channel channel
	      :program (procussion-program 'control-snares)
	      :keynumber-map
	      (procussion-keymap 36 106 '((garbage-1  . (36))
					  (garbage-2  . (37))
					  (garbage-3  . (38))
					  (garbage-4  . (39))
					  (garbage-5  . (40))
					  (garbage-6  . (41))
					  (house-1    . (41))
					  (house-2    . (42))
					  (house-3    . (43))
					  (house-4    . (44))
					  (house-5    . (45))
					  (house-6    . (46))
					  (stereo-1   . (46))
					  (stereo-2   . (47))
					  (stereo-3   . (48))
					  (stereo-4   . (49))
					  (stereo-5   . (50))
					  (stereo-6   . (51))
					  (brush-1    . (51))
					  (brush-2    . (52))
					  (brush-3    . (53))
					  (brush-4    . (54))
					  (brush-5    . (55))
					  (brush-6    . (56))
					  (wet-1      . (56))
					  (wet-2      . (57))
					  (wet-3      . (58))
					  (wet-4      . (59))
					  (wet-5      . (60))
					  (wet-6      . (61))
					  (tam-1      . (61))
					  (tam-2      . (62))
					  (tam-3      . (63))
					  (tam-4      . (64))
					  (tam-5      . (65))
					  (tam-6      . (66))
					  (slap-1     . (66))
					  (slap-2     . (67))
					  (slap-3     . (68))
					  (slap-4     . (69))
					  (slap-5     . (70))
					  (herbo-1    . (71))
					  (herbo-2    . (72))
					  (herbo-3    . (73))
					  (herbo-4    . (104))
					  (herbo-5    . (105))
					  (herbo-6    . (106))
					  (backward-1 . (74))
					  (backward-2 . (75))
					  (backward-3 . (76))
					  (modverb-1  . (77))
					  (modverb-2  . (78))
					  (modverb-3  . (79))
					  (rev-1      . (80))
					  (rev-2      . (81))
					  (rev-3      . (82))
					  (tonal-1    . (83))
					  (tonal-2    . (84))
					  (tonal-3    . (85))
					  (ambi-1     . (86))
					  (ambi-2     . (87))
					  (ambi-3     . (88))
					  (smash-1    . (89))
					  (smash-2    . (90))
					  (smash-3    . (91))
					  (modpan-1   . (92))
					  (modpan-2   . (93))
					  (modpan-3   . (94))
					  (modrim-1   . (95))
					  (modrim-2   . (96))
					  (modrim-3   . (97))
					  (rim-1      . (98))
					  (rim-2      . (99))
					  (rim-3      . (100)))))) 


