;;;; CYCO plugins sj em procussion tollkit.lisp
;;;;
;;;; Zone    Stack             key-range 
;;;; Z01     S540 twank        036 038
;;;; Z02     S541 megaPlanks   039 041
;;;; Z03     S514 waterpin     042 044
;;;; Z04     S542 templodoom   045 047
;;;; Z05     S543 glong        048 050
;;;; Z06     S544 giantrtachet 051 053
;;;; Z07     S367 wood block   054 056 *
;;;; Z08     S330 clank        054 056 *
;;;; Z09     S545 echo thud    057 059
;;;; Z10     S546 awk bosk     060 062
;;;; Z11     S547 gymnasium    063 065
;;;; Z12     S544 giantratchet 066 068
;;;; Z13     S541 mega planks  069 071
;;;; Z14     S547 gymnasium    072 074
;;;; Z15     S546 awk bosk     075 077
;;;; Z16     S057 spaceKick    078 080
;;;; Z17     S124 pariSnare    081 083
;;;; Z18     S287 16Choke1     084 086
;;;; Z19     S318 HyperReal    087 089
;;;; Z20     S115 AmbiSnare1   090 092
;;;; Z21     S118 ModVerbSnr   093 095
;;;; Z22     S171 RevSnare2    096 098
;;;; Z23     S125 Snare 1157   099 101
;;;; Z24     S209 TunedTomz    102 104
;;;;

(let ((key-list '((gymnasium-1   . (63))
		  (gymnasium-2   . (64))
		  (gymnasium-3   . (65))
		  (gymnasium-4   . (72))
		  (gymnasium-5   . (73))
		  (gymnasium-6   . (74))
		  (kick-1        . (78))
		  (kick-2        . (79))
		  (kick-3        . (80))
		  (snare-1       . (81))
		  (snare-2       . (82))
		  (snare-3       . (83))
		  (snare-4       . (90))
		  (snare-5       . (91))
		  (snare-6       . (92))
		  (snare-7       . (93))
		  (snare-8       . (94))
		  (snare-9       . (95))
		  (snare-10      . (99))
		  (snare-11     . (100))
		  (snare-12     . (101))
		  (revsnare-1    . (96))
		  (revsnare-2    . (97))
		  (revsnare-3    . (98))
		  (toms-1       . (102))
		  (toms-2       . (103))
		  (toms-3       . (104))
		  (choke-1       . (84))
		  (choke-2       . (85))
		  (choke-3       . (86))
		  (ride-1        . (87))
		  (ride-2        . (88))
		  (ride-3        . (89))
		  (clank-1       . (54))
		  (clank-2       . (55))
		  (clank-3       . (56))
		  (waterpin-1    . (42))
		  (waterpin-2    . (43))
		  (waterpin-3    . (44))
		  (twank-1       . (36))
		  (twank-2       . (37))
		  (twank-3       . (38))
		  (plank-1       . (69))
		  (plank-2       . (70))
		  (plank-3       . (71))
		  (plank-4       . (39))
		  (plank-5       . (40))
		  (plank-6       . (41))
		  (doom-1        . (35))
		  (doom-2        . (46))
		  (doom-3        . (47))
		  (glong-1       . (48))
		  (glong-2       . (49))
		  (glong-3       . (50))
		  (ratchet-1     . (66))
		  (ratchet-2     . (67))
		  (ratchet-3     . (68))
		  (ratchet-4     . (51))
		  (ratchet-5     . (52))
		  (ratchet-6     . (53))
		  (thud-1        . (57))
		  (thud-2        . (58))
		  (thud-3        . (59))
		  (awkbosk-1     . (60))
		  (awkbosk-2     . (61))
		  (awkbosk-3     . (62))
		  (awkbosk-4     . (75))
		  (awkbosk-5     . (76))
		  (awkbosk-6     . (77)))))
  (defun toolkit (&key (parent procussion)(channel nil))
    (instrument toolkit
		:parent parent
		:channel channel
		:program (procussion-program 'toolkit)
		:keynumber-map (procussion-keymap 36 104 key-list)
		:transient t))) 
		


