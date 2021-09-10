;;;; CYCO Emu Procussion plugin: found-sound
;;;;
;;;; Zone Stack             key rang
;;;;  1 446 Dust Pan     : 036 044          *
;;;;  2                  :                  *
;;;;  3                  :                  *
;;;;  4 321 NoiseBurst   : 045 047          *
;;;;  5 322 BigThawk     : 048 052          *
;;;;  6 323 WhiteNoise   : 053 057          *
;;;;  7 324 Big Rasp     : 058 062          *
;;;;  8 325 Big Hammer   : 063 067          *
;;;;  9 326 MetalStack   : 068 072          *
;;;; 10 327 Stilleto     : 073 077          *
;;;; 11 329 Pipe         : 078 082          *
;;;; 12 330 Clank        : 083 087          *
;;;; 13 331 Lazar Hit    : 088 092          *
;;;; 14 332 Sonic Crack  : 093 096          *
;;;; 15 523 Cheering     : 098              *
;;;; 16                  :                  *
;;;; 17                  :                  *
;;;; 18                  :                  *
;;;; 19                  :                  *
;;;; 20                  :                  *
;;;; 21                  :                  *
;;;; 22                  :                  *
;;;; 23                  :                  *
;;;; 24                  :                  *


(let ((keylist '((pan-1       . (36 "Dust pan"))
		 (pan-2       . (37))
		 (pan-3       . (38))
		 (pan-4       . (39))
		 (pan-5       . (44))
		 (thawk-1     . (48 "BigThawk"))
		 (thawk-2     . (49))
		 (thawk-3     . (50))
		 (thawk-4     . (51))
		 (thawk-5     . (52))
		 (white-1     . (53 "WhiteNoise"))
		 (white-2     . (54))
		 (white-3     . (55))
		 (white-4     . (56))
		 (white-5     . (57))
		 (rasp-1      . (58 "Big Rasp"))
		 (rasp-2      . (59))
		 (rasp-3      . (60))
		 (rasp-4      . (61))
		 (rasp-5      . (62))
		 (hammer-1    . (63 "Big Hammer"))
		 (hammer-2    . (64))
		 (hammer-3    . (65))
		 (hammer-4    . (66))
		 (hammer-5    . (67))
		 (stack-1     . (68 "MetalStack"))
		 (stack-2     . (69))
		 (stack-3     . (70))
		 (stack-4     . (71))
		 (stack-5     . (72))
		 (stilleto-1  . (73))
		 (stilleto-2  . (74))
		 (stilleto-3  . (75))
		 (stilleto-4  . (76))
		 (stilleto-5  . (77))
		 (pipe-1      . (78))
		 (pipe-2      . (79))
		 (pipe-3      . (80))
		 (pipe-4      . (81))
		 (pipe-5      . (82))
		 (clank-1     . (83))
		 (clank-2     . (84))
		 (clank-3     . (85))
		 (clank-4     . (86))
		 (clank-5     . (87))
		 (lazar-1     . (88 "Lazar Hit"))
		 (lazar-2     . (89))
		 (lazar-3     . (90))
		 (lazar-4     . (91))
		 (lazar-5     . (92))
		 (crack-1     . (93 "Sonic Crack"))
		 (crack-2     . (94))
		 (crack-3     . (95))
		 (crack-4     . (96))
		 (burst-1     . (45 "NoiseBurst"))
		 (burst-2     . (46))
		 (burst-3     . (47))
		 (cheering    . (98)))))

      (defun found-sound (&key (parent procussion) channel)
	(instrument found-sound
		    :parent parent
		    :channel channel
		    :program (procussion-program 'found-sound)
		    :keynumber-map (procussion-keymap 36 98 keylist)))) 
  

