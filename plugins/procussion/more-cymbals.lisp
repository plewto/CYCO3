;;;; CYCO plugin ion emu procussion more-cymbals.lisp
;;;;
;;;; more-cymbals
;;;; Zone Stack
;;;;  1 267 HouseHat 5    : 036 041                                                               
;;;;  2 268 HouseHat 6    : 041 046                                                            
;;;;  3 244 ClsTipHatA    : 046 051                                                           
;;;;  4 245 OpenHiHatA    : 051 056                                                             
;;;;  5 277 Hatsoff       : 056 061                            
;;;;  6 270 Beasty Hat    : 061 066                                                            
;;;;  7 271 BeastyOpen    : 066 071                                                         
;;;;  8 317 StereoMalt    : 071 076                                                         
;;;;  9 297 Cymb.Decay    : 076 081                                                        
;;;; 10 299 PangCymbal    : 081 086                                                              
;;;; 11 296 Dubl Crash    : 086 096                                                            
;;;; 12 312 SFX 2         : 098 098
;;;;

(let ((keylist '((HOUSE-HAT      . (36))
		 (HOUSE-HAT-1    . (37))
		 (HOUSE-HAT-2    . (38))
		 (HOUSE-HAT-3    . (39))
		 (HOUSE-HAT-4    . (40))
		 (HOUSE-HAT-5    . (41))
		 (HOUSE-HAT-6    . (42))
		 (HOUSE-HAT-7    . (43))
		 (HOUSE-HAT-8    . (44))
		 (HOUSE-HAT-9    . (45))
		 (TIP-HAT        . (46))
		 (TIP-HAT-1      . (47))
		 (TIP-HAT-2      . (48))
		 (TIP-HAT-3      . (49))
		 (TIP-HAT-4      . (50))
		 (OPEN-HAT       . (51))
		 (OPEN-HAT-1     . (52))
		 (OPEN-HAT-2     . (53))
		 (OPEN-HAT-3     . (54))
		 (OPEN-HAT-4     . (55))
		 (HATS-OFF       . (56))
		 (HATS-OFF-1     . (57))
		 (HATS-OFF-2     . (58))
		 (HATS-OFF-3     . (59))
		 (HATS-OFF-4     . (60))
		 (BEASTY         . (61))
		 (BEASTY-1       . (62))
		 (BEASTY-2       . (63))
		 (BEASTY-3       . (64))
		 (BEASTY-4       . (65))
		 (BEASTY-OPEN    . (66))
		 (BEASTY-OPEN-1  . (67))
		 (BEASTY-OPEN-2  . (68))
		 (BEASTY-OPEN-3  . (69))
		 (BEASTY-OPEN-4  . (70))
		 (MALT           . (71))
		 (MALT-1         . (72))
		 (MALT-2         . (73))
		 (MALT-3         . (74))
		 (MALT-4         . (75))
		 (CYMB           . (76))
		 (CYMB-1         . (77))
		 (CYMB-2         . (78))
		 (CYMB-3         . (79))
		 (CYMB-4         . (80))
		 (PANG           . (81))
		 (PANG-1         . (82))
		 (PANG-2         . (83))
		 (PANG-3         . (84))
		 (PANG-4         . (85))
		 (CRASH          . (86))
		 (CRASH-1        . (87))
		 (CRASH-2        . (88))
		 (CRASH-3        . (89))
		 (CRASH-4        . (90))
		 (SFX            . (98)))))
  
  (defun more-cymbals (&key (parent PROCUSSION)(channel nil))
    (instrument more-cymbals
  		:parent parent
		:remarks "Procussion more-cymbals instrument"
		:transient t
		:channel channel
  		:program (procussion-program 'more-cymbals)
		:keynumber-map (procussion-keymap 36 98 keylist)))) 

