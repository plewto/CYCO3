;;;; CYCO plugin sj emu procussion more-cymbals.lisp
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

(let ((alist (list (cons 'HOUSE-HAT      '(36) )
		   (cons 'HOUSE-HAT-1    '(37) )
		   (cons 'HOUSE-HAT-2    '(38) )
		   (cons 'HOUSE-HAT-3    '(39) )
		   (cons 'HOUSE-HAT-4    '(40) )
		   (cons 'HOUSE-HAT-5    '(41) )
		   (cons 'HOUSE-HAT-6    '(42) )
		   (cons 'HOUSE-HAT-7    '(43) )
		   (cons 'HOUSE-HAT-8    '(44) )
		   (cons 'HOUSE-HAT-9    '(45) )
		   (cons 'TIP-HAT        '(46) )
		   (cons 'TIP-HAT-1      '(47) )
		   (cons 'TIP-HAT-2      '(48) )
		   (cons 'TIP-HAT-3      '(49) )
		   (cons 'TIP-HAT-4      '(50) )
		   (cons 'OPEN-HAT       '(51) )
		   (cons 'OPEN-HAT-1     '(52) )
		   (cons 'OPEN-HAT-2     '(53) )
		   (cons 'OPEN-HAT-3     '(54) )
		   (cons 'OPEN-HAT-4     '(55) )
		   (cons 'HATS-OFF       '(56) )
		   (cons 'HATS-OFF-1     '(57) )
		   (cons 'HATS-OFF-2     '(58) )
		   (cons 'HATS-OFF-3     '(59) )
		   (cons 'HATS-OFF-4     '(60) )
		   (cons 'BEASTY         '(61) )
		   (cons 'BEASTY-1       '(62) )
		   (cons 'BEASTY-2       '(63) )
		   (cons 'BEASTY-3       '(64) )
		   (cons 'BEASTY-4       '(65) )
		   (cons 'BEASTY-OPEN    '(66) )
		   (cons 'BEASTY-OPEN-1  '(67) )
		   (cons 'BEASTY-OPEN-2  '(68) )
		   (cons 'BEASTY-OPEN-3  '(69) )
		   (cons 'BEASTY-OPEN-4  '(70) )
		   (cons 'MALT           '(71) )
		   (cons 'MALT-1         '(72) )
		   (cons 'MALT-2         '(73) )
		   (cons 'MALT-3         '(74) )
		   (cons 'MALT-4         '(75) )
		   (cons 'CYMB           '(76) )
		   (cons 'CYMB-1         '(77) )
		   (cons 'CYMB-2         '(78) )
		   (cons 'CYMB-3         '(79) )
		   (cons 'CYMB-4         '(80) )
		   (cons 'PANG           '(81) )
		   (cons 'PANG-1         '(82) )
		   (cons 'PANG-2         '(83) )
		   (cons 'PANG-3         '(84) )
		   (cons 'PANG-4         '(85) )
		   (cons 'CRASH          '(86) )
		   (cons 'CRASH-1        '(87) )
		   (cons 'CRASH-2        '(88) )
		   (cons 'CRASH-3        '(89) )
		   (cons 'CRASH-4        '(90))
		   (cons 'SFX            '(98)) )))
  
  (defun more-cymbals (&key (parent PROB)(channel nil) articulation-map dynamic-map)
    (instrument more-cymbals
  		:parent parent
		:remarks "Procussion more-cymbals instrument"
		:transient t
		:channel channel
  		:program (procussion-program 'more-cymbals)
		:articulation-map articulation-map
		:dynamic-map dynamic-map
		:keynumber-map (symbolic-keynumber-map alist)))) 
