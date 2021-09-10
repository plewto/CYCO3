;;;; CYCO Emu Procussion plugin: sound-fx
;;;;
;;;; Zone Stack             key range
;;;;  1 521 GongAttack    : 036 038        
;;;;  2 522 UFOs          : 039 041   
;;;;  3 523 Cheering      : 042 044   
;;;;  4 524 GeneratorX    : 045 047   
;;;;  5 525 Noise Fx      : 048 050   
;;;;  6 530 Rocket 1      : 051 053 * 
;;;;  7 531 Rocket 1      : 051 053 * 
;;;;  8 478 Surfin USA    : 054 056   
;;;;  9 441 Star Tree     : 057 059   
;;;; 10 548 Klicker       : 060 062   
;;;; 11 468 MutantMal 2   : 063 065   
;;;; 12 549 Der Aufish    : 066 068   
;;;; 13 550 Bubl Trubl    : 069 071   
;;;; 14 462 Pipeline      : 072 074   
;;;; 15 470 Big Pipe      : 075 077   
;;;; 16 536 Qool Klanq    : 078 080   
;;;; 17 474 TinkerTine    : 081 083   
;;;; 18 551 FunkinNoys    : 084 086   
;;;; 19 554 Rasparity     : 087 089   
;;;; 20 552 Lazer Slew    : 090 092   
;;;; 21 450 VoxFreak 2    : 093 095   
;;;; 22 467 MutantMall    : 096 098     
;;;; 23 553 Kloggers      : 099 101   
;;;; 24 449 VoxFreak 1    : 102 104   
;;;;

(let ((keylist '((gong-1        . (036 "Gong Attack"))
		 (gong-2        . (037))
		 (gong-3        . (038))
		 (ufo-1         . (039))
		 (ufo-2         . (040))
		 (ufo-3         . (041))
		 (cheering-1    . (042))
		 (cheering-2    . (043))
		 (cheering-3    . (044))
		 (generator-1   . (045 "GeneratorX"))
		 (generator-2   . (046))
		 (generator-3   . (047))
		 (nsefx-1       . (048 "Noise Fx"))
		 (nsefx-2       . (049))
		 (nsefx-3       . (050))
		 (rocket-1      . (051 "Rocket 1"))
		 (rocket-2      . (052))
		 (rocket-3      . (053))
		 (surf-1        . (054 "Surfin USA"))
		 (surf-2        . (053))
		 (surf-3        . (056))
		 (star-tree-1   . (057))
		 (star-tree-2   . (058))
		 (star-tree-3   . (059))
		 (klicker-1     . (060))
		 (klicker-2     . (061))
		 (klicker-3     . (062))
		 (mutantmall-1  . (063))
		 (mutantmall-2  . (064))
		 (mutantmall-3  . (065))
		 (mutantmall-4  . (096))
		 (mutantmall-5  . (097))
		 (mutantmall-6  . (098))
		 (der-aufish-1  . (066 "Der Aufish"))
		 (der-aufish-2  . (067))
		 (der-aufish-3  . (068))
		 (bubble-1      . (069 "BublTrbl"))
		 (bubble-2      . (070))
		 (bubble-3      . (071))
		 (pipeline      . (072))
		 (pipeline-2    . (073))
		 (pipeline-3    . (074))
		 (pipe-1        . (075 "Big Pipe"))
		 (pipe-2        . (076))
		 (pipe-3        . (077))
		 (klang-1       . (078 "Qool Klanq"))
		 (klang-2       . (079))
		 (klang-3       . (080))
		 (tinker-1      . (081 "TinkerTine"))
		 (tinker-2      . (082))
		 (tinker-3      . (083))
		 (noys-1        . (084 "FunkinNoys"))
		 (noys-2        . (085))
		 (noys-3        . (086))
		 (rasp-1        . (087 "Rasparity"))
		 (rasp-2        . (088))
		 (rasp-3        . (089))
		 (lazer-1       . (090 "Lazar Slew"))
		 (lazer-2       . (091))
		 (lazer-3       . (092))
		 (voxfreak-1    . (093))
		 (voxfreak-2    . (094))
		 (voxfreak-3    . (095))
		 (voxfreak-4    . (102))
		 (voxfreak-5    . (103))
		 (voxfreak-6    . (104))
		 (kloggers-1    . (099))
		 (kloggers-2    . (100))
		 (kloggers-3    . (101)))))
      (defun sound-fx (&key (parent procussion) channel)
	(instrument sound-fx
		    :parent parent
		    :channel channel
		    :program (procussion-program 'sound-fx)
		    :keynumber-map (procussion-keymap 36 104 keylist))))
						  
						  
						     
						  

