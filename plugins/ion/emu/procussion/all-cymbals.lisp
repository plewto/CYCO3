;;;; CYCO plugins ion emu procussion all-cymbals.lisp
;;;;
;;;;  zone/stack            range
;;;;  1 507 Gong Pow      : 036 037                                                               
;;;;  2 233 HihatA 1/3    : 038 039                                                            
;;;;  3 234 HihatA 2/3    : 040 041                                                           
;;;;  4 235 HihatA Open   : 042 043                                                             
;;;;  5 236 HihatA stomp  : 044 045                            
;;;;  6 286 16"Cymbal 1   : 046 047                                                            
;;;;  7 287 16"Choke 1    : 048 049                                                         
;;;;  8 288 16"Crash 2    : 050 051                                                         
;;;;  9 289 19"Pang       : 052 053                                                        
;;;; 10 290 Ride Ping     : 054 056                                                              
;;;; 11 291 Ride Bell     : 057 059                                                            
;;;; 12 298 HyeRideCym    : 060 063                                                            
;;;; 13 302 DarkCymbl1    : 064 066                                                             
;;;; 14 303 DarkCymb2     : 067 069                               
;;;; 15 309 GongODoom     : 070 071                                                         
;;;; 16 318 Hyperreal     : 072 074                                  
;;;; 17 317 StereoMalt    : 075 076                                               
;;;; 18 321 NoiseBurst    : 077 078                                               
;;;; 19 493 IceMissile    : 079 083                                              
;;;; 20 237 HihatB Shut   : 084 085                                      
;;;; 21 238 HiHatB 1/3    : 086 087                                      
;;;; 22 239 HihatB 2/3    : 088 089                                      
;;;; 23 240 HihatB Open   : 090 091                                      
;;;; 24 241 HihatB stomp  : 092 093
;;;;
;;;; ALL-CYMBALS
;;;;    | 
;;;;    +-- ACYM-GONG
;;;;    +-- ACYM-AHAT
;;;;    +-- ACYM-BHAT
;;;;    +-- ACYM-CYM
;;;;    +-- ACYM-SFX
;;;;

(let* ((gong-keys '((gong1  . (36))
		    (gong2  . (70))
		    (gong3  . (37))
		    (gong   . (71))))
       (hha-keys '((ped   . (44))
		   (op    . (38))
		   (opn   . (40))
		   (open  . (42))
		   (ped2  . (45))
		   (op2   . (38))
		   (opn2  . (41))
		   (open2 . (43))))
       (hhb-keys '((x     . (84))
		   (op    . (86))
		   (opn   . (88))
		   (open  . (90))
		   (ped   . (92))
		   (x2    . (85))
		   (op2   . (87))
		   (opn2  . (89))
		   (open2 . (91))
		   (ped2  . (93))))
       (cym-keys '((ride-1      . (046))
		   (choke-1     . (048))
		   (crash-1     . (050))
		   (pang-1      . (052))
		   (ping-1      . (054))
		   (bell-1      . (057))
		   (hyper-1     . (060))
		   (dark-1      . (064))
		   (dark-2      . (067))
		   (hyper-1     . (072))
		   (malt-1      . (075))
		   (ride-2      . (047))
		   (choke-2     . (049))
		   (crash-2     . (051))
		   (pang-2      . (053))
		   (ping-2      . (056))
		   (bell-2      . (059))
		   (hyper-2     . (063))
		   (dark-3      . (066))
		   (dark-4      . (069))
		   (hyper-2     . (074))
		   (malt-2      . (076))))
       (sfx-keys '((nse-1  . (077))
		   (nss-2  . (078))
		   (ice1   . (079))
		   (ice2   . (080))
		   (ice3   . (081))
		   (ice4   . (082))
		   (ice5   . (083))))
       (combined-keys (append gong-keys
			      hha-keys
			      hhb-keys
			      cym-keys
			      sfx-keys)))
  (defun all-cymbals (&key (parent PROB) channel articulation-map dynamic-map)
    (let ((ac (instrument all-cymbals
			  :parent parent
			  :remarks "Procussion all-cymbals parent instrument"
			  :transient t
			  :channel channel
			  :program (procussion-program 'all-cymbals)
			  :articulation-map articulation-map
			  :dynamic-map dynamic-map
			  :keynumber-map (symbolic-keynumber-map combined-keys))))
      (instrument acym-gong
		  :parent ac
		  :remarks "Procussion all-cymbals gong instrument"
		  :transient t
		  :keynumber-map (symbolic-keynumber-map gong-keys))
      (instrument acym-ahat
		  :parent ac
		  :remarks "Procussion all-cymbals hat A instrument"
		  :transient t
		  :keynumber-map (symbolic-keynumber-map hha-keys))
      (instrument acym-bhat
		  :parent ac
		  :remarks "Procussion all-cymbals hat B instrument"
		  :transient t
		  :keynumber-map (symbolic-keynumber-map hhb-keys))
      (instrument acym-cym
		  :parent ac
		  :remarks "Procussion all-cymbals cymbals instrument"
		  :transient t
		  :keynumber-map (symbolic-keynumber-map cym-keys))
      (instrument acym-sfx
		  :parent ac
		  :remarks "Procussion all-cymbals effects instrument"
		  :transient t
		  :keynumber-map (symbolic-keynumber-map sfx-keys))
      ac))) 
