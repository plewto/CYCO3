;;;; CYCO sj config emu/procussion/cymbals2
;;;;
;;;; Zone Stack             key range
;;;;  1 241 HiHatBStmp    : 024 025                                                               
;;;;  2 237 HiHatBshut    : 026 027                                                            
;;;;  3 238 HiHatB 1/3    : 028 029                                                           
;;;;  4 239 HiHatB 2/3    : 030 031                                                             
;;;;  5 240 HiHatBopen    : 032 033
;;;;  6 301 Vlcty Ride    : 036 037                                                            
;;;;  7 298 HypeRideCym   : 038 039                                                         
;;;;  8 318 Hypereal      : 040 041
;;;;  9 290 Ride Ping     : 042 043 044                                                    
;;;; 10 291 Ride Bell     : 045 046 047
;;;; 11 286 16"Crash      : 048                                                                
;;;; 12 287 16"Choke      : 049
;;;; 13 298 Dbl Crash     : 050 051                                                             
;;;; 14 302 DarkCymbal1   : 052 053                               
;;;; 15 303 DarkCymbal2   : 054 055
;;;; 16 293 China Gong    : 059 060                                  
;;;; 17 299 PangCymbal    : 061 062                                               
;;;; 18 314 SmashCymbl    : 063 064                                               
;;;; 19 306 SplashDance   : 065 068                                              
;;;; 20 308 FingerCymb    : 069 071
;;;; 21 309 Gong Doom     : 072 080                                      
;;;; 22 294 MalletRoll    : 080 083                                      
;;;; 23 292 MalCymbal     : 084 092                                      
;;;; 24 317 StereoMalt    : 093 127
;;;;
;;;; cymbals2
;;;;    |
;;;;    +-- gong
;;;;    +-- ride
;;;;    +-- ping
;;;;    +-- crash
;;;;    +-- mallet
;;;;    +-- malt
;;;;    +-- hats
;;;;

(let* ((gong-keys '((gong    . (72))
		     (gong-2  . (73))
		     (gong-3  . (74))
		     (gong-4  . (75))
		     (gong-5  . (76))
		     (gong-6  . (77))
		     (gong-7  . (78))
		     (gong-8  . (79))
		     (gong-9  . (80))))
       (ride-keys '((hyper   . (38))
		     (hyper-1 . (40))
		     (hyper-2 . (39))
		     (hyper-3 . (41))
		     (ping    . (42))
		     (bell    . (45))
		     (ping-2  . (43))
		     (bell-2  . (46))
		     (ping-3  . (44))
		     (bell-3  . (47))))
       (ping-keys  '((ping     . (42))
		      (pang     . (61))
		      (china    . (59))
		      (smash    . (63))
		      (splash   . (65))
		      (finger   . (69))
		      (ping-2   . (43))
		      (pang-2   . (62))
		      (china-2  . (60))
		      (ping-3   . (44))
		      (choke    . (49))
		      (smash-2  . (64))
		      (splash-2 . (66))
		      (finger-2 . (69))
		      (splash-3 . (67))
		      (finger-3 . (70))
		      (splash-4 . (68))
		      (finger-4 . (71))))
       (crash-keys '((dbl     . (50))
		      (dark    . (52))
		      (dark-2  . (54))
		      (dark-3  . (53))
		      (dark-4  . (55))
		      (dbl-2   . (51))
		      (sixteen . (48 "16-inch crash"))
		      (choke   . (49 "16-inch choke"))))
       (mallet-keys '((mallet   . (84))
		       (mallet-2 . (85))
		       (mallet-3 . (86))
		       (mallet-4 . (87))
		       (mallet-5 . (88))
		       (mallet-6 . (89))
		       (mallet-7 . (90))
		       (mallet-8 . (91))
		       (mallet-9 . (92))))
       (malt-keys '((malt-01 . (093))
		     (malt-02 . (094))
		     (malt-03 . (095))
		     (malt-04 . (096))
		     (malt-05 . (097))
		     (malt-06 . (098))
		     (malt-07 . (099))
		     (malt-08 . (100))
		     (malt-09 . (101))
		     (malt-10 . (102))
		     (malt-11 . (103))
		     (malt-12 . (104))
		     (malt-13 . (105))
		     (malt-14 . (106))
		     (malt-15 . (107))
		     (malt-16 . (108))
		     (malt-17 . (109))
		     (malt-18 . (110))
		     (malt-19 . (111))
		     (malt-20 . (112))
		     (malt-21 . (113))
		     (malt-22 . (114))
		     (malt-23 . (115))
		     (malt-24 . (116))
		     (malt-25 . (117))
		     (malt-26 . (118))
		     (malt-27 . (119))
		     (malt-28 . (120))
		     (malt-29 . (121))
		     (malt-30 . (122))
		     (malt-31 . (123))
		     (malt-32 . (124))
		     (malt-33 . (125))
		     (malt-34 . (126))
		     (malt-35 . (127))))
       (hats-keys '((x     . (26))
		     (op    . (28))
		     (opn   . (30))
		     (open  . (32))
		     (x2    . (27))
		     (op2   . (29))
		     (opn2  . (31))
		     (open2 . (33))))
       (combined-keys (append gong-keys ride-keys ping-keys hats-keys
			       crash-keys mallet-keys mallet-keys)))

  (defun cymbals2 (&key (parent pro2) channel articulation-map dynamic-map)
    (let ((c2 (instrument cymbals2
			  :parent parent
			  :remarks "Procussion cymbals2 parent instrument"
			  :transient t
			  :channel channel
			  :program (procussion-program 'cymbals2)
			  :articulation-map (or articulation-map
						(constant-articulation-map 12.0))
			  :dynamic-map dynamic-map
			  :keynumber-map (symbolic-keynumber-map combined-keys))))
      (instrument gong
      		  :parent c2
      		  :remarks "Procussin all-cymbals gong instrument"
      		  :keynumber-map (symbolic-keynumber-map gong-keys))
      (instrument ride
      		  :parent c2
      		  :remarks "Procussin all-cymbals ride instrument"
      		  :keynumber-map (symbolic-keynumber-map ride-keys))
      (instrument ping
      		  :parent c2
      		  :remarks "Procussin all-cymbals ride ping instrument"
      		  :keynumber-map (symbolic-keynumber-map ping-keys))
      (instrument crash
      		  :parent c2
      		  :remarks "Procussin all-cymbals crash instrument"
      		  :keynumber-map (symbolic-keynumber-map crash-keys))
      (instrument mallet
      		  :parent c2
      		  :remarks "Procussin all-cymbals mallet instrument"
      		  :keynumber-map (symbolic-keynumber-map mallet-keys))
      (instrument malt
      		  :parent c2
      		  :remarks "Procussin all-cymbals cymbal rolls instrument"
      		  :keynumber-map (symbolic-keynumber-map malt-keys))
      (instrument hats
      		  :parent c2
      		  :remarks "Procussin all-cymbals hats instrument"
      		  :keynumber-map (symbolic-keynumber-map hats-keys))
      c2))) 
