;;;; CYCO modx-drumkits plugin midnight-funk
;;;;
;;;; 24 4671 Bd EDM01            
;;;; 25 5393 Sd AnCR             
;;;; 26 5353 Sd T9-5             
;;;; 27 4190 RX11 Clap           
;;;; 28 5354 Sd T9Gate           
;;;; 29 5206 Sd RockRoll St      
;;;; 30 5351 Sd T9-3             
;;;; 31 5352 Sd T9-4             
;;;; 32 5354 Sd T9Gate           
;;;; 33 4563 Bd T9-4             
;;;; 34 5353 Sd T9-5             
;;;; 35 4560 Bd T9-1             
;;;; 36 4671 Bd EDM01            
;;;; 37 5355 Sd T9Rim            
;;;; 38 5349 Sd T9-1             
;;;; 39 4190 RX11 Clap           
;;;; 40 5350 Sd T9-2             
;;;; 41 6107 Tom T9 Lo           
;;;; 42 5795 HH Closed RX11      
;;;; 43 6107 Tom T9 Lo           
;;;; 44 5767 HH Open T9          
;;;; 45 6107 Tom T9 Lo           
;;;; 46 5796 HH Open RX11        
;;;; 47 6106 Tom T9 Hi           
;;;; 48 3062 Cabasa1A 1-2        
;;;; 49 6303 Crash T9-1          
;;;; 50 3858 Tombek Tek Dead Sw  
;;;; 51 4247 Cowbell RX11        
;;;; 52 6287 China2              
;;;; 53 6281 Ride Cup Mono       
;;;; 54 4258 Tambourine RX5      
;;;; 55 6179 Crash3              
;;;; 56 3223 Cowbell1            
;;;; 57 6180 Crash4              
;;;; 58 4242 Cowbell T8          
;;;; 59 6231 Ride2 R             
;;;; 60 4231 Conga T8            
;;;; 61 4231 Conga T8            
;;;; 62 2922 Conga1 Hi Tip       
;;;; 63 2928 Conga1 Hi Slap Open 
;;;; 64 2926 Conga1 Hi Open 2    
;;;; 65 3026 Timbale1 Hi 1-3     
;;;; 66 3030 Timbale1 Lo 1-2     
;;;; 67 4286 Analog Click        
;;;; 68 4231 Conga T8            
;;;; 69 3062 Cabasa1A 1-2        
;;;; 70 3059 Maracas1 Slur       
;;;; 71 2613 FxGun2              
;;;; 72 2612 FxGun1              
;;;; 73 2629 Scratch H           
;;;; 74 2631 Scratch Down        
;;;; 75 2512 Hi Q1               
;;;; 76 2512 Hi Q1               
;;;; 77 2513 Hi Q2               
;;;; 78 2630 Scratch L           
;;;; 79 2630 Scratch L           
;;;; 80 2884 Triangle1 Mute      
;;;; 81 2883 Triangle1 Open      
;;;; 82 4250 Analog Shaker       
;;;; 83 2908 Sleigh Bell1        
;;;; 84 2912 Wind Chime 1        
;;;; 85 4959 Sd Piccolo          
;;;; 86 5380 Sd T8-5             
;;;; 87 5209 Sd RockRollD St     
;;;; 88 5227 Sd Brush Med St     
;;;; 89 4596 Bd BlpHd            
;;;; 90 5397 Sd Jungle1          
;;;; 91 4629 Bd Sustain          
;;;; 92 5401 Sd D&B1             
;;;; 93 4607 Bd Break2           
;;;; 94 5427 Sd Dist             
;;;; 95 4630 Bd TekPower         
;;;; 96 4615 Bd Distortion R     

(in-package :modx)

(let* (
       (general-klist
	'((KICK       . (24 "4671 Bd EDM01"))
			(KICK-T9A   . (33 "4563 Bd T9-4"))
			(KICK-T9B   . (35 "4560 Bd T9-1"))
			(KICK-EDM   . (36 "4671 Bd EDM01"))
			(KICK-BLP   . (89 "4596 Bd BlpHd"))
			(KICK-SUS   . (91 "4629 Bd Sustain"))
			(KICK-BREAK . (93 "4607 Bd Break2"))
			(KICK-POWER . (95 "4630 Bd TekPower"))
			(KICK-CLIP  . (96 "4615 Bd Distortion R"))

			(SNARE          . (25 "5393 Sd AnCR"))
			(SNARE-T9A      . (38 "5349 Sd T9-1"))
			(SNARE-T9B      . (40 "5350 Sd T9-2"))
			(SNARE-T9C      . (30 "5351 Sd T9-3"))
			(SNARE-T9D      . (31 "5352 Sd T9-4"))
			(SNARE-T9C      . (34 "5353 Sd T9-5"))
			(SNARE-T9D      . (26 "5353 Sd T9-5"))
			(SNARE-T9-GATE  . (32 "5354 Sd T9Gate"))
			(SNARE-T9-RIM   . (37 "5355 Sd T9Rim"))
			(SNARE-T9-GATE2 . (28 "5354 Sd T9Gate"))
			(SNARE-T8       . (86 "5380 Sd T8-5"))
			(SNARE-ROCK     . (29 "5206 Sd RockRoll St"))
			(SNARE-PICCOLO  . (85 "4959 Sd Piccolo"))
			(SNARE-ROCK2    . (87 "5209 Sd RockRollD St"))
			(SNARE-BRUSH    . (88 "5227 Sd Brush Med St"))
			(SNARE-JUMGLE   . (90 "5397 Sd Jungle1"))
			(SNARE-DB1      . (92 "5401 Sd D&B1"))
			(SNARE-CLIP     . (94 "5427 Sd Dist"))

			(CLAP-A . (27 "4190 RX11 Clap"))
			(CLAP-B . (39 "4190 RX11 Clap"))
			(CLAP-CLICK . (67 "4286 Analog Click"))

			(TOM-A . (41 "6107 Tom T9 Lo"))
			(TOM-B . (43 "6107 Tom T9 Lo"))
			(TOM-C . (45 "6107 Tom T9 Lo"))
			(TOM-D . (47 "6106 Tom T9 Hi"))

			(HAT     . (42 "5795 HH Closed RX11"))
			(HAT-OPN . (44 "5767 HH Open T9"))
			(HA-OPEN . (46 "5796 HH Open RX11"))

			(CYM        . (59 "6231 Ride2 R"))
			(CYM-CUP    . (53 "6281 Ride Cup Mono"))
			(CYM-CRASH  . (55 "6179 Crash3"))
			(CYM-CRASH2 . (57 "6180 Crash4"))
			(CYM-CREAH3 . (49 "6303 Crash T9-1"))
			(CYM-CHINA  . (52 "6287 China2"))

			(COW      . (56 "3223 Cowbell1"))
			(COW-RX11 . (51 "4247 Cowbell RX11"))
			(COW-T8   . (58 "4242 Cowbell T8"))

			(TRI      . (81 "2883 Triangle1 Open"))
			(TRI-MUTE . (80 "2884 Triangle1 Mute"))

			(HIQ-A . (75 "2512 Hi Q1"))
			(HIQ-B . (76 "2512 Hi Q1"))
			(HIQ-C . (77 "2513 Hi Q2"))

			(CONGA-T8A    . (60 "4231 Conga T8"))
			(CONGA-T8B    . (61 "4231 Conga T8"))
			(CONGA-T8C    . (68 "4231 Conga T8"))
			(CONGA-TIP    . (62 "2922 Conga1 Hi Tip"))
			(CONGA-SLAP   . (63 "2928 Conga1 Hi Slap Open"))
			(CONGA-HI     . (64 "2926 Conga1 Hi Open 2"))

			(TOMBEK . (50 "3858 Tombek Tek Dead Sw"))

			(TIMBALE-LO . (66 "3030 Timbale1 Lo 1-2"))
			(TIMBALE-HI . (65 "3026 Timbale1 Hi 1-3"))
			
			(SHAKER            . (82 "4250 Analog Shaker"))
			(SHAKER-CABASA     . (48 "3062 Cabasa1A 1-2"))
			(SHAKER-CABASA2    . (69 "3062 Cabasa1A 1-2"))
			(SHAKER-MARACAS    . (70 "3059 Maracas1 Slur"))
			(SHAKER-TAMBOURINE . (54 "4258 Tambourine RX5"))
			(SHAKER-SLEIGH     . (83 "2908 Sleigh Bell1"))
			(SHAKER-WIND-CHOME . (84 "2912 Wind Chime 1"))
			
			(SCRATCH-A   . (73 "2629 Scratch H"))
			(SCRATCH-B   . (74 "2631 Scratch Down"))
			(SCRATCH-C   . (78 "2630 Scratch L"))
			(SCRATCH-D   . (79 "2630 Scratch L"))
			
			(FX-A . (71 "2613 FxGun2"))
			(FX-B . (72 "2612 FxGun1"))))

		      (kick-klist (extract-sub-symbolic-keylist 'kick general-klist))
		      (snare-klist (extract-sub-symbolic-keylist 'snare general-klist))
		      (clap-klist (extract-sub-symbolic-keylist 'clap general-klist))
		      (tom-klist (extract-sub-symbolic-keylist 'tom general-klist))
		      (hat-klist (extract-sub-symbolic-keylist 'hat general-klist))
		      (cym-klist (extract-sub-symbolic-keylist 'cym general-klist))
		      (cow-klist (extract-sub-symbolic-keylist 'cow general-klist))
		      (tri-klist (extract-sub-symbolic-keylist 'tri general-klist))
		      (hiq-klist (extract-sub-symbolic-keylist 'hiq general-klist))
		      (conga-klist (extract-sub-symbolic-keylist 'conga general-klist))
		      (tombak-klist (extract-sub-symbolic-keylist 'tombak general-klist))
		      (timbale-klist (extract-sub-symbolic-keylist 'timbale general-klist))
		      (shaker-klist (extract-sub-symbolic-keylist 'shaker general-klist))
		      (scratch-klist (extract-sub-symbolic-keylist 'scratch general-klist))
		      (fx-klist (extract-sub-symbolic-keylist 'fx general-klist))

		      (mf (make-main-instrument midnight-funk general-klist)))

  (make-sub mfnk-kick mf kick-klist)
  (make-sub mfnk-snare mf snare-klist)
  (make-sub mfnk-clap mf clap-klist)
  (make-sub mfnk-tom mf tom-klist)
  (make-sub mfnk-hat mf hat-klist)
  (make-sub mfnk-cym mf cym-klist)
  (make-sub mfnk-cow mf cow-klist)
  (make-sub mfnk-tri mf tri-klist)
  (make-sub mfnk-hiq mf hiq-klist)
  (make-sub mfnk-conga mf conga-klist)
  (make-sub mfnk-tombak mf tombak-klist)
  (make-sub mfnk-timbale mf timbale-klist)
  (make-sub mfnk-shaker mf shaker-klist)
  (make-sub mfnk-scratch mf scratch-klist)
  (make-sub mfnk-fx mf fx-klist))

(export '(mfnk-kick
	  mfnk-snare
	  mfnk-clap
	  mfnk-tom
	  mfnk-hat
	  mfnk-cym
	  mfnk-cow
	  mfnk-tri
	  mfnk-hiq
	  mfnk-conga
	  mfnk-tombak
	  mfnk-timbale
	  mfnk-shaker
	  mfnk-scratch
	  mfnk-fx) :modx)

(import '(modx:mfnk-kick
	  modx:mfnk-snare
	  modx:mfnk-clap
	  modx:mfnk-tom
	  modx:mfnk-hat
	  modx:mfnk-cym
	  modx:mfnk-cow
	  modx:mfnk-tri
	  modx:mfnk-hiq
	  modx:mfnk-conga
	  modx:mfnk-tombak
	  modx:mfnk-timbale
	  modx:mfnk-shaker
	  modx:mfnk-scratch
	  modx:mfnk-fx) :cyco)
  
			
			
