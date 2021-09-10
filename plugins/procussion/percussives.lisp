;;;; CYCO plugins ion emu procussion percussives.lisp
;;;;
;;;;                                  3   4         5         6         7         8         9 
;;;; Zone Stack             key-range 678901234567890123456789012345678901234567890123456789012345678 
;;;; Z01  S488 BAM BAM      036 047   6**********7                                                   
;;;; Z02  S274 Noise Hat A  037 053    7***************3                                                
;;;; Z03  S064 Tick Kick    048 059               8**********9
;;;; Z04  S336 Syn Scratch  054 065                     4**********5                                    
;;;; Z05  S352 Analog Tick  060 071                           0**********1                              
;;;; Z06  S263 HouseHat 1   066 077                                 6**********7                        
;;;; Z07  S367 Wood Block   072 083                                       2**********3                  
;;;; Z08  S308 FingerCymb   078 096                                             8*****************6     
;;;; Z09  S450 VoxFreak2    084 096                                                   4***********6     
;;;; Z10  S312 SFX 2        098 098                                                                 8   
;;;;
;;;;
;;;; PERCUSSIVES
;;;;    |
;;;;    +-- PV-BAMBAM        36-47
;;;;    +-- PV-TICK-KICK     48-59
;;;;    +-- PV-ANALOG-TICK   60-65
;;;;    +-- PV-HOUSEHAT      66-77
;;;;    +-- PV-BLOCKCYM      78-83
;;;;    +-- PV-VOXCYM        84-96 
;;;;    +-- PV-SFX2          98-98

(defun percussives (&key (parent PROCUSSION)(channel nil))
  (let ((perc (instrument percussives
			  :parent parent
			  :remarks "Emu Procussion Percussives parent instrument"
			  :channel channel
			  :program (procussion-program 'percussives)
			  :transient t
			  :keynumber-map (procussion-keymap 36 98))))
    (instrument pv-bambam     
		:parent perc
		:remarks "Emu Percussives bambam instrument"
		:transient t
		:keynumber-map (circular-keynumber-map 36 47))
    (instrument pv-tick-kick     
		:parent perc
		:remarks "Emu Percussives tick-kick instrument"
		:transient t
		:keynumber-map (circular-keynumber-map 48 59))
    (instrument pv-analog-tick     
		:parent perc
		:remarks "Emu Percussives anlog-tick instrument"
		:transient t
		:keynumber-map (circular-keynumber-map 60 65))
    (instrument pv-househat     
		:parent perc
		:remarks "Emu Percussives househat instrument"
		:transient t
		:keynumber-map (circular-keynumber-map 66 77))
    (instrument pv-blockcym     
		:parent perc
		:remarks "Emu Percussives blockcym instrument"
		:transient t
		:keynumber-map (circular-keynumber-map 78 83))
    (instrument pv-voxcym     
		:parent perc
		:remarks "Emu Percussives voxcym instrument"
		:transient t
		:keynumber-map (circular-keynumber-map 84 96))
    (instrument pv-sfx2     
		:parent perc
		:remarks "Emu Percussives sfx2 instrument"
		:transient t
		:keynumber-map (circular-keynumber-map 98 98))
    (param percussives perc)
    perc))
  
  
