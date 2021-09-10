;;;; CYCO Emu Procussion plugin: rocket-drums
;;;;
;;;; Zone Stack             key range
;;;;  1 021 MansionKck     : 036      :
;;;;  2 044 HouseKick2     : 038      :
;;;;  3 105 ModernTwin     : 040      :
;;;;  4 173 CenterRvers    : 045      :
;;;;  5 099 ModerenSnare   : 041      :
;;;;  6 106 SnareCenter    : 043      :
;;;;  7 257 Elec Hat 1     : 037      :
;;;;  8 258 Elec Hat 2     : 039      :
;;;;  9 260 Elec Hat 4     : 042      :
;;;; 10 045 HouseKick3     : 035      :
;;;; 11                    :          :
;;;; 12                    :          :
;;;; 13                    :          :
;;;; 14                    :          :
;;;; 15                    :          :
;;;; 16 303 DarkCymbl2     : 046      :
;;;; 17                    :          :
;;;; 18                    :          :
;;;; 19                    :          :
;;;; 20                    :          :
;;;; 21 285 Bckwrd Hat     : 047      :
;;;; 22                    :          :
;;;; 23 223 AcousRoto1     : 072 122  :
;;;; 24 224 ElecTomic      : 048 072  :
;;;;
;;;; ROCKET-DRUMS
;;;;   RD-KICK
;;;;   RD-SNARE
;;;;   RD-HAT
;;;;   RD-ROTO
;;;;   RD-TOMIC
;;;;


(defun rocket-drums (&key (parent procussion) channel)
  (let ((rd (make-instrument 'rocket-drums
			     :parent parent
			     :channel channel
			     :program (procussion-program 'rocket-drums)
			     :keynumber-map (procussion-keymap 36 122))))
    (defparameter rd-kick (make-instrument 'rd-kick
					   :parent rd
					   :keynumber-map (procussion-subkey-map '((A . (36))
										   (B . (38))
										   (C . (35))))))
    (defparameter rd-snare (make-instrument 'rd-snare
					    :parent rd
					    :keynumber-map (procussion-subkey-map '((A . (40))
										    (B . (45))
										    (C . (41))
										    (D . (43))))))
    (defparameter rd-hat (make-instrument 'rd-hat
					  :parent rd
					  :keynumber-map (procussion-subkey-map '((closed . (37))
										  (op     . (39))
										  (opn    . (42))
										  (open   . (42))
										  (reverse . (47))
										  (crash   . (46))))))
    (defparameter rd-roto (make-instrument 'rd-roto
					   :parent rd
					   :keynumber-map (circular-keynumber-map 72 122)))
    (defparameter rd-tomic (make-instrument 'rd-tomic
					    :parent rd
					    :keynumber-map (circular-keynumber-map 48 72)))
    
    (defparameter rocket-drums rd)
    rd))
