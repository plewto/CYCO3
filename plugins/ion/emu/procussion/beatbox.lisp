;;;; CYCO plugins sj emu procussion beatbox
;;;;
;;;;                                4         5         6         7         8         9
;;;; Zone Stack           Range 6789012345678901234567890123456789012345678901234567890123456
;;;;    1 047 Rap Kick    36 36 X------------------------------------------------------------
;;;;    2 381 Plastic Cow 46 46 ----------X--------------------------------------------------         
;;;;    3 141 AnalogSnr   38 38 --X----------------------------------------------------------
;;;;    4 406 QunitoTone  58 59 ----------------------XX-------------------------------------
;;;;    5 380 Synclav     37 37 -X-----------------------------------------------------------
;;;;    6 347 Klapz       39 39 ---X---------------------------------------------------------
;;;;    7 256 SynthHatB   42 42 ------X------------------------------------------------------
;;;;    8 255 SynthHatA   44 44 --------X----------------------------------------------------
;;;;    9 210 AnalogTom   60 96 ------------------------XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
;;;;   10 255 SynthHatA   50 52 ----------------X--------------------------------------------
;;;;   11 210 AnalogTom   43 43 -------X-----------------------------------------------------
;;;;   12 210 AnalogTom   45 45 ---------X---------------------------------------------------
;;;;   13 210 AnalogTom   47 47 -----------X-------------------------------------------------
;;;;   14 210 AnalogTom   40 40 ----X--------------------------------------------------------
;;;;   15 210 AnalogTom   41 41 -----X-------------------------------------------------------
;;;;   16 210 AnalogTom   57 57 ---------------------X---------------------------------------
;;;;   17 389 CabasaFrnt  48 48 ------------X------------------------------------------------
;;;;   18 390 CabasaBack  49 49 -------------X-----------------------------------------------
;;;;   19  88 SnapSnare   53 56 -----------------XXXX----------------------------------------
;;;;   20   -
;;;;   21   -
;;;;   22   -
;;;;   23 110 WetSnare1   34 34 -------------------------------------------------------------
;;;;   24   -
;;;;
;;;; beatbox
;;;;    |
;;;;    +-- bb-kick
;;;;    +-- bb-ido
;;;;    +-- bb-hat
;;;;    +-- bb-snare
;;;;    +-- bb-quinto
;;;;    +-- bb-tom
;;;;    +-- bb-analog-tom

(let* ((kick-keys '((a . (36 "Rap Kick"))
		    (b . (36 "Rap Kick"))))
       (ido-keys '((cow      . (46 ""))
		   (clave    . (37 ""))
		   (claps    . (39 ""))
		   (cabasa   . (48 "Cabasa Front"))
		   (cabasa-2 . (49 "Cabasa Back"))
		   (tick     . (53 ""))
		   (tick-2   . (54 ""))
		   (tick-3   . (55 ""))
		   (tick-4   . (56 ""))))
       (hat-keys '((x . (42 "SynthHat B"))
		   (A . (43 "SynthHat A"))
		   (B . (50 "SynthHat A"))
		   (C . (51 "SynthHat A"))
		   (D . (52 "SynthHat A"))))
       (snare-keys '((x   . (38 "Analog Snare"))
		     (wet . (34 "Wet Snare"))))
       (quinto-keys '((a  . (58 "Qunito Tone"))
		      (b  . (59 "Quinto Tone"))))
       (tom-keys   '((a  . (40 "Low Tom"))
		     (b  . (41 ""))
		     (c  . (57 ""))
		     (d  . (43 ""))
		     (e  . (45 ""))
		     (f  . (47 ""))))
       (composite-keys '((kick . (36 "Rap Kick"))
			 (cow      . (46 ""))
			 (clave    . (37 ""))
			 (claps    . (39 ""))
			 (cabasa   . (48 "Cabasa Front"))
			 (cabasa-2 . (49 "Cabasa Back"))
			 (tick     . (53 ""))
			 (tick-2   . (54 ""))
			 (tick-3   . (55 ""))
			 (tick-4   . (56 ""))
			 (hat . (42 "SynthHat B"))
			 (hat-A . (43 "SynthHat A"))
			 (hat-B . (50 "SynthHat A"))
			 (hat-c . (51 "SynthHat A"))
			 (hat-D . (52 "SynthHat A"))
			 (snare   . (38 "Analog Snare"))
			 (wet-snare . (34 "Wet Snare"))
			 (quinto  . (58 "Qunito Tone"))
			 (quinto-b  . (59 "Quinto Tone"))
			 (tom-a  . (40 "Low Tom"))
			 (tom-b  . (41 ""))
			 (tom-c  . (57 ""))
			 (tom-d  . (43 ""))
			 (tom-e  . (45 ""))
			 (tom-f  . (47 "")))))
  (defun beatbox (&key (parent PROB))
    (let ((bb (instrument beatbox
			  :parent parent
			  :remarks "Procussion BeatBox parent instrument"
			  :transient t
			  :keynumber-map (symbolic-keynumber-map composite-keys)
			  :program (procussion-program 'beatbox))))
      (instrument bb-kick
		  :parent bb
		  :remarks "Procussion BeatBox Kick instrument"
		  :transient t
		  :keynumber-map (symbolic-keynumber-map kick-keys))
      (instrument bb-ido
		  :parent bb
		  :remarks "Procussion BeatBox bb-ido instrument"
		  :transient t
		  :keynumber-map (symbolic-keynumber-map ido-keys))
      (instrument bb-hat
		  :parent bb
		  :remarks "Procussion BeatBox bb-hat instrument"
		  :transient t
		  :keynumber-map (symbolic-keynumber-map hat-keys))
      (instrument bb-snare
		  :parent bb
		  :remarks "Procussion BeatBox bb-snare instrument"
		  :transient t
		  :keynumber-map (symbolic-keynumber-map snare-keys))
      (instrument bb-quinto
		  :parent bb
		  :remarks "Procussion BeatBox bb-quinto instrument"
		  :transient t
		  :keynumber-map (symbolic-keynumber-map quinto-keys))
      (instrument bb-tom
		  :parent bb
		  :remarks "Procussion BeatBox bb-tom instrument"
		  :transient t
		  :keynumber-map (symbolic-keynumber-map tom-keys))
      (instrument bb-analog-tom
		  :parent bb
		  :remarks "Procussion BeatBox bb-analog-tom instrument"
		  :transient t
		  :keynumber-map (circular-keynumber-map 60 96))
      bb)))
