;;;; CYCO Emu Procussion plugin: hip-house
;;;;                                    
;;;; Zone Stack             key range : 
;;;;  1 014 Dry Kick 6     : 036      : 
;;;;  2 166 BackwrdSnr1    : 047      : s
;;;;  3 140 DanceSnare     : 038      : s
;;;;  4 151 TamboSnare     : 037      : s
;;;;  5 099 ModernSnare    : 040      : s
;;;;  6 167 BackwrdSnr2    : 048      : s
;;;;  7 282 ServoHat 1     : 042      : h
;;;;  8 283 ServoHat 2     : 044      : h
;;;;  9 284 ServoHat 3     : 046      : h
;;;; 10 167 BackwardSnr2   : 050      : s
;;;; 11 373 Cowtombell     : 041      : t
;;;; 12 373 Cowtombell     : 043      : t
;;;; 13 373 Cowtombell     : 045      : t
;;;; 14 432 ModSawTone     : 072 096  :
;;;; 15 453 Red Sky        : 049      : 
;;;; 16 454 JungleNite     : 051      :
;;;; 17 169 BackwardSnr4   : 052      : s
;;;; 18 272 Horny Hat      : 066 071  :
;;;; 19 346 Whak Claps     : 039      :
;;;; 20 170 RevSnare 1     : 053      : s
;;;; 21 160 RinTinTin      : 054 056  : 
;;;; 22 422 BongoBoy       : 097 127  :
;;;; 23 337 ErsatScrch     : 057 059  :
;;;; 24 422 BongoBoy       : 060 065  :
;;;;

;;;; 14 432 ModSawTone     : 072 096  :
;;;; 22 422 BongoBoy       : 097 127  :

(let ((snare-keys '((x     . (38))
		    (tambo . (37))
		    (mod   . (40))
		    (rev   . (47))
		    (rev2  . (48))
		    (rev3  . (50))
		    (rev4  . (52))
		    (rim   . (53))
		    (claps . (39))))
      (composite-keys '((snare     . (38))
			(tambosnr . (37))
			(modsnr   . (40))
			(revsnr   . (47))
			(revsnr2  . (48))
			(revsnr3  . (50))
			(revsnr4  . (52))
			(rim   . (53))
			(claps . (39))
			(kick . (36))
			(hh . (42))
			(hh-lazer . (44))
			(hh-opn . (46))
			(horny-hat   . (66))
			(horny-hat-2 . (67))
			(horny-hat-3 . (68))
			(horny-hat-4 . (69))
			(horny-hat-5 . (70))
			(horny-hat-6 . (71))
			(cowtom   . (41))
			(cowtom-2 . (43))
			(cowtom-3 . (45))
			(junglenite . (51))
			(redsky   . (49))
			(rintin   . (54))
			(rintin-2 . (55))
			(rintin-3 . (56))
			(scratch   . (57))
			(scratch-2 . (58))
			(scratch-3 . (59))
			(bongo     . (60))
			(bongo-1   . (61))
			(bongo-2   . (62))
			(bongo-3   . (63))
			(bongo-4   . (64))
			(bongo-5   . (65)))))
  (defun hip-house (&key (parent procussion) channel)
    (let ((hh (make-instrument 'hip-house
			      :parent parent
			      :channel channel
			      :keynumber-map (procussion-keymap  36 127 composite-keys))))
      (defparameter hh-snare (make-instrument 'hh-snare
					      :parent hh
					      :keynumber-map (procussion-subkey-map snare-keys)))
      (defparameter hh-drums (make-instrument 'hh-drums
					      :parent hh
					      :keynumber-map (procussion-keymap 97 127)))
      (defparameter hh-saw (make-instrument 'hh-saw
					    :parent hh
					    :keynumber-map (procussion-keymap 72 96)))
      (defparameter hip-house hh)
      hh))) 
			
      
		  
      
