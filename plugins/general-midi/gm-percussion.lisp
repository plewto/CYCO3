;;;; Defines General MIDI percussion instruments; both as a single unified
;;;; instrument and as individual instrument groups.
;;;;
;;;;  gm-percussion
;;;;      |
;;;;      +-- gm-cowbell
;;;;      +-- gm-woodblock
;;;;      +-- gm-cymbal
;;;;      +-- gm-drum
;;;;      +-- gm-hihat
;;;;      +-- gm-shaker
;;;;      +-- gm-snare
;;;;      +-- gm-kick
;;;;      +-- gm-cuica
;;;;      +-- gm-timbale
;;;;      +-- gm-tom
;;;;

(constant +general-midi-percussion-keylist+ 
	  '((kick1 . (35))
	    (kick2 . (36))
	    (stick . (37))
	    (snare1 . (38))
	    (clap . (39))
	    (snare2 . (40))
	    (tom1 . (41))
	    (hat-closed . (42))
	    (tom2 . (43))
	    (hat-ped . (44))
	    (tom3 . (45))
	    (hat-open . (46))
	    (tom4 . (47))
	    (tom5 . (48))
	    (crash1 . (49))
	    (tom6 . (50))
	    (ride1 . (51))
	    (chinese . (52))
	    (ride-bell . (53))
	    (tambourine . (54))
	    (splash . (55))
	    (cow . (56))
	    (crash2 . (57))
	    (vibraslap . (58))
	    (ride2 . (59))
	    (bongo-high . (60))
	    (bongo-low . (61))
	    (conga-high . (62))
	    (conga-open . (63))
	    (conga-low . (64))
	    (timbale-high . (65))
	    (timbale-low . (66))
	    (agogo-high . (67))
	    (agogo-low . (68))
	    (cabasa . (69) )
	    (maracas . (70) )
	    (whistle1  . (71))
	    (whistle2  . (72))
	    (guiro-short . (73) )
	    (guiro-long . (74) )
	    (clave . (75))
	    (block-high . (76))
	    (block-low . (77))
	    (cuica-mute . (78))
	    (cuica-open . (79))
	    (triangle-mute . (80))
	    (triangle . (81))))

(constant +general-midi-percussion-keymap+
	  (symbolic-keynumber-map +general-midi-percussion-keylist+))

(defun general-midi-drum-keylist (&optional modifications)
  "Returns general-midi drum kit key assignment association list.
If optional modifications is specified, it should be an association list 
of form ((symbol-1 . (keynumber-1))(symbol-2 . (keynumber-2)) ...)
The modification list is appended to the default key-list.   If a default 
symbol appears in the modification list, the default assignment is replaced 
with the modified one."
  (let ((acc (clone +general-midi-percussion-keylist+)))
    (dolist (entry modifications)
      (setf acc (remove entry acc :test #'(lambda (a b)(eq (car a)(car b))))))
    (append acc modifications)))

(instrument gm-percussion
	    :parent +root-instrument+
	    :transient nil
	    :channel 10
	    :program 0
	    :bank nil
	    :keynumber-map +general-midi-percussion-keymap+
	    :remarks "General MIDI percussion")

(constant +gm-cowbell-keymap+ (symbolic-keynumber-map
			       '((cow . (56))
				 (hi-agogo . (67))
				 (low-agogo . (68))
				 (triangle . (81))
				 (mute-triangle . (80)))))

(instrument gm-cowbell
	    :parent gm-percussion
	    :transient nil
	    :keynumber-map +gm-cowbell-keymap+)

(constant +gm-woodblock-keymap+ (symbolic-keynumber-map
				 '((hi . (76))
				   (low . (77))
				   (clave . (75)))))

(instrument gm-woodblock
	    :parent gm-percussion
	    :transient nil
	    :keynumber-map +gm-woodblock-keymap+
	    :remarks "Woodblocks & clave")

(constant +gm-cymbal-keymap+ (symbolic-keynumber-map
			      '((ride1 . (51))
				(ride2 . (59))
				(bell . (53))
				(crash1 . (49))
				(crash2 . (57))
				(chinese . (52))
				(splash . (55)))))
(instrument gm-cymbal
	    :parent gm-percussion
	    :transient nil
	    :keynumber-map +gm-cymbal-keymap+)

(constant +gm-drum-keymap+ (symbolic-keynumber-map
			    '((conga-low . (64))
 			      (conga-hi . (62))
			      (conga-open . (63))
			      (bongo-low . (61))
			      (bongo-hi . (60)))))

(instrument gm-drum
	    :parent gm-percussion
	    :transient nil
	    :keynumber-map +gm-drum-keymap+
	    :remarks "Conga and Bongo")

(constant +gm-hihat-keymap+ (symbolic-keynumber-map
			     '((closed . (42))
			       (open . (46))
			       (ped . (44)))))

(instrument gm-hihat
	    :parent gm-percussion
	    :transient nil
	    :keynumber-map +gm-hihat-keymap+)

(constant +gm-shaker-keymap+ (symbolic-keynumber-map
			      '((cabasa . (69) )
				(guiro-short . (73) )
				(guiro-long . (74) )
				(maracas . (70) )
				(tambourine . (54))
				(vibraslap . (58))
				(whistle1  . (71))
				(whistle2  . (72)))))

(instrument gm-shaker
	    :parent gm-percussion
	    :transient nil
	    :keynumber-map +gm-shaker-keymap+
	    :remarks "Cabasa, Guiro, maracas, vibraslap & whistle")

(constant +gm-snare-keymap+ (symbolic-keynumber-map
			     '((x1 . (38))
			       (x2 . (40))
			       (stick . (37))
			       (clap . (39)))))

(instrument gm-snare
	    :parent gm-percussion
	    :transient nil
	    :keynumber-map +gm-snare-keymap+)

(constant +gm-kick-keymap+ (symbolic-keynumber-map
			    '((x1 . (35))
			      (x2 . (36)))))

(instrument gm-kick
	    :parent gm-percussion
	    :transient nil
	    :keynumber-map +gm-kick-keymap+)

(constant +gm-cuica-keymap+ (symbolic-keynumber-map
			     '((open . (79))
			       (mute . (78)))))

(instrument gm-cuica
	    :parent gm-percussion
	    :transient nil
	    :keynumber-map +gm-cuica-keymap+)

(constant +gm-timbale-keymap+ (symbolic-keynumber-map
			       '((low . (66))
				 (high . (65)))))

(instrument gm-timbale
	    :parent gm-percussion
	    :transient nil
	    :keynumber-map +gm-timbale-keymap+)

(constant +gm-tom-keymap+ (symbolic-keynumber-map
			   '((x1 . (41))
			     (x2 . (43))
			     (x3 . (45))
			     (x4 . (47))
			     (x5 . (48))
			     (x6 . (50)))))

(instrument gm-tom
	    :parent gm-percussion
	    :transient nil
	    :keynumber-map +gm-tom-keymap+
	    :remarks "General MIDI toms, x1 is largest drum.")
