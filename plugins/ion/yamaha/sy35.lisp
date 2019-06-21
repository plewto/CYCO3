;;;; CYCO plugins ion yamaha sy35
;;;;
;;;; The SY35 is at the low-end of Yamaha's "vector" synths.  It is little
;;;; more the a rompler combined with cheap FM and a joystick.   Memory is
;;;; divided into three spaces:
;;;;
;;;;    1  "Internal" - User RAM
;;;;    2  "Card"     - External ram/rom cards
;;;;    3  "Preset"   - Factory ROM
;;;;
;;;; Each of these spaces is organized into 8 banks of 8 programs each.
;;;; Unfortunately they neglected  to include a means of selecting which
;;;; memory to use via external MIDI commands; you have to do it manually.
;;;;
;;;; This plugin defines a main "SY35" instrument and a macro to add
;;;; instruments under it.  Two sets of programs are defines; one for
;;;; internal and the other for preset memories.  Card memory is ignored. 
;;;;
;;;; Calling the SY35 macro binds a new instrument to a symbol with the
;;;; same name as the name argument.   Program numbers are specified in one
;;;; of three ways:
;;;; 
;;;;   1  If the name matches a defined program name the program-number is
;;;;      automatically set.  Use (?SY35) or (?SY35 :ROM) for list or
;;;;      programs. 
;;;;   2  Directly as a MIDI program number 0..63
;;;;   3  List of form (BANK PROGRAM) where both bank and program are
;;;;      integers between 1 and 8 inclusive.
;;;;

(instrument sy35
	    :parent +root-instrument+
	    :channel (meta-channel :sy35)
	    :transient nil)

(let ((ram '((wide    . (1 1 "ME*Wide"))    ;; "Internal" RAM
	     (slwsg   . (1 2 "ME*Slwsg"))
	     (tzone   . (1 3 "ME*Tzone"))
	     (space   . (1 4 "ME*Space"))
	     (filter  . (1 5 "SP*Filter"))
	     (deep    . (1 6 "SP*Deep"))
	     (fog     . (1 7 "SP*Fog"))
	     (dyna    . (1 8 "SP*Dyna"))
	     (ice35   . (2 1 ""))
	     (Elgnt   . (2 2 ""))
	     (richpad . (2 3 ""))
	     (coin    . (2 4 ""))
	     (brash   . (2 5 ""))
	     (water   . (2 6 ""))
	     (bell-y  . (2 7 ""))
	     (tzone2  . (2 8 ""))
	     (saw     . (3 1 ""))
	     (sqaure  . (3 2 ""))
	     (sync    . (3 3 ""))
	     (power   . (3 4 ""))
	     (5th     . (3 5 ""))
	     (2VCO    . (3 6 ""))
	     (fat     . (3 7 ""))
	     (anasyn  . (3 8 ""))
	     (cat     . (4 1 "Organ"))
	     (organ1  . (4 2 "Rock organ"))
	     (organ2  . (4 3 "Rock organ"))
	     (organ3  . (4 4 "Full organ"))
	     (vibes   . (4 5 ""))
	     (mbox    . (4 6 "Music box"))
	     (gospel  . (4 7 "Organ"))
	     (soap    . (4 8 "Organ"))
	     (strings1 . (5 1 "Modern"))
	     (strings2 . (5 2 "Soft"))
	     (strings3 . (5 3 "Mild"))
	     (strings4 . (5 4 "Light"))
	     (strings5 . (5 5 "Ensamble"))
	     (org      . (5 6 "organ"))
	     (init     . (5 7 ""))
	     (init     . (5 8 ""))
	     (piano1   . (6 1 "Rock piano"))
	     (piano2   . (6 2 "Classic acoustic"))
	     (piano3   . (6 3 "Chorus"))
	     (piano4   . (6 4 "Bell piano"))
	     (epiano1  . (6 5 "Light electric piano"))
	     (epiano2  . (6 6 "Old electric piano"))
	     (epiano3  . (6 7 "Mallet+EPiano"))
	     (clav1    . (6 8 ""))
	     (brass1   . (7 1 "Brass comb"))
	     (init     . (7 2 ""))
	     (init     . (7 3 ""))
	     (init     . (7 4 ""))
	     (init     . (7 5 ""))
	     (init     . (7 6 ""))
	     (init     . (7 7 ""))
	     (init     . (7 8 ""))
	     (init     . (8 1 ""))
	     (init     . (8 2 ""))
	     (init     . (8 3 ""))
	     (init     . (8 4 ""))
	     (init     . (8 5 ""))
	     (init     . (8 6 ""))
	     (init     . (8 7 ""))
	     (testtone . (8 8 "sine"))))
      (rom '((piano1   . (1 1 "Rock piano")) ;; "Preset" factory ROM
	     (piano2   . (1 2 "Classic piano"))
	     (piano3   . (1 3 "Chorus piano"))
	     (piano4   . (1 4 "Honky Tonk"))
	     (piano5   . (1 5 "Soft Piano"))
	     (piano6   . (1 6 "? piano"))
	     (piano7   . (1 7 "Blend piano"))
	     (piano8   . (1 8 "Bell piano"))
	     (etine    . (2 1 "EPiano tine"))
	     (epiano1  . (2 2 "Light electric piano"))
	     (epiano2  . (2 3 "Old electric piano"))
	     (epiano3  . (2 4 "Mallet+EPiano"))
	     (clav1    . (2 5 ""))
	     (clav2    . (2 6 ""))
	     (celst    . (2 7 "Celeste"))
	     (harpsi   . (2 8 "Harpsichord"))
	     (trumpet  . (3 1 ""))
	     (mute     . (3 2 "Muted trumpet"))
	     (trombone . (3 3 ""))
	     (flugel   . (3 4 "Flugel Horn"))
	     (french   . (3 5 "French Horn"))
	     (br-sec1  . (3 6 "Brass section 1"))
	     (br-sec2  . (3 7 "Brass Section 2"))
	     (fanfare  . (3 8 "Brass Fanfare"))
	     (arco1   . (4 1 "Strings"))
	     (arco2   . (4 2 "Strings"))
	     (cello   . (4 3 "Strings"))
	     (slwstr  . (4 4 "Slow attack strings"))
	     (pizz    . (4 5 "Pizzicato"))
	     (trem    . (4 6 "Tremolo strings"))
	     (storch1 . (4 7 "String orchestra 1"))
	     (storch2 . (4 8 "String orchestra 2"))
	     (wood     . (5 1 "Bass"))
	     (fretless . (5 2 "Bass"))
	     (slap     . (5 3 "Bass"))
	     (finger   . (5 4 "Bass"))
	     (pick     . (5 5 "Bass"))
	     (bsynth   . (5 6 "Bass synth"))
	     (techno   . (5 7 "Bass synth"))
	     (groov    . (5 8 "Bass synth"))
	     (sax      . (6 1 ""))
	     (flute    . (6 2 ""))
	     (clarinet . (6 3 ""))
	     (oboe     . (6 4 ""))
	     (panflute . (6 5 ""))
	     (saxens   . (6 6 "Sax ensemble"))
	     (wnens    . (6 7 "Wind ensemble"))
	     (wnorch   . (6 8 "Wind orchestra"))
	     (gypsy    . (7 1 "Guitar"))
	     (folk     . (7 2 "Guitar"))
	     (wide-gtr . (7 3 "Guitar"))
	     (mute-gtr . (7 4 "Muted guitar"))
	     (rock-gtr . (7 5 "Rock guitar"))
	     (dist-gtr . (7 6 "Distorted guitar"))
	     (chrng    . (7 7 "?"))
	     (sitar    . (7 8 ""))
	     (pure    . (8 1 "Chorus"))
	     (itopy   . (8 2 "Chorus"))
	     (uhh     . (8 3 "Chorus"))
	     (angel   . (8 4 "Chorus"))
	     (bellchr . (8 5 "Bell Chorus"))
	     (snow    . (8 6 "Chorus"))
	     (vcodr   . (8 7 "Vocoder"))
	     (marin   . (8 8 "? Voice")))) )

  (defun ?sy35 (&optional (memory :ram))
    (let* ((membank (cond ((eq memory :rom)
			   (format t "SY35 ROM (Preset) Programs:~%")
			   rom)
			  ((eq memory :ram)
			   (format t "SY35 RAM (Internal) Programs:~%")
			   ram)
			  (t
			   (cyco-warning
			    "Expected :RAM or :ROM as memory argument to ?SY35"
			    (sformat "Encounterd ~A" memory))
			   (return-from ?sy35))))
	   (frmt "[~3D]  ~D.~D  ~12A ~20S ")
	   (pary (->vector membank)))
      (dotimes (i 32)
	(let* ((j (+ i 32))
	       (p1 (aref pary i))
	       (p2 (aref pary j)))
	  
	  (dolist (p (list p1 p2))
	    (let* ((name (car p))
		   (bank (second p))
		   (pnum (third p))
		   (rem (fourth p))
		   (program-number (+ (* 8 (1- bank))(1- pnum))))
	      (format t frmt program-number bank pnum name rem)))
	  (format t "~%")
	  (if (= (rem i 8) 7)
	      (format t "~%"))))))    

  ;; prg integer  --> MIDI program number
  ;; prg (bank p) --> use SY bank/program 1..8, 1..8
  ;; prg symbol   --> alist key
  (defun sy35-program (prg &optional (memory :ram))
    (flet ((warnfn ()
		   (cyco-warning
		    (sformat "Invalid SY35 program: ~A" prg)
		    "Using program 0")
		   0))
      (let ((mem (cond ((eq memory :ram) ram)
		       ((eq memory :rom) rom)
		       (t (cyco-warning
			   "Expected :RAM or :ROM as memory argument to SY35-PROGRAM"
			   (sformat "Encounterd: ~A" memory)
			   "Using default :ram")
			  ram))))
	(cond ((and (integerp prg)(<= 0 prg)(<= prg 64))
	       prg)
	      ((listp prg)
	       (let ((bnk (car prg))
		     (pn (second prg)))
		 (if (and (integerp bnk)(<= 1 bnk)(<= bnk 8)
			  (integerp pn)(<= 1 pn)(<= pn 8))
		     (+ (* 8 (1- bnk))(1- pn))
		   (warnfn))))
	      (t (let ((q (cdr (assoc prg mem))))
		   (if q
		       (let ((bnk (first q))
			     (pn (second q)))
			 (sy35-program (list bnk pn)))
		     (warnfn)))))))) )

(defmacro sy35 (name &key program (memory :ram)
		     keynumber-map articulation-map dynamic-map remarks)
  `(let* ((prg (sy35-program (or ,program ',name) ,memory))
	  (bnk (1+ (truncate (/ prg 8))))
	  (pnum (1+ (rem prg 8)))
	  (rem (or ,remarks (sformat "SY35 program ~A ~A.~A" ,memory bnk pnum)))
	  (inst (make-instrument ',name
				:parent sy35
				:program prg
				:transient t
				:keynumber-map ,keynumber-map
				:articulation-map ,articulation-map
				:dynamic-map ,dynamic-map
				:remarks (->string rem))))
     (defparameter ,name inst)
     inst))