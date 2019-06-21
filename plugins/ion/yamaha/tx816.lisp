;;;; CYCO plugins ion yamaha tx816
;;;;
;;;; The TX816 is an FM beast. When fully loaded it is equivalent to 8
;;;; DX7s.  This specific instrument has 6 modules which are always used in
;;;; pairs.  Loading the plugin creates a TX816 instrument and three child
;;;; instruments: TXA, TXB and TXC.
;;;;
;;;; TX816
;;;;  |
;;;;  +-- TXA
;;;;  +-- TXB
;;;;  +-- TXC
;;;;
;;;; Corresponding macros (TXA, TXB, TXC) and info functions (?TXA, ?TXB,
;;;; ?TXC) are defined.  Calling one of the info functions displays a list
;;;; of the available programs
;;;;
;;;; Calling one of the instrument macros (TXA, TXB or TXC), binds a new
;;;; instrument to a symbol with the same name as the name argument and
;;;; places it as a child of the appropriate "module" instruments.
;;;;
;;;; The TX memories are a linear list of programs between 0 and 31
;;;; inclusive.  Program numbers may be specified in one of two ways:
;;;;
;;;;   1  If the name argument matches a defined program the corresponding
;;;;      program-number is used.
;;;;   2  Directly via the :program keyword argument.
;;;;


(instrument tx816 :parent *root-instrument* :transient nil)
(instrument txa :parent tx816 :channel (meta-channel :txa) :transient nil)
(instrument txb :parent tx816 :channel (meta-channel :txb) :transient nil)
(instrument txc :parent tx816 :channel (meta-channel :txc) :transient nil)

(let ((txa-programs '((BASS-A1     0 "BASS-1        Bass-Keys ")
		      (BASS-A2     1 "BASS-1        BASS1EDBOW")
		      (BASS-A3     2 "BASS-1        Best Bass ") 
		      (BASS-A4     3 "BASS-1        FrtLess II") 
		      (BASS-A5     4 "BASS-1        FrtLess IB") 
		      (BASS-A6     5 "BASS-1        FrtLess IC") 
		      (BASS-B1     6 "BASS-MOVIE    BASS THUMB") 
		      (BASS-C1     7 "BASS2-PULZ    BASS1EDBOW") 
		      (BASS-C2     8 "BASS2-PULZ    Best Bass ") 
		      (BASS-C3     9 "BASS2-PULZ    FrtLess II") 
		      (BASS-C4    10 "BASS2-PULZ    FrtLess IB") 
		      (FRTLSS-2   11 "FrtLess IB    FrtLess II") 
		      (FRTLSS-1B  12 "FrtLess IB    FrtLess IB") 
		      (SUBHARM    13 "SUBASS1       SB1Harmnc ") 
		      (BASSORCH   14 "FlatBass      5thFlat   ") 
		      (SIMPAD     15 "SIMPSYNTH1    ODDSYN    ") 
		      (INIVOICE   31 "INVOICE       INVOICE   ")))
      (txb-programs '((PIANO-21    0   "PIANO 21A   PIANO 21B ")
		      (PIANO-26    1   "PIANO 26A   PIANO 26B ")
		      (RHODES      2   "RHODESdb1   RHODESdb2 ")
		      (INIVOICE    3   "INIVOICE    INIVOICE  ")
		      (VIBES       4   "MBOX1       CELESTA1  ")
		      (GLOCKEN     5   "GLOCKEN  1  GLOCKEN  2")
		      (DIRT-PAD    6   "SIMPSYNTH2  DIRTAMB   ")
		      (MULTI-VIBE  7   "NOTYNE      MULTIVIBE ")
		      (HI-BELL     8   "HiBells01   HiBell02  ")
		      (TUBE-BELL   9   "TUB BELLS   SharpBells")
		      (INIVOICE   31   "INIVOICE    INIVOICE  ")))
      (txc-programs '((HI-PIANO      0   "MelPno01      HiPno01   ")
		      (PIANO-CP70    1   "CLEANPIANO    YAMAHACP70")
		      (PIANO-26      2   "PIANO 26A     PIANO 26B ")
		      (INIVOICE      3   "INIVOICE      INIVOICE  ")
		      (VIBES-1       4   "VIBE1A        VIBE1B    ")
		      (ICE9          5   "ICE9A         ICE9B     ")
		      (GLASSICE      6   "GLASS  A      ICE9B     ")
		      (GLASS         7   "GLASS  A      GLASS  B  ")
		      (FATSYNTH      8   "NOJOKE        FATSYNTHA2")
		      (SLO-GLASS1    9   "SlowGlass     SlowIce   ")
		      (PLK          10   "Pluck02       StrumNse2 ")
		      (BELL-PAD     11   "BellPad       SilkPad   ")
		      (SCHIMMER     12   "Schimmer01    Schimmer02")
		      (BRIGHT-ORGAN 13   "MayDayOrgn    maySoap   ")
		      (SLO-GLASS2   14   "SLW GLS A     LW GLS B  ")
		      (INIVOICE     31   "INIVOICE      INIVOICE  ")))
      (default-program 0))
  (macrolet ((def-docfn
	      (fname iname plist)
	      `(defun ,fname ()
		 (format t "~A Programs:~%" ',iname)
		 (dolist (q ,plist)
		   (format t "[~2D] ~13A  Remarks ~S~%"
			   (second q)(first q)(third q)))))

	     (def-program-function
	      (fname iname pmap)
	      `(defun ,fname (prg)
		 (let ((q (assoc prg ,pmap)))
		   (or (and (integerp prg)(<= 1 prg)(<= prg 32)(1- prg))
		       (and q (second q))
		       (progn
			 (cyco-warning
			  (sformat "Invalid ~A program: ~A" ',iname prg)
			  (sformat "Using default ~A" default-program))
			 default-program))))) )
    (def-docfn ?txa txa txa-programs)
    (def-docfn ?txb txb txb-programs)
    (def-docfn ?txc txc txc-programs)
    (def-program-function txa-program 'txa txa-programs)
    (def-program-function txb-program 'txb txb-programs)
    (def-program-function txc-program 'txc txc-programs)
    
    (defmacro txa (name &key program keynumber-map articulation-map dynamic-map remarks)
      `(let* ((prg (txa-program (or ,program ',name)))
	      (rem (or ,remarks (sformat "TXA program ~A" (1+ prg))))
	      (inst (make-instrument ',name
				     :parent txa
				     :program prg
				     :transient t
				     :keynumber-map ,keynumber-map
				     :articulation-map ,articulation-map
				     :dynamic-map ,dynamic-map
				     :remarks (->string rem))))
	 (defparameter ,name inst)
	 inst))
    
     (defmacro txb (name &key program keynumber-map articulation-map dynamic-map remarks)
       `(let* ((prg (txb-program (or ,program ',name)))
	       (rem (or ,remarks (sformat "TXB program ~A" (1+ prg))))
	       (inst (make-instrument ',name
				      :parent txb
				      :program prg
				      :transient t
				      :keynumber-map ,keynumber-map
				      :articulation-map ,articulation-map
				      :dynamic-map ,dynamic-map
				      :remarks (->string rem))))
	  (defparameter ,name inst)
	  inst))
     
     (defmacro txc (name &key program keynumber-map articulation-map dynamic-map remarks)
       `(let* ((prg (txc-program (or ,program ',name)))
	       (rem (or ,remarks (sformat "TXC program ~A" (1+ prg))))
	       (inst (make-instrument ',name
				     :parent txc
				     :program prg
				     :transient t
				     :keynumber-map ,keynumber-map
				     :articulation-map ,articulation-map
				     :dynamic-map ,dynamic-map
				     :remarks (->string rem))))
	  (defparameter ,name inst)
	  inst)) ))
