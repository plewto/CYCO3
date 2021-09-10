;;;; CYCO plugins ion emu procussion standard-kit.lisp
;;;;
;;;; Within all "standard" Procussion kits, specific key numbers are 
;;;; assigned to the same type of instrument.  Thees sub instruments are:
;;;;
;;;;    PKICK   - bass drum
;;;;    PSNARE  - snare drum
;;;;    PTOM    - tom drums
;;;;    PHAT    - high hats
;;;;    PCYM    - cymbals
;;;;    PTUNED  - Some sort of tuned instrument, bass synth, toms, bells...
;;;;
;;;; The exact samples assigned to these instruments are dependent on the
;;;; specific kit. 
;;;;
;;;;
;;;; The Standard Kits
;;;;    ampitheater
;;;;    mega-drums
;;;;    rock-n-roll
;;;;    palladium
;;;;    jazz-drums
;;;;    metal-drums
;;;;    rap-session
;;;;    ambient-rock
;;;;    acoustic-kit
;;;;    rock-drums
;;;;    house-machine
;;;;    fusion-stix
;;;;    space-drums
;;;;    hard-rock
;;;;    stadium-rox
;;;;    dance-2000
;;;;    heavy-metal
;;;;    hip-hop
;;;;    sluggo-drums
;;;;    huge-room
;;;;    drum-dance
;;;;    heavyosity
;;;;    dance-club
;;;;    country-kit
;;;;    rockabilly
;;;;    plankbastard
;;;;
;;;;
;;;;  Standard key numbers
;;;;  key   zone Instrument
;;;;  033 - Z01  Main Kick (Tuned) 
;;;;  034 - Z01  Main Kick (Tuned) 
;;;;  035 - ZO1  Main Kick (Tuned) 
;;;;  036 - ZO1  Main Kick (Tuned) 
;;;;  037 - Z04  Aux Snare 1
;;;;  038 - Z03  Main Snare
;;;;  039 - Z05  Aux Snare 2
;;;;  040 - Z19  Tom (Tuned)
;;;;  041 - Z11  Tom (Tuned)
;;;;  042 - Z07  HH Closed
;;;;  043 - Z12  Tom (Tuned)
;;;;  044 - Z08  HH Open 
;;;;  045 - Z19  Tom (Tuned)
;;;;  046 - Z10  Percussion
;;;;  047 - Z19  Tom (Tuned)
;;;;  048 - Z15  Choke Cymbal (or other percussion)
;;;;  049 - Z16  Cymbal 1 (Crash or ride)
;;;;  050 - Z17  Aux Ride
;;;;  051 - Z18  Cymbal 2 (Crash or Ride)
;;;;  052 - Z09  HH Stack (Controller A)
;;;;  053 - Z02  Kick (Nontransposed)
;;;;  054 - Z02  Kick (Nontransposed)
;;;;  055 - Z02  Kick (Nontransposed)
;;;;  056 - Z02  Kick (Nontransposed)
;;;;  057 - Z02  Kick (Nontransposed)
;;;;  058 - Z02  Kick (Nontransposed)
;;;;  059 - Z06  Snare (Nontransposed)
;;;;  060 - Z06  Snare (Nontransposed)
;;;;  061 - Z06  Snare (Nontransposed)
;;;;  062 - Z06  Snare (Nontransposed)
;;;;  063 - Z06  Snare (Nontransposed)
;;;;  064 - Z06  Snare (Nontransposed)
;;;;  065 - Z21  HH Stomp
;;;;  066 - Z21  HH Stomp
;;;;  067 - Z22  HH Shut
;;;;  068 - Z23  HH 2/3 Open
;;;;  069 - Z23  HH 2/3 Open
;;;;  070 - Z24  HH Open
;;;;  071 - Z24  HH Open
;;;;  072 - Z14  Tuned tom, bass or other instrument
;;;;  073 - Z14  Tuned tom, bass or other 
;;;;  074 - Z14  Tuned tom, bass or other
;;;;  075 - Z14  Tuned tom, bass or other
;;;;  076 - Z14  Tuned tom, bass or other
;;;;  077 - Z14  Tuned tom, bass or other
;;;;  078 - Z14  Tuned tom, bass or other
;;;;  079 - Z14  Tuned tom, bass or other
;;;;  080 - Z14  Tuned tom, bass or other
;;;;  081 - Z14  Tuned tom, bass or other
;;;;  082 - Z14  Tuned tom, bass or other
;;;;  083 - Z14  Tuned tom, bass or other
;;;;  084 - Z14  Tuned tom, bass or other
;;;;  085 - Z14  Tuned tom, bass or other
;;;;  086 - Z14  Tuned tom, bass or other
;;;;  087 - Z14  Tuned tom, bass or other
;;;;  088 - Z14  Tuned tom, bass or other
;;;;  089 - Z14  Tuned tom, bass or other
;;;;  090 - Z14  Tuned tom, bass or other
;;;;  091 - Z14  Tuned tom, bass or other
;;;;  092 - Z14  Tuned tom, bass or other
;;;;  093 - Z14  Tuned tom, bass or other
;;;;  094 - Z14  Tuned tom, bass or other
;;;;  095 - Z14  Tuned tom, bass or other
;;;;  096 - Z14  Tuned tom, bass or other
;;;;  097 - Z14  Tuned tom, bass or other
;;;;  098 - Z14  Tuned tom, bass or other
;;;;

(param ampitheater nil)
(param mega-drums nil)
(param rock-n-roll nil)
(param palladium nil)
(param jazz-drums nil)
(param metal-drums nil)
(param rap-session nil)
(param ambient-rock nil)
(param acoustic-kit nil)
(param rock-drums nil)
(param house-machine nil)
(param fusion-stix nil)
(param space-drums nil)
(param hard-rock nil)
(param stadium-rox nil)
(param dance-2000 nil)
(param heavy-metal nil)
(param hip-hop nil)
(param sluggo-drums nil)
(param huge-room nil)
(param drum-dance nil)
(param heavyosity nil)
(param dance-club nil)
(param country-kit nil)
(param rockabilly nil)
(param plankbastard nil)
(param pkick nil)
(param psnare nil)
(param ptom nil)
(param phat nil)
(param pcym nil)
(param ptuned nil)


;;; ---------------------------------------------------------------------- 
;;;			Standard "sub" instruments
;;;
;;; The following functions create the sub-instruments for a standard kit.
;;; Executing one of these functions:
;;;
;;;   1) Creates an instrument by the same name.
;;;   2) Links that instrument to the parent kit.
;;;   3) Binds the instrument to a symbol with the same name.
;;;

(let ((hold4 (constant-articulation-map 4.0))
      (hold8 (constant-articulation-map 8.0)))

  (defun pkick (parent &key dynamic-map)
    (instrument pkick
		:parent parent
		:remarks "Standard Emu Procussion Kick drum"
		:transient t
		:keynumber-map (symbolic-keynumber-map
				'((A . (34))
				  (B . (33))
				  (C . (35))
				  (D . (36))))
	      :articulation-map hold4
	      :dynamic-map dynamic-map))

  ;; The "MOR2 Compatibility" remarks indicate these assignments
  ;; are compatible with the Ministry of Rock drums.
  ;;
  (defun psnare (parent &key dynamic-map)
    (instrument psnare
		:parent parent
		:remarks "Standard Emu Procussion Snare"
		:transient t
		:keynumber-map (procussion-subkey-map
				'((A       . (38 "Main Snare"))
				  (B       . (37 "Alternate Snare 1"))
				  (C       . (39 "Alternate Snare 2"))
				  (D       . (46 "Percussion"))
				  (X       . (38 "MOR2 Compatibility --> A"))
				  (RIM     . (46 "MOR2 Compatibility --> D"))
				  (CRACK   . (46 "MOR2 Compatibility --> D"))
				  (EDGE    . (37 "MOR2 Compatibility --> B"))
				  (BOUNCE  . (39 "MOR2 Compatibility --> C"))
				  (FLAM    . (39 "MOR2 Compatibility --> C"))
				  (ROLL    . (39 "MOR2 Compatibility --> C"))
				  (X2      . (38 "MOR2 Compatibility --> A"))
				  (RIM2    . (46 "MOR2 Compatibility --> D"))
				  (CRACK2  . (46 "MOR2 Compatibility --> D"))
				  (EDGE2   . (37 "MOR2 Compatibility --> B"))))
		:articulation-map hold4
		:dynamic-map dynamic-map))

  (defun ptom (parent &key dynamic-map)
    (instrument ptom
		:parent parent
		:remarks "Standard Emu Procussion Toms"
		:transient t
		:keynumber-map (procussion-subkey-map
				'((A         . (40 "Low tom"))
				  (B         . (41))
				  (C         . (43))
				  (D         . (45))
				  (E         . (47 "High tom"))
				  (F         . (47 "MOR2 Compatibility --> E"))
				  (A-FLAM    . (40 "MOR2 Compatibility"))
				  (B-FLAM    . (41 "MOR2 Compatibility"))
				  (C-FLAM    . (43 "MOR2 Compatibility"))
				  (D-FLAM    . (45 "MOR2 Compatibility"))
				  (E-FLAM    . (47 "MOR2 Compatibility"))
				  (F-FLAM    . (47 "MOR2 Compatibility"))
				  (A-BOUNCE  . (40 "MOR2 Compatibility"))
				  (B-BOUNCE  . (41 "MOR2 Compatibility"))
				  (C-BOUNCE  . (43 "MOR2 Compatibility"))
				  (D-BOUNCE  . (45 "MOR2 Compatibility"))
				  (E-BOUNCE  . (47 "MOR2 Compatibility"))
				  (F-BOUNCE  . (47 "MOR2 Compatibility"))))
		:articulation-map hold4
		:dynamic-map dynamic-map))
  
  (defun phat (parent &key dynamic-map)
    (instrument phat
		:parent parent
		:remarks "Standard Emu Procussion Hi Hats"
		:transient t
		:keynumber-map (procussion-subkey-map
				'((X        . (42))
				  (OPEN     . (44))
				  (PED      . (65))
				  (SHUT     . (67))
				  (OPN      . (68 "2/3 open"))
				  (STACK    . (52 "Mod wheel controlled?"))
				  (PED2     . (66))
				  (OPN2     . (69))
				  (OPEN2    . (70))
				  (OPEN3    . (71))))
		:articulation-map hold4
		:dynamic-map dynamic-map))

  (defun pcym (parent &key dynamic-map)
    (instrument pcym
		:parent parent
		:remarks "Standard Emu Procussion Cymbals"
		:transient t
		:keynumber-map (procussion-subkey-map
				'((RIDE    . (50))
				  (A       . (49))
				  (B       . (51))
				  (C       . (48 "Sometimes Choke"))))
		:articulation-map hold8
		:dynamic-map dynamic-map))

  (defun ptuned (parent &key articulation-map dynamic-map)
    (instrument ptuned
		:parent parent
		:remarks "Standard Emu Procussion Tuned instrument."
		:transient t
		:keynumber-map (basic-keynumber-map :min 72 :max 98)
		:articulation-map (or articulation-map
				      +default-articulation-map+)
		:dynamic-map dynamic-map)) )



;;; The following list is a composite of the sub-instrument key
;;; assignments.  Some names have been changed to avoid conflict.
;;; i.e. SNARE-A and KICK-A instead of just A.
;;;
;;; These assignments are used with the parent instrument, not 100% needed
;;; but useful.
;;;
(constant +STANDARD-PROCUSSION-KEY-MAP+
	  (procussion-keymap 33 98 '((KICK-A        . (34))
				     (KICK-B        . (33))
				     (KICK-C        . (35))
				     (KICK-D        . (36))
				     (SNARE-A       . (38 "Main Snare"))
				     (SNARE-B       . (37 "Alternate Snare 1"))
				     (SNARE-C       . (39 "Alternate Snare 2"))
				     (SNARE-D       . (46 "Percussion"))
				     (TOM-A         . (40 "Low tom"))
				     (TOM-B         . (41))
				     (TOM-C         . (43))
				     (TOM-D         . (45))
				     (TOM-E         . (47 "High tom"))
				     (HAT-X         . (42))
				     (HAT-OPEN      . (44))
				     (HAT-PED       . (65))
				     (HAT-SHUT      . (67))
				     (HAT-OPN       . (68 "2/3 open"))
				     (HAT-STACK     . (52 "Mod wheel controlled?"))
				     (HAT-PED2      . (66))
				     (HAT-OPN2      . (69))
				     (HAT-OPEN2     . (70))
				     (HAT-OPEN3     . (71))
				     (CYM-RIDE      . (50))
				     (CYM-A         . (49))
				     (CYM-B         . (51))
				     (CYM-C         . (48 "Sometimes Choke"))
				     (C0      . (72 "Tuned instrument"))
				     (CS0     . (73 "Tuned instrument"))
				     (D0      . (74 "Tuned instrument"))
				     (DS0     . (75 "Tuned instrument"))
				     (E0      . (76 "Tuned instrument"))
				     (F0      . (77 "Tuned instrument"))
				     (FS0     . (78 "Tuned instrument"))
				     (G0      . (79 "Tuned instrument"))
				     (GS0     . (80 "Tuned instrument"))
				     (A0      . (81 "Tuned instrument"))
				     (AS0     . (82 "Tuned instrument"))
				     (B0      . (83 "Tuned instrument"))
				     (C1      . (84 "Tuned instrument"))
				     (CS1     . (85 "Tuned instrument"))
				     (D1      . (86 "Tuned instrument"))
				     (DS1     . (87 "Tuned instrument"))
				     (E1      . (88 "Tuned instrument"))
				     (F1      . (89 "Tuned instrument"))
				     (FS1     . (90 "Tuned instrument"))
				     (G1      . (91 "Tuned instrument"))
				     (GS1     . (92 "Tuned instrument"))
				     (A1      . (93 "Tuned instrument"))
				     (AS1     . (94 "Tuned instrument"))
				     (B1      . (95 "Tuned instrument"))
				     (C2      . (96 "Tuned instrument"))
				     (CS2     . (97 "Tuned instrument"))
				     (D2      . (98 "Tuned instrument")))))

;;; ---------------------------------------------------------------------- 
;;;			  Standard Kit Functions
;;;
;;; The following code creates functions for each of the standard kits.
;;; Calling one of these functions:
;;;
;;; 1) Creates an instrument by the same name.
;;; 2) Sets the appropriate program number.
;;; 3) Creates the standard set of sub instruments; PKICK, PSNARE etc...
;;;    and binds them to symbols of the same name.
;;; 4) Binds the standard kit instrument to a symbol with the same name.
;;;
;;; There is a potential conflict which occurs when two or more standard
;;; kit functions are executed.  The first time a standard kit is created
;;; it's sub instruments are bound to the symbols PKICK, PSNARE etc...  If
;;; a second standard kit is then created, it's sub instruments are bound
;;; to the same symbols, wiping out the original, bindings.  The original
;;; instruments still exist on the orchestra tree, however they are no
;;; longer bound to symbols.  A work around is to explicitly bind the first
;;; set of instruments to some other symbols before creating the second
;;; kit.
;;;

(macrolet ((definst (name)
	     `(progn 
		(defun ,name (&key (parent procussion)(channel  nil))
		  (instrument ,name 
			      :parent parent
			      :transient t
			      :channel channel
			      :program (car (cdr (assoc ',name *PROCUSSION-PROGRAMS*)))
			      :remarks "Standard Procussion kit"
			      :keynumber-map +STANDARD-PROCUSSION-KEY-MAP+)
		  (pkick ,name)
		  (psnare ,name)
		  (ptom ,name)
		  (phat ,name)
		  (pcym ,name)
		  (ptuned ,name))
		,name)))
  (definst ampitheater)
  (definst mega-drums)
  (definst rock-n-roll)
  (definst palladium)
  (definst jazz-drums)
  (definst metal-drums)
  (definst rap-session)
  (definst ambient-rock)
  (definst acoustic-kit)
  (definst rock-drums)
  (definst house-machine)
  (definst fusion-stix)
  (definst space-drums)
  (definst hard-rock)
  (definst stadium-rox)
  (definst dance-2000)
  (definst heavy-metal)
  (definst hip-hop)
  (definst sluggo-drums)
  (definst huge-room)
  (definst drum-dance)
  (definst heavyosity)
  (definst dance-club)
  (definst country-kit)
  (definst rockabilly)
  (definst plankbastard))

