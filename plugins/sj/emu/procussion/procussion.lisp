;;;; CYCO plugins sj emu procussion procussion.lisp
;;;;
;;;; The Emu Procussion is essentially a Proteus dedicated to percussion.
;;;; It has 128 memory slots, the first 64 are factory ROM, the final 64
;;;; are user RAM.  Procussion programs are multi timbrel with up to 24
;;;; "zones" per kit.  Zones operate over specific key ranges which may
;;;; overlap. Several kits are designated as "standard". All such kits
;;;; are composed of a standard set of instruments with specific key
;;;; ranges.
;;;;
;;;; Non-standard kits have a wide variety of sub instruments at various
;;;; key positions.  Finally there are a few tuned instruments such as
;;;; vibraphone.  The standard and tuned instruments are the easiest to
;;;; deal with.  The other kits must be treated more or less individually.


(constant +PROCUSSION-PROGRAMS+
	  '((ampitheater     . (000 standard)) ;* defined
	    (mega-drums      . (001 standard)) ;* defined
	    (rock-n-roll     . (002 standard)) ;* defined
	    (palladium       . (003 standard)) ;* defined
	    (jazz-drums      . (004 standard)) ;* defined
	    (metal-drums     . (005 standard)) ;* defined
	    (rap-session     . (006 standard)) ;* defined
	    (latin-drums     . (007 ))         ;* defined
	    (percussion1     . (008 ))         ;* defined        
	    (toolkit         . (009 ))         ;* defined
	    (ambient-rock    . (010 standard)) ;* defined
	    (acoustic-kit    . (011 standard)) ;* defined
	    (thundadome      . (012 tuned2))   ;* defined  
	    (discoball       . (013 ))
	    (vibrations      . (014 tuned))    ;* defined
	    (rock-drums      . (015 standard)) ;* defined
	    (house-machine   . (016 standard)) ;* defined
	    (fusion-stix     . (017 standard)) ;* defined
	    (found-sound     . (018 ))
	    (marimba         . (019 tuned))    ;* defined
	    (space-drums     . (020 standard)) ;* defined
	    (hard-rock       . (021 standard)) ;* defined
	    (stadium-rox     . (022 standard)) ;* defined
	    (dance-2000      . (023 standard)) ;* defined
	    (industry        . (024 tuned2))
	    (heavy-metal     . (025 standard)) ;* defined
	    (hip-hop         . (026 standard)) ;* defined
	    (rosewood        . (027 tuned))    ;* defined
	    (percussion2     . (028 ))
	    (sound-fx        . (029 ))
	    (sluggo-drums    . (030 standard)) ;* defined
	    (beatbox         . (031 ))         
	    (rocket-drums    . (032 ))
	    (huge-room       . (033 standard)) ;* defined
	    (churchyard      . (034 tuned2))
	    (drum-dance      . (035 standard)) ;* defined
	    (percussion3     . (036 ))
	    (malletbells     . (037 tuned))    ;* defined
	    (hip-house       . (038 ))
	    (latin-layers    . (039 ))
	    (big-band        . (040 not-defined))
	    (multi-fx        . (041 ))
	    (heavyosity      . (042 standard)) ;* defined
	    (ritual-night    . (043 tuned2))
	    (dance-club      . (044 standard)) ;* defined
	    (indo-steel      . (045 tuned))    ;* defined
	    (orch-percussion . (046))
	    (country-kit     . (047 standard)) ;* defined
	    (killer-synth    . (048 tuned))    ;* defined
	    (heavy-hannded   . (049 ))
	    (percussives     . (050 ))         ;* defined
	    (mystic-land     . (051 tuned))    ;* defined
	    (beach-party     . (052 ))
	    (intervallix     . (053 tuned2))
	    (jazzy-traps     . (054 not-defined))
	    (clavarimba      . (055 tuned))    ;* defined
	    (rockabilly      . (056 standard)) ;* defined
	    (control-snares  . (057 ))
	    (lotsa-kicks     . (058 ))
	    (all-snares      . (059 ))
	    (more-toms       . (060 ))
	    (more-cymbals    . (061 )) 	     ;* defined
	    (more-percussion . (062 ))
	    (more-basses     . (063 tuned3))
	    (all-cymbals     . (064)) 	     ;* defined
	    (redsky          . (065))
	    (cymbals2        . (066)) 	     ;* defined
	    (snare-eater     . (067 not-defined))
	    (proshake        . (068))	     ;* defined
	    (plankbastard    . (069 standard)) ;* defined
	    (metronome       . (127))))

(instrument procussion
	    :parent +root-instrument+
	    :transient nil)

(set-symbolic-program-map procussion +procussion-programs+ :offset 0)

(defun procussion-program (prg)
  (or (and (integerp prg)(<= 0 prg)(< prg 128) prg)
      (second (assoc prg +procussion-programs+))
      (progn 
	(cyco-warning
	 (sformat "Invalid Emu Procussion program: ~A" prg)
	 "See +PROCUSSION-PROGRAMS+"
	 "Using default 0")
	0)))

;; ---------------------------------------------------------------------- 
;;		     Top level Procussion instruments.

(instrument PROA
	    :parent procussion
	    :channel (meta-channel :PROA)
	    :transient nil
	    :remarks "Reserved for 'standard' or primary Procussion kit.")

(instrument PROB
	    :parent procussion
	    :channel (meta-channel :PROB)
	    :transient nil
	    :remarks "Reserved for cymbals.")

(load-plugin-file "emu/procussion/standard-kit")
(load-plugin-file "emu/procussion/tuned-instruments")
(load-plugin-file "emu/procussion/latin-drums")
(load-plugin-file "emu/procussion/percussion1")
(load-plugin-file "emu/procussion/toolkit")
(load-plugin-file "emu/procussion/thundadome")
(load-plugin-file "emu/procussion/beatbox")
(load-plugin-file "emu/procussion/percussives")
(load-plugin-file "emu/procussion/more-cymbals")
(load-plugin-file "emu/procussion/all-cymbals")
(load-plugin-file "emu/procussion/cymbals2")
(load-plugin-file "emu/procussion/proshake")
