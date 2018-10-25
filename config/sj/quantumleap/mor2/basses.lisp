;;;; PigIron CYCO sj config quantumleap mor2-bass
;;;;
;;;; Instruments:
;;;;
;;;;    mor2-bass
;;;;        eb2 
;;;;        hofner 
;;;;        jazzman 
;;;;        lakland 
;;;;        musicman 
;;;;        rickenbacker 
;;;;        silvertone 
;;;;        stingray 
;;;;        hofner-fingered 
;;;;        lakland-fingered 
;;;;        silvertone-fingered 
;;;;        stingray-fingered 
;;;;

(defun eb2 (&key (parent ql-bass) channel 
		 keynumber-map articulation-map dynamic-map
		 (remarks "Ministry of Rock Bass"))
  (let ((inst (make-instrument 'eb2
			      :parent parent
			      :channel channel
			      :transient t
			      :remarks remarks
			      :keynumber-map keynumber-map
			      :articulation-map articulation-map
			      :dynamic-map dynamic-map)))
    (--make-ql-program-map inst '((sus "Sustain open RR")
				  (mute "Mute RR")
				  (slide "Slide up sustain")
				  (bend "Bend up sustain")
				  (slide2 "Slide up")
				  trem noise noise2))
    (param eb2 inst)
    inst))


(defun hofner (&key (parent ql-bass) channel 
		    keynumber-map articulation-map dynamic-map
		    (remarks "Ministry of Rock Bass"))
  (let ((inst (make-instrument 'hofner
			       :parent parent
			       :channel channel
			       :transient t
			       :remarks remarks
			       :keynumber-map keynumber-map
			       :articulation-map articulation-map
			       :dynamic-map dynamic-map)))
    (--make-ql-program-map inst '((sus slide trem noise)))
    (param hofner inst)
    inst))
 

(defun jazzman (&key (parent ql-bass) channel 
		     keynumber-map articulation-map dynamic-map
		     (remarks "Ministry of Rock Bass"))
  (let ((inst (make-instrument 'jazzman
			       :parent parent
			       :channel channel
			       :transient t
			       :remarks remarks
			       :keynumber-map keynumber-map
			       :articulation-map articulation-map
			       :dynamic-map dynamic-map)))
    (--make-ql-program-map inst '((SUS     "Sustain")
				  (VIB     "Sustain strong vibrato")
				  (STAC    "Staccato")
				  (SLIDE   "Slide up Sustain slow")
				  (SLIDE2  "Slide up sustain medium")
				  (SLIDE3  "Slide up slow")
				  (SLIDE4  "Slide up medium")
				  (SLIDE5  "Slide up fast")
				  (SLIDE-  "Slide down")
				  (SLIDE8  "Slide up octave")
				  (HARM1   "Harmonics 1")
				  (HARM2   "Harmonics 2")
				  (FX      "Effects")))
    (param jazzman inst)
    inst))

(defun lakland (&key (parent ql-bass) channel 
		     keynumber-map articulation-map dynamic-map
		     (remarks "Ministry of Rock Bass"))
  (let ((inst (make-instrument 'lakland
			       :parent parent
			       :channel channel
			       :transient t
			       :remarks remarks
			       :keynumber-map keynumber-map
			       :articulation-map articulation-map
			       :dynamic-map dynamic-map)))
  (--make-ql-program-map inst '((OPEN   "Open down")
				(OPEN2  "Open up/down dual range")
				(LEG    "Legatto")
				(SLIDE  "Open slide down")
				(SLIDE2 "Open slide down fast")
				(SLIDE3 "Open slide up")
				(SLIDE4 "Open slide up fast")
				(SLIDE5 "Open slide up sustain")
				(OCTAVE "Octave")
				(BRRR   "Burr")
				(MUTE   "Mute down")
				(MUTE2  "Mute Up/Down dual range")
				(MUTE3  "Mute slide down")
				(MUTE4  "Mute slide up")))
  (param lakland inst)
  inst))


(defun musicman (&key (parent ql-bass) channel 
		      keynumber-map articulation-map dynamic-map
		      (remarks "Ministry Of Rock Bass"))
  (let ((inst (make-instrument 'musicman
			       :parent parent
			       :channel channel
			       :transient t
			       :remarks remarks
			       :keynumber-map keynumber-map
			       :articulation-map articulation-map
			       :dynamic-map dynamic-map)))
    (--make-ql-program-map inst '((SUS    "Sustain RR")
				  (OPEN   "Open down")
				  (LEG    "Legato")
				  (SLIDE  "Open slide down")
				  (SLIDE2 "Open slide down fast")
				  (SLIDE3 "Open slide up")
				  (SLIDE4 "Open slide up fast")
				  (SLIDE5 "Open slide up sustain")
				  (OCTAVE "Octave")
				  (BRRR   "BRRR")
				  (MUTE   "Mute down")
				  (MUTERR "Mute RR")
				  (MUTE2  "Mute slide down")
				  (MUTE3  "Mute slide up")))
    (param musicman inst)
    inst))

(defun rickenbacker (&key (parent ql-bass) channel 
			  keynumber-map articulation-map dynamic-map
       				 (remarks "Ministry of Rock Bass"))
  (let ((inst (make-instrument 'rickenbacker
				      :parent parent
				      :channel channel
				      :transient t
				      :remarks remarks
				      :keynumber-map keynumber-map
				      :articulation-map articulation-map
				      :dynamic-map dynamic-map)))
    (--make-ql-program-map inst '((SUS    "Sustain RR")
				  (SLIDE  "Slide up sus")
				  (NOISE  "Noise")
				  (NOISE2 "Noise")))
    (param rickenbacker inst)
    inst))

(defun silvertone (&key (parent ql-bass) channel 
			keynumber-map articulation-map dynamic-map
			(remarks "Ministry of Rock Bass"))
  (let ((inst (make-instrument 'silvertone
			       :parent parent
			       :channel channel
			       :transient t
			       :remarks remarks
			       :keynumber-map keynumber-map
			       :articulation-map articulation-map
			       :dynamic-map dynamic-map)))
  (--make-ql-program-map inst '((SUS    "Sustain RR")
				(LEG    "Legato")
				(SLIDE  "Slide up sustain")
				(SLIDE2 "Slide down")
				(NOISE  "Noise")))
  (param silvertone inst)
  inst))

(defun stingray (&key (parent ql-bass) channel 
		      keynumber-map articulation-map dynamic-map
		      (remarks "Ministry Of Rock Bass"))
  (let ((inst (make-instrument 'stingray
			       :parent parent
			       :channel channel
			       :transient t
			       :remarks remarks
			       :keynumber-map keynumber-map
			       :articulation-map articulation-map
			       :dynamic-map dynamic-map)))
    (--make-ql-program-map inst '((LONG       "Sustain long RR")
				  (SHORT      "Sustain short RR")
				  (MUTE       "Sustain mute RR")
				  (SUS+HAMMER "Sustain (link slide)")
				  (SLIDE      "Slide up & Down link sus+hammer")
				  (FALL       "")
				  (DOIT       "")
				  (BEND       "Bend up/down")
				  (REP        "Fast rep RR")
				  (REP2       "Fast rep perf")
				  (HAMMER     "Hammer on/off link sus+hammer")
				  (NOISE      "")))
    (param stingray inst)
    inst))

(defun hofner-fingered (&key (parent ql-bass) channel 
			     keynumber-map articulation-map dynamic-map
			     (remarks "Ministry of Rock Bass"))
  (let ((inst (make-instrument 'hofner-fingered
			       :parent parent
			       :channel channel
			       :transient t
			       :remarks remarks
			       :keynumber-map keynumber-map
			       :articulation-map articulation-map
			       :dynamic-map dynamic-map)))
    (--make-ql-program-map inst '((SUS   "Sustain")
				  (SLIDE "Slide up sustain")
				  (NOISE "Noise")))
    (param hofner-fingered inst)
    inst))

(defun lakland-fingered (&key (parent ql-bass) channel 
			      keynumber-map articulation-map dynamic-map
			      (remarks "Ministry of Rock Bass"))
  (let ((inst (make-instrument 'lakland-fingered
			       :parent parent
			       :channel channel
			       :transient t
			       :remarks remarks
			       :keynumber-map keynumber-map
			       :articulation-map articulation-map
			       :dynamic-map dynamic-map)))
    (--make-ql-program-map inst '((OPEN   "Open sustain")
				  (LEG    "Legatto")
				  (SLIDE  "Slide down")
				  (SLIDE2 "Slide up")
				  (SLIDE3 "Slide up sustain")
				  (OCTAVE "Octave")
				  (BRRR   "Burr")))
    (param lakland-fingered inst)
    inst))

(defun silvertone-fingered (&key (parent ql-bass) channel 
				 keynumber-map articulation-map dynamic-map
				 (remarks "Ministry of Rock Bass"))
  (let ((inst (make-instrument 'silvertone-fingered
			       :parent parent
			       :channel channel
			       :transient t
			       :remarks remarks
			       :keynumber-map keynumber-map
			       :articulation-map articulation-map
			       :dynamic-map dynamic-map)))
    (--make-ql-program-map inst '((SUS    "Sustained")
				  (SLIDE  "Slide up")
				  (BEND   "Bend up")
				  (SLIDE2 "Slide down")
				  (NOISE  "Noise")))
    (param silvertone-fingered inst)
    inst))

(defun stingray-fingered (&key (parent ql-bass) channel 
			       keynumber-map articulation-map dynamic-map
			       (remarks "Ministry of Rock Bass"))
  (let ((inst (make-instrument 'stingray-fingered
			       :parent parent
			       :channel channel
			       :transient t
			       :remarks remarks
			       :keynumber-map keynumber-map
			       :articulation-map articulation-map
			       :dynamic-map dynamic-map)))
    (--make-ql-program-map inst '((LONG   "Sustain long RR")
				  (SHORT  "Sustain short RR")
				  (MUTE   "Sustain mute RR")
				  (HAMMER "Hammer on/pull off link slide")
				  (SLIDE  "Slide up/down link hammer")
				  (FALL   "")
				  (DOIT   "")
				  (BEND   "Bend up/down")
				  (NOISE  "")))
    (param stingray-fingered inst)
    inst))


