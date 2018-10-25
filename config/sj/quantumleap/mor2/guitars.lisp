;;;; PigIron CYCO sj config quantumleap mor2-guitar
;;;;
;;;; Instruments:
;;;;
;;;; ql-guitar
;;;;    baritone
;;;;    baritone-rhythm
;;;;    carvin-bridge
;;;;    carvin-bridge-rhythm
;;;;    carvin-neck
;;;;    carvin-neck-rhythm
;;;;    jaguar
;;;;    jaguar-rhythm
;;;;    lespaul
;;;;    lespaul-rhythm
;;;;    schecter
;;;;    schecter-rhythm
;;;;    thinline
;;;;    thinline-rhythm

(defun baritone (&key (parent ql-guitar) channel 
		      keynumber-map articulation-map dynamic-map
		      (remarks "Ministry of Rock Guitar"))
  (let ((inst (make-instrument 'baritone
			       :parent parent
			       :channel channel
			       :remarks remarks
			       :transient t
			       :keynumber-map keynumber-map
			       :articulation-map articulation-map
			       :dynamic-map dynamic-map)))
    (--make-ql-program-map inst '((sus         "Sustain down/up RR")
				  (lead-hammer "Lead hammer on/pull off (link with lead-slide)")
				  (lead-slide  "Lead slide up/down (link with lead-hammer)")
				  (harm        "Harmonics")
				  (short       "Staccato short RR")
				  (palm        "Staccato palm slow RR")
				  (trem        "Tremolo")))
    (param baritone inst)
    inst))


(defun baritone-rhythm (&key (parent ql-guitar) channel 
			     keynumber-map articulation-map dynamic-map
			     (remarks "Ministry of Rock Guitar"))
  (let ((inst (make-instrument 'baritone-rhythm
			       :parent parent
			       :channel channel
			       :remarks remarks
			       :transient t
			       :keynumber-map keynumber-map
			       :articulation-map articulation-map
			       :dynamic-map dynamic-map)))
    (--make-ql-program-map inst '((sus    "Pick sustain")
				  (sus2   "Pick sustain 2")
				  (sus3   "Pick sustain down/up")
				  (hammer "Hammer on/pull off (link hammer slide slide2)")
				  (slide  "Slide up/down      (link hammer slide slide2)")
				  (slide2 "Slide up/down slow (link hammer slide slide2)")
				  (short  "Pick short RR")
				  (death  "Death")
				  (death2 "Death long")
				  (death3 "Death slow")
				  (palm   "Palm slow RR")
				  (palm2  "Palm medium RR")
				  (palm3  "Palm staccato fast RR")
				  (palm4  "Palm faster RR")
				  (perf   "Performance Palm")
				  (chug   "Short chugs")
				  (chugs2 "Performance chugs")
				  (fx     "")
				  (scrapes "Scrapes and noises")))
    (param baritone-rhythm inst)
    inst))

(defun carvin-bridge (&key (parent ql-guitar) channel 
			   keynumber-map articulation-map dynamic-map
			   (remarks "Ministry of Rock Guitar"))
  (let ((inst (make-instrument 'carvin-bridge
			       :parent parent
			       :channel channel
			       :remarks remarks
			       :transient t
			       :keynumber-map keynumber-map
			       :articulation-map articulation-map
			       :dynamic-map dynamic-map)))
    (--make-ql-program-map inst '((sus       "Sustain no-vibrato RR")
				  (vib       "Sustain med vibrato RR")
				  (vib2      "Sustain deep vibrato RR")
				  (hammer    "Hammer on/Pull off (link hammer slide)")
				  (slide     "Slide up/down (link hammer slide)")
				  (scream    "")
				  (harm      "Harmonics")
				  (pinch     "Pinch harmonics")
				  (scrapes   "")
				  (steck8va  "Staccato drop octave RR")
				  (palm      "Palm RR")
				  (steck     "Staccato mute RR")
				  (perf      "Performance")
				  (perf2     "Performance 2")
				  (scrapes   "Scrape effects")
				  (scrapes2  "Scrape effects")
				  (fx-scream "Effects scream")
				  (fx-single "Effects single notes")
				  (clusters  "Effects clusters")))
    (param carvin-bridge inst)
    inst))


(defun carvin-bridge-rhythm (&key (parent ql-guitar) channel 
				  keynumber-map articulation-map dynamic-map
				  (remarks "Ministry of Rock Guitar"))
  (let ((inst (make-instrument 'carvin-bridge-rhythm
			       :parent parent
			       :channel channel
			       :remarks remarks
			       :transient t
			       :keynumber-map keynumber-map
			       :articulation-map articulation-map
			       :dynamic-map dynamic-map)))
    (--make-ql-program-map inst '((down   "Pick down")
				  (up     "Pick up")
				  (double "Pick double (sustain) RR")
				  (slide  "Slide up/down (link slide slide2)")
				  (slide2 "Slide up/down (link slide slide2)")
				  (palm   "Palm medium RR")
				  (palm2  "Palm short RR")
				  (efx    "Rhythmic effects")))
    (param carvin-bridge-rhythm inst)
    inst))


(defun carvin-neck (&key (parent ql-guitar) channel 
			 keynumber-map articulation-map dynamic-map
			 (remarks "Ministry of Rock Guitar"))
  (let ((inst (make-instrument 'carvin-neck
			       :parent parent
			       :channel channel
			       :remarks remarks
			       :transient t
			       :keynumber-map keynumber-map
			       :articulation-map articulation-map
			       :dynamic-map dynamic-map)))
    (--make-ql-program-map inst '((sus       "Sustain no-vibrato RR")
				  (vib       "Sustain med vibrato RR")
				  (vib2      "Sustain deep vibrato RR")
				  (hammer    "Hammer on/Pull off (link hammer slide)")
				  (slide     "Slide up/down (link hammer slide)")
				  (scream    "")
				  (harm      "Harmonics")
				  (pinch     "Pinch harmonics")
				  (scrapes   "")
				  (steck8va  "Staccato drop octave RR")
				  (palm      "Palm RR")
				  (steck     "Staccato mute RR")
				  (perf      "Performance")
				  (perf2     "Performance 2")
				  (scrapes   "Scrape effects")
				  (scrapes2  "Scrape effects")
				  (fx-scream "Effects scream")
				  (fx-single "Effects single notes")
				  (clusters  "Effects clusters")))
    (param carvin-neck inst)
    inst))


(defun carvin-neck-rhythm (&key (parent ql-guitar) channel 
				keynumber-map articulation-map dynamic-map
				(remarks "Ministry of Rock Guitar"))
  (let ((inst (make-instrument 'carvin-neck-rhythm
			       :parent parent
			       :channel channel
			       :remarks remarks
			       :transient t
			       :keynumber-map keynumber-map
			       :articulation-map articulation-map
			       :dynamic-map dynamic-map)))
    (--make-ql-program-map inst '((sus       "Sustain no-vibrato RR")
				  (vib       "Sustain med vibrato RR")
				  (vib2      "Sustain deep vibrato RR")
				  (hammer    "Hammer on/Pull off (link hammer slide)")
				  (slide     "Slide up/down (link hammer slide)")
				  (scream    "")
				  (harm      "Harmonics")
				  (pinch     "Pinch harmonics")
				  (scrapes   "")
				  (steck8va  "Staccato drop octave RR")
				  (palm      "Palm RR")
				  (steck     "Staccato mute RR")
				  (perf      "Performance")
				  (perf2     "Performance 2")
				  (scrapes   "Scrape effects")
				  (scrapes2  "Scrape effects")
				  (fx-scream "Effects scream")
				  (fx-single "Effects single notes")
				  (clusters  "Effects clusters")))
    (param carvin-neck-rhythm inst)
    inst))


(defun jaguar (&key (parent ql-guitar) channel 
		    keynumber-map articulation-map dynamic-map
		    (remarks "Ministry of Rock Guitar"))
  (let ((inst (make-instrument 'jaguar
			       :parent parent
			       :channel channel
			       :remarks remarks
			       :transient t
			       :keynumber-map keynumber-map
			       :articulation-map articulation-map
			       :dynamic-map dynamic-map)))
    (--make-ql-program-map inst '((sus     "Sustain no vibrato RR")
				  (vib     "Sustain with vibrato RR")
				  (sus3    "3 second sustain, no vibrato RR")
				  (vib3    "3 second sustain with vibrato RR")
				  (strum   "Strum long position 1, RR")
				  (strum2  "Strum long position 2, RR")
				  (hammer  "Hammer on/Pull off  (link hammer slide)")
				  (slide   "Slide up/down (link hammer slide)")
				  (slide2  "Slide up with half-tone vibrato")
				  (slide3  "Slide up with whole-tone vibrato")
				  (scrape  "Long scrapes")
				  (scrape2 "Short scrapes")
				  (trem    "Tremolo")
				  (trem2   "Tremolo mute")
				  (perf    "Performance 120 BPM")
				  (short   "Strum short RR")
				  (perf2   "Performance 1/4 note")
				  (fall    "Octave fall")
				  (noise   "")))
    (param jaguar inst)
    inst))


(defun jaguar-rhythm (&key (parent ql-guitar) channel 
			   keynumber-map articulation-map dynamic-map
			   (remarks "Ministry of Rock Guitar"))
  (let ((inst (make-instrument 'jaguar-rhythm
			       :parent parent
			       :channel channel
			       :remarks remarks
			       :transient t
			       :keynumber-map keynumber-map
			       :articulation-map articulation-map
			       :dynamic-map dynamic-map)))
    (--make-ql-program-map inst '((long   "Strum long RR, position 1")
				  (long2  "Strum long RR, position 2")
				  (short  "Strum short")))
    (param jaguar-rhythm inst)
    inst))


(defun lespaul (&key (parent ql-guitar) channel 
		     keynumber-map articulation-map dynamic-map
		     (remarks "Ministry of Rock Guitar"))
  (let ((inst (make-instrument 'lespaul
			       :parent parent
			       :channel channel
			       :remarks remarks
			       :transient t
			       :keynumber-map keynumber-map
			       :articulation-map articulation-map
			       :dynamic-map dynamic-map)))
    (--make-ql-program-map inst '((long         "Long no vibrato RR")
				  (vib          "Medium vibrato RR")
				  (vib2         "Hard vibrato RR")
				  (hammer       "Hammer on / Pull off  (link hammer slide)")
				  (slide        "Slide up/down         (link hammer slide)")
				  (hammer-vib   "Hammer on/off hard vib (link hammer-vib slide-vib)")
				  (slide-vib    "Slide up/down hard slow vib (link hammer-vib slide-vib)")
				  (bend         "Bend up")
				  (bend2        "Bend down")
				  (scream-bend  "Scream, bend up")
				  (scream-bend2 "Scream, fast bend down")
				  (scream-bend3 "Scream, fast bend up")
				  (scream-vib   "Scream, vibrato, bend up")
				  (pinch        "Pinch harmonics")
				  (stac         "Staccato RR")
				  (mute         "Staccato mute RR")
				  (perf         "Performance 1/16 notes")))
    (param lespaul inst)
    inst))


(defun lespaul-rhythm (&key (parent ql-guitar) channel 
			    keynumber-map articulation-map dynamic-map
			    (remarks "Ministry of Rock Guitar"))
  (let ((inst (make-instrument 'lespaul-rhythm
			       :parent parent
			       :channel channel
			       :remarks remarks
			       :transient t
			       :keynumber-map keynumber-map
			       :articulation-map articulation-map
			       :dynamic-map dynamic-map)))
    (--make-ql-program-map inst '((long        "Long fast RR")
				  (slow        "Long slower RR")
				  (death       "Long death RR")
				  (hammer      "Hammer on/pull off  (link hammer slide)")
				  (slide       "Slide up/down       (link hammer slide)")
				  (stac        "Staccato RR")
				  (palm        "Palm mute long RR")
				  (palm2       "Palm mute medium RR")
				  (palm3       "Palm mute short RR")
				  (chug        "Chug long RR")
				  (perf        "Chug performance fast")
				  (perf2       "Performance")
				  (perf3       "Medium performance")
				  (gallup      "Performance")
				  (chug2       "Chug RR")
				  (chug3       "Chug double mute")
				  (chug-fx     "Chug effects")
				  (chug-harm   "Chug harmonics")
				  (chug-squall "Chug death squall")))
    (param lespaul-rhythm inst)
    inst))


(defun schecter (&key (parent ql-guitar) channel 
		      keynumber-map articulation-map dynamic-map
		      (remarks "Ministry of Rock Guitar"))
  (let ((inst (make-instrument 'schecter
			       :parent parent
			       :channel channel
			       :remarks remarks
			       :transient t
			       :keynumber-map keynumber-map
			       :articulation-map articulation-map
			       :dynamic-map dynamic-map)))
    (--make-ql-program-map inst '((sus          "Sustain no-vibrato")
				  (vib          "Sustain with vibrato RR")
				  (motto        "Motto vibrato")
				  (hammer       "Hammer on/Pull off  (link hammer slide)")
				  (slide        "Slide up/down       (link hammer slide)")
				  (scrape-vib   "Scrape with vibrato")
				  (scream       "Scream with fall vibrato")
				  (pinch        "Pinch harmonic")
				  (harm         "Harmonics")
				  (scream-fall  "Scream with fall")
				  (mute         "Staccato mute RR")))
    (param schecter inst)
    inst))


(defun schecter-rhythm (&key (parent ql-guitar) channel 
			     keynumber-map articulation-map dynamic-map
			     (remarks "Ministry of Rock Guitar"))
  (let ((inst (make-instrument 'schecter-rhythm
			       :parent parent
			       :channel channel
			       :remarks remarks
			       :transient t
			       :keynumber-map keynumber-map
			       :articulation-map articulation-map
			       :dynamic-map dynamic-map)))
    (--make-ql-program-map inst '((long   "Long sustain RR")
				  (short  "Short RR")
				  (death  "Long death RR")
				  (death2 "Long slow death")
				  (long2  "Long slow RR")
				  (mute   "Mute long RR")
				  (mute2  "Mute medium RR")
				  (mute3  "Mute short RR")
				  (stac   "Staccato fast RR")
				  (perf   "Performance fast")
				  (hammer "hammer on/pull off (link hammer slide)")
				  (slide  "slide up/down      (link hammer slide)")
				  (perf2  "Performance fast chugs")
				  (chug2  "Chug with vibrato")))
    (param schecter-rhythm inst)
    inst))


(defun thinline (&key (parent ql-guitar) channel 
		      keynumber-map articulation-map dynamic-map
		      (remarks "Ministry of Rock Guitar"))
  (let ((inst (make-instrument 'thinline
			       :parent parent
			       :channel channel
			       :remarks remarks
			       :transient t
			       :keynumber-map keynumber-map
			       :articulation-map articulation-map
			       :dynamic-map dynamic-map)))
    (--make-ql-program-map inst '((sus     "Sustain no vibrato RR")
				  (vib     "Sustain with vibrato RR")
				  (vib2    "Vibrato accent")
				  (pos1    "Rhythm position 1 RR")
				  (pos2    "Rhythm position 2 RR")
				  (hammer  "Hammer on/Pull off (link hammer slide)")
				  (slide   "Slide up/down (link hammer slide)")
				  (scream  "Mild scream")
				  (bend    "Bend down fast")
				  (fall    "Fall down")
				  (fall2   "Fall up")
				  (harm    "Harmonics (3 key areas)")
				  (stec    "Staccato RR")
				  (scrape  "")
				  (noise   "Strum noise")
				  (noise2  "Strum noise 2")))
    (param thinline inst)
    inst))

(defun thinline-rhythm (&key (parent ql-guitar) channel 
			     keynumber-map articulation-map dynamic-map
			     (remarks "Ministry of Rock Guitar"))
  (let ((inst (make-instrument 'thinline-rhythm
			       :parent parent
			       :channel channel
			       :remarks remarks
			       :transient t
			       :keynumber-map keynumber-map
			       :articulation-map articulation-map
			       :dynamic-map dynamic-map)))
    (--make-ql-program-map inst '((pos1   "Position 1 RR")
				  (pos2   "Position 2 RR")
				  (noise  "Strum noise 1")
				  (noise2 "Strum noise 2")))
    (param thinline-rhythm inst)
    inst))
