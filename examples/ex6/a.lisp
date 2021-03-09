;;;; CYCO example project ex6 section A
;;;;

(section a :bars 16)

;; STRUMMER parts have a rudimentary ability to create simple MIDI
;; control-change and pitch-bend events.  The CONTROLLERS and BENDER parts
;; produce more complex events.
;;

;; (CONTROLLERS name instruments &key section cuefn shuffle shift
;;                                    tempo unit bars beats subbeats
;;                                    render-once no-thin remarks events)
;;
;; name         - Unquoted symbol, the new part is bound to the symbol name.
;; instruments  - Instrument or list of instruments.
;; :section     - Parent section, defaults to current-section of *PROJECT*
;; :cuefn       - Time cue function, defaults to section value.
;; :shuffle     - Time shuffle function, defaults to section value.
;; :shift        - Time shift either in absolute seconds or using relative
;;                 metric value. Defaults to 0.
;; :tempo        - Tempo in BPS, defaults to section's value.          
;; :unit         - Time signature beat-unit, defaults to section's value.           
;; :bars         - Time signature number of bars per phrase, defaults to
;;                 section's value.
;; :beats        - Time signature beats per bar, defaults to section's value.            
;; :subbeats     - Time signature sub-beats per beat, defaults to section's
;;                 value.
;; :render-once  - Boolean, if true do not repeat part.  Render-once has
;;                 no effect if the controllers and part lengths are the same.
;;                 Default nil.
;; :no-thin      - Boolean, if non-nil do not thin redundant events, default nil.
;; :remarks      - Optional remarks text.              
;; :events       - List of events to generate.  See below.

;; CONTROLLERS events are specified as a nested list similar to STRUMMER
;; events. 


(strummer a-organ organ
	  :events '((:time (1 1 1) :key c5 :dur w)
		    (:time (2 1 1) :key e5 :dur w+w)
		    (:time (4 1 1) :key f5 :dur w+w)
		    (:time (6 1 1) :key c5 :chord [min7])
		    (:time (8 1 1) :key e4 :chord [circle-4] :dur w)
		    (:time (9 1 1) :key e4 :chord [circle-4] :dur w)
		    (:time (10 1 1) :key f6 :chord [klstr-9])
		    (:time (11 1 1) :key g6 :chord [min-add11]) ))


(controllers a-cc-1 organ
	     :events '(
		       ;; The :cc keyword generates a single event.
		       ;;
		       ;;     :cc time controller-number value
		       ;;
		       ;;     controller-number may either be an integer
		       ;;     0 <= controller-number < 128 or a named
		       ;;     controller.  Use the ?CONTROLLERS function 
	               ;;     to display list of defined controller names.
                       ;;     The special controller-name 'pressure'
		       ;;     is used for channel-pressure events.
		       ;;
		       ;;     value must be an integer 0 <= value < 128.
		       ;;
		       (:cc (1 1 1) volume 127)
		       (:cc (1 2 1) volume  50)
		       (:cc (1 3 1) volume 127)

		       ;; More complex events require more setup.  At a 
		       ;; minimum this involves setting time and value ranges,
		       ;; and a controller-number.
		       ;;
		       ;; :time is used to set a time interval
		       ;;
		       ;;    :time start end interval
		       ;;
		       ;;    Where start and end specify the time range and
		       ;;    must be in a format accepted by the cue function.
		       ;;    interval sets the rate at which events are
		       ;;    produced and may either be in seconds (absolute)
		       ;;    or a metric-expression (tempo-scaled).
		       ;;
		       (:time (2 1 1)(3 4 4) s) 

		       ;; The :value keyword sets the range of generated values.
		       ;;
		       ;;    :value start end
		       ;;
		       ;;     Where start != end, and 0 <= start < 128, 0 <= end < 128.
		       ;;
		       (:value 0 127)

		       ;; The :ctrl keyword specifies the controller-number.  It
		       ;; may be a integer MIDI controller-number, a named controller or
		       ;; the special symbol PRESSURE (for channel-pressure events)
		       ;;
		       (:ctrl volume)

		       ;; The preceding three statements set a time-span
		       ;; and value-range for volume events but do not
		       ;; generate anything.   To actually produce events,
		       ;; one of the generating keywords must be used.
		       ;; The simplest generator is :ramp.  It is important
		       ;; that generating keywords be specified -after- any
		       ;; preparatory clauses.   The following statement
		       ;; produces a volume fade-in beginning at bar 2.
		       ;; Events are generated every 16/th note.
		       ;;
		       (:time (2 1 1)(4 1 1) s :value 0 127 :ctrl volume :ramp)

		       ;; The next statement produces a fade-out starting
		       ;; at bar 4.   The special symbol < for starting
		       ;; time and value, uses the previous end
		       ;; time/value.   Another special symbol is * which
		       ;; reuses the current value.
		       ;;
		       (:time < (6 1 1) s :value < 0 :ramp)


		       ;; Cyclical events are produced by the :saw, :tri
		       ;; and :pulse keywords.
		       ;;
		       (:time (6 1 1)(8 1 1) x :value 0 127 :ctrl volume :tri)

		       ;; :cycles keyword sets number of cycles produced
		       ;;
		       (:time < (9 1 1) x :cycles 4 :saw)

		       ;; down saw.
		       (:time < (10 1 1) x :value 127 0 :cycles 4 :saw)

		       ;; The :pulse wave has an optional :width parameter.
		       ;;
		       (:time < (11 1 1) x :value 0 127 :cycles 8 :width 0.50 :pulse)
		       (:time < (12 1 1) x :width 0.60 :pulse)))

(->midi a)
