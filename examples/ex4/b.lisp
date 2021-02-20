;;;; CYCO examples ex4 b
;;;;
;;;; Use bag pattern to create a tone-row.


;; Section b bars and tempo values override the project's
;; defaults.
;;
(section b :bars 6 :tempo 120)

(metronome b-metronome)

;; Create the tone-row and assign it to a global constant +TONE-ROW+
;; The tone-row is randomly generated the first time this file
;; is loaded, thereafter the same tone-row is reused.
;;
(constant +tone-row+ (let ((b (bag :of '(0 1 2 3 4 5 6 7 8 9 10 11))))
		       (transpose (next b 12) 60)))

;; Use invert to create a key-inversion of the tone-row.
;;
(constant +inverted-tone-row+ (invert +tone-row+ 72))

(format t "tone-row  ~A~%" (keyname +tone-row+))
(format t "inverted  ~A~%" (transpose (keyname +inverted-tone-row+) 12))
	

(let* ((cue-list (create-cue-list :bars 3)))
 
  ;; Repeat base tone-row for duration of the section.
  (qball b-tone-row piano
	 :bars 3
	 :cue cue-list
	 :dur 'q
	 :key +tone-row+
	 :amp 'mf)

  ;; The qball for the inverted motif has the same cue-list as
  ;; above but is delayed by 3 whole notes plus a sixteenth note.
  ;;
  (qball b-inverted-tone-row piano
	 :shift 'w+w+w+s
	 :bars 3
	 :render-once t
	 :cue cue-list
	 :dur 'q
	 :key +inverted-tone-row+
	 :amp 'mp))

(->midi b)
