;;;; CYCO examples ex2-main.lisp
;;;;
;;;; Example 2 is a cover of the first half of Brian Eno's "The Great
;;;; Pretender".  The piece is built on a simple 8-bar motif which is
;;;; repeated throughout.  On each repetition additional instruments are
;;;; added.   Producing this song using only general MIDI is rather
;;;; restricting and no attempt has been made at the 2nd half.  

(version 3)
(plugin general-midi)

;; Create project object.
;; 
(project ex2
	 :tempo 112
	 :bars 8
	 :beats 4
	 :title "The Great Pretender")

;; Load additional files.
;;
(lpf orchestra)
(lpf preroll)  ;; initializartion & count-in metronome.
(lpf a)        ;; basic motif
(lpf b1)       ;; vibes
(lpf b2)       ;; inverted vibes
(lpf c)        ;; clanks
(lpf d)        ;; synth
(lpf e)        ;; guitar
(lpf fade)
(lpf controller-reset)

;; Set the section order.   
;; Sections with ":x 2" are repeated twice.
;;
(section-order '(preroll a b1 b2 c (d :x 2) (e :x 2) fade controller-reset))

;; Render the project and save to the MIDI directory.
;;
(project->midi)
