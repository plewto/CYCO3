;;;; CYCO examples ex2-main.lisp
;;;;
;;;; Example 2 is a cover of the first half of Brian Eno's "The Great
;;;; Pretender".  The piece is built on a simple 8-bar motif which is
;;;; repeated throughout.  On each repetition additional instruments are
;;;; added.

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
(lpf preroll)  ;; initialization & count-in
(lpf a)        ;; basic motif
(lpf b1)       ;; vibes
(lpf b2)       ;; inverted vibes
(lpf c)        ;; percussive "clanks"
(lpf d)        ;; synth
(lpf e)        ;; ersatz guitar
(lpf fade)
(lpf controller-reset)

;; Set the section order.   
;; Sections with ":x 2" are repeated twice.
;;
(section-order '(preroll a b1 b2 c (d :x 2) (e :x 2) fade controller-reset))

;; Render the project and save to the MIDI directory.
;;
(project->midi)

;; Use DUMP-EVENTS to examine the generated MIDI events.  Usage is slightly
;; different between a project and section/parts.  Projects must be rendered
;; before using dump-events
;;
;; (dump-events (render-project))
;;
;; When dumping sections or parts there is no need to render first.
;;
;; (dump-events b1)         ;; section b1
;; (dump-events b1-vibes)   ;; vibes part in section b1
;;
