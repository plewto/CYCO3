;;;; CYCO  example ex1
;;;;
;;;; Example 1 is a simple Russian folk song "Moscow Nights"
;;;; As scored the piece consist of a 7-bar phrase.   This example
;;;; is divided into 3 sections.
;;;;   1) "preroll" - a metronome count-in and program-changes
;;;;   2) "A"  - The piece as scored.
;;;;   3) "B"  - clone of section A but inverted.
;;;;
;;;; See score.png
;;;;



;;; Force CYCO version 3.
;;;

(version 3)

;;; Load general-midi plugin.
;;;

(plugin general-midi)


;;; Create project named ex1
;;;
;;; The title and remarks values are optional.
;;; Normally the title should serve as the project's name but here they
;;; are separate.
;;;
;;; The tempo, bars and beats values are defaults for the remainder
;;; of the project.
;;;

(project ex1
	 :tempo 90    
	 :bars 7                   ;; bars per phrase, odd length that!
	 :beats 2                  ;; beats per bar, IE in 2/4 time.
	 :title "Moscow Nights"
	 :remarks "CYCO example project 1")


;;; Use LPF (Load Project File) to load remaining project files.
;;;

(lpf orchestra)
(lpf preroll)
(lpf a)
(lpf b)


;;; Before creating the main MIDI file the section-order must be specified.
;;;
;;; The section names are listed in order.  (b :invert c5) indicates
;;; section b is to be inverted aqrounds c5 (middle-C).
;;; 

(section-order '(preroll a (b :invert c5)))

;;; Write the main MIDI file.
;;; The file is placed in the MIDI folder na dhas the name
;;; ex1-main.mid
;;;

(project->midi)
