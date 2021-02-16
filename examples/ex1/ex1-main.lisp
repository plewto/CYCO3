;;;; CYCO  example ex1
;;;;
;;;; Example 1 is a simple Russian folk song "Moscow Nights"
;;;; As scored the piece consist of a 7-bar phrase.   This example
;;;; is divided into 3 sections.
;;;;
;;;;   1) "preroll" - Program-changes with a metronome count-in.
;;;;   2) "A"  - The piece as scored.
;;;;   3) "B"  - Clone of section A but inverted.
;;;;
;;;; See score.png
;;;;


;;; Ensure CYCO version 3.
;;;

(version 3)

;;; Load general-midi plugin.
;;;

(plugin general-midi)


;;; Create project named ex1
;;;
;;; Title and remarks are optional.
;;; Normally the title should serve as the project's name but here they
;;; are separate.
;;;
;;; The tempo, bars and beats values are the defaults for the remainder
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
;;; 

(section-order '(preroll a b))

;;; Write the main MIDI file.
;;; The file is placed in the MIDI folder and names ex1-main.mid
;;;

(project->midi)
