;;;; CYCO  example ex1
;;;;
;;;; Example 1 is a simple Russian folk song "Moscow Nights"
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
;;; Normally the title whould serve as the projet's name but here they
;;; are seperate.
;;;

(project ex1
	 :tempo 90    
	 :bars 7                   ;; bars per phrase, odd length that!
	 :beats 2                  ;; beats per bar, IE in 2/4 time.
	 :title "Moscow Nights"
	 :remarks "CYCO example project 1")


;;; Use LPF (Load Project File) to load remaining project files.
;;;

(lpf 'orchestra)
(lpf 'preroll)
(lpf 'a)
(lpf 'b)

(section-order '(preroll a (b :invert c5)))

(project->midi)
