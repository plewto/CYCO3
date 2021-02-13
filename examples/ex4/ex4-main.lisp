;;;; CYCO examples ex4-main
;;;;

(version 3)
(plugin general-midi)

(project ex4
	 :tempo 90
	 :bars 4)


(lpf orchestra)
(lpf preroll)
(lpf a)   ;; qball drum pattern
(lpf b)   ;; tone-row
(lpf c)   ;; shift-register
(lpf d)   ;; recaman
(lpf e)   ;; hailstone
