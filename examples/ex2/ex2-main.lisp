;;;; CYCO examples ex2-main.lisp
;;;;

(version 3)
(plugin general-midi)

(project ex2
	 :tempo 112
	 :bars 8
	 :beats 4
	 :title "The Great Pretender"
	 :remarks "Brian Eno")

(lpf orchestra)
(lpf preroll)
(lpf a)
(lpf b)


(section-order '(preroll  a  (b :x 2)))
(project->midi)
