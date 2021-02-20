;;;; CYCO examples ex4-main
;;;;
;;;; Example project ex4 illustrates the various patterns and generators
;;;; and is not intended as a cohesive piece.  Each section stands alone
;;;; and demonstrates one or two concepts.

(version 3)
(plugin general-midi)

(project ex4 :tempo 90 :bars 4)

(lpf utilities)	       
(lpf orchestra)
(lpf preroll)
(lpf a)   ;; qball drum pattern
(lpf b)   ;; tone-row using the bag pattern
(lpf c)   ;; shift-register
(lpf d)   ;; recaman generator
(lpf e)   ;; hailstone generator
(lpf f)   ;; logistic generator
(lpf g)   ;; slowglass & palindrome for generated harmony
(lpf h)   ;; transpose using alloy generator
(lpf i)   ;; strummer and chord-tables


(section-order '(preroll a b c d e f g))
(project->midi)
