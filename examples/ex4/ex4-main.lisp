;;;; CYCO examples ex4-main
;;;;
;;;; Example project ex4 illustrates patterns and generators and is not
;;;; intended as a cohesive piece.  Each section stands alone and
;;;; demonstrates one or two concepts.
;;;;

;;;; Patterns and Generators are similar and at time may be interchanged.
;;;; The methods reset, value, next-1, next-n and next are defined for both
;;;; classes.  The difference are summarized below:
;;;;
;;;; value
;;;;    Patterns may have a value of any type.
;;;;    Generators have numeric values only.
;;;;
;;;; nesting
;;;;    Patterns may be nested to any level.
;;;;    Generators may not be nested, though a generator may be an element
;;;;    of a pattern.
;;;;
;;;; value hook function
;;;;    Patterns do not have a value hook function.
;;;;    Generators have a value hook function.   The hook is applied to the
;;;;    generators internal or natural value.
;;;;
;;;; Action functions
;;;;    Patterns do not have action functions.
;;;;    Generators may have an action function which is called whenever the
;;;;    they produce specific values. 
;;;;

(version 3)
(plugin general-midi)

(project ex4 :tempo 90 :bars 4)

(lpf utilities)	       
(lpf orchestra)
(lpf preroll)
(lpf a)   ;; qball drum pattern
(lpf b)   ;; line pattern
(lpf c)   ;; tone-row using the bag pattern
(lpf d)   ;; dice and walker patterns
(lpf e)   ;; coin and wrapper patterns
(lpf f)   ;; Nessted patterns
(lpf g)   ;; Counter, ramp and asr-envelope generators
;; (lpf h)   ;; LFO generators  **DEPRECIATED**
(lpf i)   ;; Shift-register
(lpf j)   ;; recaman generator
(lpf k)   ;; hailstone generator
(lpf m)   ;; logistic generator
(lpf n)   ;; slowglass & palindrome for generated harmony
(lpf o)   ;; transpose using alloy generator


