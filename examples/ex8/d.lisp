;;;; CYCO example ex8 D
;;;;
;;;; Cuelist manipulation using MASK-CUELIST.
;;;;

;;;; As hinted at in example ex3, CYCO cuelist are extremely flexible.  Just
;;;; about any type of object may serve as a cue list provided an appropriate
;;;; cue-function is provided.  That being said, there is a standard form
;;;; used with the default BAR cue-function: ((bar beat subbeat) ...).
;;;;
;;;; BINCUE provides an additional form using a binary notation.
;;;;
;;;;    (param bc (bincue :timesig '(1 4 4)))
;;;;    (bincue-translate bc '(1000 1010 1000 1010)) -> BAR form cuelist.
;;;;
;;;; bincue-translate may also take a binary string:
;;;;
;;;;    (bincue-translate bc "1000 1010 1000 1010")  -> BAR form cuelist.
;;;;    ;; white-space is ignored.
;;;;
;;;; For this discussion there are two definitions:
;;;;
;;;;  1) 'cuelist' refers to a list of cue-points as used by the BAR
;;;;      cue-function.   ((bar beat subbeat)(bar beat subbeat) ...)
;;;;
;;;;      Further cuelist is simplified. The BAR function allows
;;;;      for an optional tick value.   (bar beat subbeat tick).
;;;;      The functions described below ignore any tick values,
;;;;      and the two cue-points (4 2 3 5) and (4 2 3) are considered 
;;;;      the same.
;;;;
;;;;  2) 'cuestring' refers to a string of binary bits as used by
;;;;      bincue-translate above.
;;;;

;;;; ----------------------------------------------------------
;;;; Printing cuelist and cuestring with PPRINT-CUESTRING.
;;;;
;;;; The function PPRINT-CUESTRING provides formatted output of cuelist and
;;;; cuestrings. 
;;;;
;;;; (PPRINT-CUELIST arg &key header form timesig use-subbeats)
;;;;
;;;; arg      - cuelist, cuestring or PART (such as a QBALL)
;;;;
;;;; :header  - optional header text.
;;;;
;;;; :form    - Defaults to list format.
;;;;            If :form has the value :binary, output is formatted as
;;;;            binary strings.
;;;;
;;;; :timesig - Reference time-signature. May be any of the following:
;;;;            1) Any instance of time-signature (such as a part or section)
;;;;            2) A list of form '(bars beats subbeats) to use a local
;;;;               time-signature.
;;;;            3) NIL. Defaults to current-section of *project*
;;;;
;;;;            The :timesig arguments for all functions below have the same
;;;;            options.
;;;;
;;;; :use-subbeats - The time-signature class provides both SUBBEATS
;;;;                 (typically 4 subbeats to the beats for 16th notes)
;;;;                 And tsubbeats which are 2/3 of a subbeats (typically
;;;;                 6 tsubbeats per beat for 16th note triplets).
;;;;
;;;;                 If the use-subbeats argument is true, then the
;;;;                 time-signature subeats value is used for the base time
;;;;                 unit.  Otherwise the tsubbeats value is used.
;;;;                 Effectively time is quantized to either subbeats or
;;;;                 tsubbeats.   Subbeats are the default.
;;;;
;;;;                 The :use-subbeats argument for all functions below
;;;;                 have the same usage.
;;;;

(defun newline ()
  (format t "~%"))


(section D :bars 2 :beats 4 :subbeats 4)

(qball d-clave gm-woodblock
       :cue '((1 1 1)(1 2 3)(1 3 3)(2 2 1)(2 3 1))
       :key 'clave)

(pprint-cuelist '((1 1 1)(1 2 3)(1 3 3)(2 2 1)(2 3 1)) :header "Example ex8-d-1")
(newline)

(pprint-cuelist d-clave :header "Example ex8 d-clave cuelist")
(newline)

(pprint-cuelist d-clave :header "Example ex8 d-clave binary form" :form :binary)
(newline)

;;;; The above code only works because d-clave inherits the time-signature from
;;;; section D.  If it had a different signature (most likely a different bar-count,
;;;; remember CYCO time-signatures include the bar count), then  you would pass
;;;; the part to the :timesig argument.
;;;;
;;;;  (pprint-cuelist d-clave :timesig d-clave)
;;;;



(banner2 "MASK-CUELIST")
;;;; ----------------------------------------------------------
;;;; Using MASK-CUELIST
;;;;
;;;; MASK-CUELIST applies bit-wise binary operations to cuelist. 
;;;; It may apply any one of the following operations:
;;;;
;;;; A few of the operations only apply to cuelist argument.
;;;;
;;;;
;;;; Simple inversion.  The 2nd argument is not used.
;;;;




(pprint-cuelist (mask-cuelist "0000 1111 0000 1111" nil :op :not :timesig '(1 4 4))
		:form :binary
		:header "Cuelist inversion"
		:timesig '(1 4 4))
(newline)


;;;; Since an alternate time-signature is use in the above code, it has to be
;;;; specified both for the mask-cuelist and pprint-cuelist functions.
;;;; To avoid this extra typing a new section is created below to establish a new
;;;; default time-signature.
;;;;

(section d2 :bars 1)

;;;; Bit shifting and rotation also only applies to the first argument
;;;;

(pprint-cuelist (mask-cuelist "0000 1111 0000 1111" nil :shift 2)
		:form :binary
		:header "Bit shift")

(newline)

(pprint-cuelist (mask-cuelist "0000 1111 0000 1111" nil :rotate -6)
		:form :binary
		:header "Bit rotation")
(newline)

;;;;
;;;; Shifting and rotation is applied -prior- to any binary operation.
;;;;

(param clist "0000 1111 0000 1111") ;; used for following examples
(param mask  "0000 0000 1111 1111")

(pprint-cuelist clist :form :binary :header "cuelist value")
(pprint-cuelist mask  :form :binary :header "mask value")
(newline)
(dolist (op '(:and :or :xor :nand :nor :nxor))
  (pprint-cuelist (mask-cuelist clist mask :op op)
		  :form :binary
		  :header (sformat "op = ~A" op)))
(newline)

;;;; Operators types which end with '!', invert the mask value before
;;;; application. 
;;;;

(dolist (op '(:and! :or! :xor! :nand! :nor! :nxor!))
    (pprint-cuelist (mask-cuelist clist mask :op op)
		  :form :binary
		  :header (sformat "op = ~A" op)))
(newline)

;;;; Ducking is a specific application of mask-cuelist.
;;;;

(pprint-cuelist (duck clist mask)
		:form :binary
		:header "Default ducking is the same as :AND!")

(pprint-cuelist (duck clist mask :invert t)
		:form :binary
		:header "Inverted ducking is the same as :AND")
(newline)



(section d3 :bars 1)

;;;; ----------------------------------------------------------
;;;; Using binary cuestrings opens up the possibility for using patterns
;;;; and generators to algorithmic cuelist creation.
;;;;

(param bc (bincue))

(param sr (shift-register #b1000101010001010  #b1001110010011100
			  :mask #b1111111111111111 :prerun 1000
			  :hook #'(lambda (v)(sformat "~B" v))))
(dotimes (i 10)
  (pprint-cuelist (bincue-translate bc (next sr)) :form :binary
		  :header (sformat "Shift-register ~A" i)))
