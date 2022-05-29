;;;; CYCO example ex7 b
;;;;
;;;; Pattern comprehension.
;;;;
;;;; Pattern comprehension allows nested pattern structures to be
;;;; expressed as a single quoted expression.  They are more succinct
;;;; at the cost of some flexibility.
;;;;
;;;; A nested pattern
;;;;
;;;;   (param p1 (cycle :of (append '(a b c) (list (dice :of (list 'ape 'bat (line :of  '(1 2))))))))
;;;;
;;;;  Or the equivalent 
;;;;
;;;;   (param p2 (cycle :of (list 'a 'b 'c (dice :of (list 'ape 'bat (line :of '(1 2)))))))
;;;;
;;;; Patterns p1 and p2 have identical structures and are constructed using
;;;; traditional nested list.
;;;;

;;;; Pattern-comprehension builds the same structure in a more concise
;;;; statement.
;;;;
;;;; (param p3 (pattern-comprehension '(cycle (a b c (dice (ape bat (line (1 2))))))))
;;;;
;;;; Note there is a single quote and the :of keyword has been eliminated.
;;;; Actually :OF may be used for clarity but it is completely ignored.
;;;;
;;;;  '(cycle :of (a b c (dice :of (ape bat (line :of (1 2))))))))
;;;;

;;;; An open parentheses is either the start of a new pattern function or
;;;; the start of a literal list.   The following pattern/generator types
;;;; may be used:
;;;;
;;;;  (BAG  (CYCLE  (DICE  (LINE  (WALKER  (BONES  (COUNTER  (HAILSTONE
;;;;  (LOGISTIC  (RECAMAN  (RAMP  and  (SHIFT-REGISTER
;;;;
;;;; All other words are taken as the start of a list of literal values.
;;;;
;;;; Many of these generator/pattern types take optional keywords.
;;;; These may be included where they would normally be expected.
;;;;
;;;;   '(SHIFT-REGISTER #b0001 #b1010 :mask #b1111)
;;;;
;;;; The following keywords may be used for pattern types that expect them
;;;;    :FINAL :OF :FUNCTION :EVEN :ODD :LENGTH :BY :MASK :PRERUN and :HOOK
;;;;
;;;; The list pattern/generator types and keywords should be considered
;;;; reserved words and should probably not be included as literal data.
;;;;
;;;; The following cycle would not be a good idea
;;;;
;;;;   '(dice :of (:odd :even))
;;;;


;;;; Pattern-comprehension may be used with QBALL parts by using the
;;;; PATTERN-COMPREHENSION function.
;;;;
;;;;   (qball foo piano
;;;;      :cue ....
;;;;      :key (pattern-comprehension '(cycle (c4 e4 (dice (g4 bf4)) c5))) 
;;;;      ....)
;;;;

;;;; Pattern-comprehension is built-in for most XBALL parameters.
;;;;
;;;;  (xball foo piano
;;;;    :cue ....
;;;;    :key '(cycle (c4 e4 (dice (g4 bf4)) c5))
;;;;    :amp '(dice (mf ff))
;;;;    :chord '(cycle [maj] [min] (dice [maj] [dim]) [7])
;;;;    ...)


