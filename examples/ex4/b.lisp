;;;; CYCO ex4 b  Line pattern
;;;;

;; The LINE pattern returns element in sequence until the final element
;; is reached.  Thereafter it continues to return the final element.
;;
;; (line :of '(1 2 3))
;; (next line 6) --> 1 2 3 3 3 3
;;
;;
;; The following example uses a line to produce a crescendo over the first
;; 31 notes.  The remaining 97 notes have the same amplitude. 
;;

(section b :bars 4 :tempo 112)

(qball b-piano piano
       :cue (create-cue-list :bars 4 :add-eighths t :add-sixteenths t)

       ;; PALINDROME takes a sequence and produces a palindrome from its
       ;; elements.   The return value is a sequence of the same type.  The
       ;; optional :elide keyword dictates how the pattern is repeated.
       ;;
       :key (palindrome '(c5 ef5 bf5 g5 d6 bf5 ef6 d6) :elide :start)

       ;; The RANGE function returns an arithmetic sequence between two
       ;; values.  The :by keyword sets the common difference.
       ;;
       :amp (line :of (range 0.1 1.0 :by 0.03))
       :dur 'q)

(print (length (render-once b-piano)))
(->midi b)
