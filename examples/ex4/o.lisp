;;;; CYCO examples o
;;;;
;;;; Example h uses an ALLOY generator to transpose a basic motif.
;;;; An alloy combines two generators (or numeric-only patterns)
;;;; with a binary function.  The default function is addition.
;;;;

(section o :bars 8 :tempo 120)

(let* ((motif (keynumber '(c4 e4 g4 bf4)))
       (transpose-list '(0 0 0 0  9 9 9 9  5 5 5 5  7 7 7 7
			 0 0 0 1  2 2 2 2  5 5 5 6  7 7 7 5))
			   
       (key-list (next (alloy (cycle :of motif)
			      (cycle :of transpose-list) :function #'+)
		       (length transpose-list)))
       (cue-list (create-cue-list :bars 8)))

  ;; The CNTH function is like the Common Lisp NTH function except it is
  ;; cyclical.   (cnth 3 '(a b c)) --> a
  ;;             (cnth 4 '(a b c)) --> b
  ;;
  (dotimes (i (length key-list))
    (format t "[~2D]  motif ~3A  transpose ~A   --> ~A~%"
	    i (keyname (cnth i motif))(cnth i transpose-list)(keyname (nth i key-list))))
  
  (qball o-piano piano
	 :bars 8
	 :cue cue-list
	 :key key-list
	 :dur 'q.))

(->midi o)
