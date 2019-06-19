;;;; CYCO plugins sj eastwest sd3 metals mongolian-chiles.lisp

(param mongolian-chimes nil)

(let ((keymap-table (make-hash-table))
      (remark-table (make-hash-table)))
  (setf (gethash 1 keymap-table) (circular-list-keynumber-map
				  (append (white-keys (ew-keynumber 'c1)
						      (ew-keynumber 'e2))
					  (white-keys (ew-keynumber 'c3)
						      (ew-keynumber 'a4))))
	(gethash 2 keymap-table) (circular-list-keynumber-map
				  (white-keys (ew-keynumber 'c1)
					      (ew-keynumber 'f4)))
	(gethash 'perf-1  keymap-table) (circular-list-keynumber-map
					 (white-keys (ew-keynumber 'c1)
						     (ew-keynumber 'b4)))
	(gethash 'perf-2 keymap-table) (circular-list-keynumber-map
					(white-keys (ew-keynumber 'c1)
						    (ew-keynumber 'c6)))
	(gethash 'chromatic keymap-table) (wrapping-keynumber-map
					   :min (ew-keynumber 'c3)
					   :max (ew-keynumber 'e5)))
  (setf (gethash 1 remark-table) "SD3 Metals Mongolian Chimes 1  RR 1"
	(gethash 2 remark-table) "SD3 Metals Mongolian Chimes 2  RR 1"
	(gethash 'perf-1 remark-table) "SD3 Metals Mongolian Chimes 1 Perf  RR 1"
	(gethash 'perf-2 remark-table) "SD3 Metals Mongolian Chimes Various Perf   RR 1"
	(gethash 'chromatic remark-table) "SD3 Metals Mongolian Chimes Chromatic  RR 1")
  (sd3-instrument mongolian-chimes ew-sd3-metals keymap-table remark-table)
  (sd3-query ?mongolian-chims 'mongolian-chimes remark-table))
	
	
