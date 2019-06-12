;;;; CYCO plugins sj eastwest sd3 metals cengceng.lisp
;;;;
;;;; cengceng (klang perf)


(param cengceng nil)


(let ((keymap-table (make-hash-table))
      (remark-table (make-hash-table)))
  (setf (gethash 'klang keymap-table)
	(circular-list-keynumber-map
	 (white-keys (ew-keynumber 'c1)
		     (ew-keynumber 'c6))))
  (setf (gethash 'klang remark-table) "SD3 Metals CengCeng   RR 1")
  (setf (gethash 'perf keymap-table)
	(circular-list-keynumber-map
	 (white-keys (ew-keynumber 'c1)
		     (ew-keynumber 'b5))))
  (setf (gethash 'perf remark-table) "SD3 Metals CengCeng Perf   110 BPM")
  (sd3-instrument cengceng ew-sd3-metals keymap-table remark-table)
  (sd3-query ?cengceng 'cengceng remark-table))
