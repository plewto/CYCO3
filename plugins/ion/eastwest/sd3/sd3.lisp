;;;; CYCO plugins sj eastwest sd3 sd3.lisp
;;;;


(defun sd3-get-keynumber-map (instrument-name variation hashmap)
  (or (gethash variation hashmap)
      (progn
	(cyco-warning
	 "Invalid SD3 variation"
	 (sformat "Instrument: ~A    variation: ~A" instrument-name variation)
	 "Using default keynumber-map")
	+default-keynumber-map+)))
	
(defun sd3-get-remarks (instrument-name variation hashmap default)
  (or default
      (gethash variation hashmap)
      (sformat "Invalid ~A variation: ~A" instrument-name variation)))


;;; Defines function for creation of SD3 instrument.
;;; name - instrument name
;;; parent - default parent instrument
;;; keymap-table hash-table maps instrument variations symbols to keynumber-map
;;; remarks-table hash-table maps instrument variation symbol to remarks text.
;;;
(defmacro sd3-instrument (name parent keymap-table remarks-table)
  `(defun ,name (variation &key (parent ,parent) channel dynamic-map remarks)
     (setf ,name (make-instrument ',name
				  :transient t
				  :parent parent
				  :channel channel
				  :dynamic-map dynamic-map
				  :keynumber-map (sd3-get-keynumber-map ',name variation ,keymap-table)
				  :remarks (sd3-get-remarks ',name variation ,remarks-table remarks)))))


;;; Defins sd3 instrument help function
;;; fn-name - name of function, name should begin with question mark ?foo
;;; name - the quoted instrument name  'foo
;;; remarks-table hash-table maps instrument variation symbol to remarks text.
;;;
(defmacro sd3-query (fn-name name remarks-table)
  `(defun ,fn-name ()
     (format t "SD3 ~A~%" ',name)
     (maphash #'(lambda (key value)
		  (format t "  Variation ~12A  ~A~%" key value))
	      ,remarks-table)))

(load-plugin-file "eastwest/sd3/bigdrums/bigdrums")
(load-plugin-file "eastwest/sd3/gongs/gongs")
(load-plugin-file "eastwest/sd3/metals/metals")
(load-plugin-file "eastwest/sd3/loops")
(load-plugin-file "eastwest/sd3/shakers/shakers")
(load-plugin-file "eastwest/sd3/smalldrums/smalldrums")
(load-plugin-file "eastwest/sd3/taiko/taiko")
(load-plugin-file "eastwest/sd3/woods/woods")


