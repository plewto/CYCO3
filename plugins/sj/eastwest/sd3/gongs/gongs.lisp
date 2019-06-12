;;;; CYCO plugins sj eastwest sd3 gongs gongs.lisp
;;;;
;;;; ew-sd3-gongs
;;;;  |
;;;;  +-- clockworks  (1 2 3)
;;;;  +-- waterphones (1 2 3 percussion)
;;;;  +-- woohan-gong (10 21 35 22-bowed)
;;;;  +-- thai-gong   (hits rolls)
;;;;  +-- turkish-cymbal
;;;;  +-- nipple-gong
;;;;  +-- mongolian-gong
;;;;

(load-plugin-file "eastwest/sd3/gongs/clockworks")
(load-plugin-file "eastwest/sd3/gongs/waterphones")
(load-plugin-file "eastwest/sd3/gongs/woohan")
(load-plugin-file "eastwest/sd3/gongs/thai")


(param turkish-cymbal nil)

(defun turkish-cymbal (&key (parent ew-sd3-gongs)
			    channel dynamic-map remarks)
  (setf turkish-cymbal (make-instrument 'turkish-cymbal
					:transient t
					:parent parent
					:channel channel
					:dynamic-map dynamic-map
					:remarks (or remarks "24in Turkish Epic Cymbal")
					:keynumber-map (circular-list-keynumber-map
							(white-keys (ew-keynumber 'c1)
								    (ew-keynumber 'b4))))))
(param nipple-gong nil)

(defun nipple-gong (&key (parent ew-sd3-gongs)
			    channel dynamic-map remarks)
  (setf nipple-gong (make-instrument 'nipple-gong
					:transient t
					:parent parent
					:channel channel
					:dynamic-map dynamic-map
					:remarks (or remarks "30in Burmese Nipple Gong")
					:keynumber-map (circular-list-keynumber-map
							(white-keys (ew-keynumber 'c1)
								    (ew-keynumber 'd3))))))

(param mongolian-gong nil)

(defun mongolian-gong (&key (parent ew-sd3-gongs)
			    channel dynamic-map remarks)
  (setf mongolian-gong (make-instrument 'mongolian-gong
					:transient t
					:parent parent
					:channel channel
					:dynamic-map dynamic-map
					:remarks (or remarks "8in Mongolian Gong")
					:keynumber-map (circular-list-keynumber-map
							(white-keys (ew-keynumber 'c1)
								    (ew-keynumber 'c2)))))) 




