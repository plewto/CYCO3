;;;; CYCO plugins sj eastwest sd3 taiko taiko.lisp
;;;;
;;;; hira   (44 48 60)
;;;; negado (32 36-a 36-b 46)
;;;; okedo  (20 26)
;;;; odaiko 
;;;; shime-daiko  (14-16 14-remo 14-japan 15 ensemble metal)
;;;;


(param odaiko nil)

(defun odaiko (&key (parent ew-sd3-taiko)
		   channel dynamic-map remarks)
  (setf odaiko (make-instrument 'odaiko
			       :transient t
			       :parent parent
			       :channel channel
			       :dynamic-map dynamic-map
			       :remarks (or remarks "Remo 28in Odaiko")
			       :keynumber-map (circular-list-keynumber-map
					       (white-keys (ew-keynumber 'c1)
							   (ew-keynumber 'e1))))))

(load-plugin-file "eastwest/sd3/taiko/hira")
(load-plugin-file "eastwest/sd3/taiko/negado")
(load-plugin-file "eastwest/sd3/taiko/okedo")
(load-plugin-file "eastwest/sd3/taiko/shime-daiko")

						
