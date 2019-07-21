;;;; CYCO plugins eastwest eastwest.lisp
;;;;
;;;; Eastwest/Quantumleap sample libraries


;; (defmacro ew-keynumber (kn)
;;   `(+ (keynumber ,kn) 24))

(load-plugin-file "eastwest/keyswitch.lisp")


(defun ew-keynumber (kn)
  (+ (keynumber kn) 24))


(instrument eastwest
	    :parent *root-instrument*
	    :transient nil)

;; Minstry of Rock 2
;;
(instrument ew-bass
	    :parent eastwest
	    :channel (meta-channel :bass)
	    :transient nil)

(instrument ew-guitar
	    :parent eastwest
	    :channel (meta-channel :guitar)
	    :transient nil)

(instrument ew-drums
	    :parent eastwest
	    :channel (meta-channel :drums)
	    :transient nil)
(load-plugin-file "eastwest/mor2/basses")
(load-plugin-file "eastwest/mor2/guitars")
(load-plugin-file "eastwest/mor2/drums")

;; Gypsy ~ renamed Roma
;;
(instrument ew-roma
	    :parent eastwest
	    :channel (meta-channel :roma)
	    :transient nil)
(load-plugin-file "eastwest/roma/roma")
(load-plugin-file "eastwest/roma/accordians")
(load-plugin-file "eastwest/roma/guitars")

;; Voices of Passion (VOP)
;;
(instrument ew-vop
	    :parent eastwest
	    :channel (meta-channel :vop)
	    :transient nil)
(load-plugin-file "eastwest/vop/american")
(load-plugin-file "eastwest/vop/bulgarian")
(load-plugin-file "eastwest/vop/indian")
(load-plugin-file "eastwest/vop/syrian")
(load-plugin-file "eastwest/vop/welsh")


;; Stormdrum3 (sd3)
;;
(instrument ew-sd3
	    :parent eastwest
	    :channel (meta-channel :sdA)
	    :transient nil)

(instrument ew-sd3-bigdrums
	    :parent ew-sd3
	    :transient nil)

(instrument ew-sd3-gongs
	    :parent ew-sd3
	    :transient nil)

(instrument ew-sd3-metals
	    :parent ew-sd3
	    :transient nil)

(instrument ew-sd3-shakers
	    :parent ew-sd3
	    :transient nil)

(instrument ew-sd3-smalldrums
	    :parent ew-sd3
	    :transient nil)

(instrument ew-sd3-taiko
	    :parent ew-sd3
	    :transient nil)

(instrument ew-sd3-woods
	    :parent ew-sd3
	    :transient nil)

(instrument ew-sd3-loops
	    :parent ew-sd3
	    :transient nil)

(load-plugin-file "eastwest/sd3/sd3")



