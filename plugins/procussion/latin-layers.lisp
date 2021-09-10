;;;; CYCO Emu Procussion plugin: latin-layers
;;;;
;;;; Zone Stack             key range : 
;;;;  1 399 Timbale 1B     : 036 047  : 
;;;;  2 368 Tambourine     : 036 053  :
;;;;  3 351 Hand Drum      : 048 059  :
;;;;  4 396 ShakerBack     : 054 065  :
;;;;  5 365 Clave          : 060 071  :
;;;;  6 392 Guiro Down     : 066 077  :
;;;;  7 404 Tumba Tone     : 072 074  :
;;;;  8 394 ShakerNet      : 078 096  :
;;;;  9 405 Tumba Rim      : 075 077  :
;;;; 10 406 Quinto Tone    : 078 080  :
;;;; 11 407 Quinto SlpOp   : 081 083  :
;;;; 12 408 Quinto SlpCl   : 084 086  :
;;;; 13 413 Macho Tip L    : 087 089  :
;;;; 14 414 Macho Tip R    : 090 092  :
;;;; 15 233 HiHat A 1/3    : 098      : 
;;;; 16 233 HiHat A 1/3    : 062      :
;;;; 17 414 MachoTip R     : 100      :
;;;; 18 416 Macho Tone     : 101      :
;;;; 19 341 RevSnapper     : 103      :
;;;; 20                    :          :
;;;; 21                    :          : 
;;;; 22                    :          :
;;;; 23                    :          :
;;;; 24                    :          :
;;;;


(defun latin-layers (&key (parent procussion) channel)
  (let ((latlay (make-instrument 'latin-layers
				      :parent parent
				      :channel channel
				      :program (procussion-program 'latin-layers)
				      :keynumber-map (procussion-keymap 36 103))))
    (defparameter ll-timbale (make-instrument 'll-timbale
					      :parent latlay
					      :keynumber-map (circular-keynumber-map 36 47)))
    (defparameter ll-hand-drum (make-instrument 'll-hand-drum
						:parent latlay
						:keynumber-map (circular-keynumber-map 48 59)))
    (defparameter ll-clave (make-instrument 'll-clave
					    :parent latlay
					    :keynumber-map (circular-keynumber-map 60 65)))
    (defparameter ll-guiro (make-instrument 'll-guiro
					    :parent latlay
					    :keynumber-map (circular-keynumber-map 66 71)))
    (defparameter ll-tumba (make-instrument 'll-tumba
					    :parent latlay
					    :keynumber-map (circular-keynumber-map 72 77)))
    (defparameter ll-quinto (make-instrument 'll-quinto
					     :parent latlay
					     :keynumber-map (circular-keynumber-map 78 86)))
    (defparameter ll-macho (make-instrument 'll-macho
					    :parent latlay
					    :keynumber-map (circular-keynumber-map 87 92)))
    (defparameter ll-shaker (make-instrument 'll-shaker
					     :parent latlay
					     :keynumber-map (circular-keynumber-map 93 96)))
    (defparameter ll-snaper (make-instrument 'll-snaper
					     :parent latlay
					     :remarks "Miscellaneous samples"
					     :keynumber-map (circular-list-keynumber-map '(98 100 101 103))))
    (defparameter latin-layers latlay)
    latlay))
