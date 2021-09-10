;;;; CYCO Emu procussion plugin: percussion-1
;;;;
;;;; Zone Stack             key range
;;;;  1 360 TempleBlock   : 036 038
;;;;  2 367 Woodblock     : 039 041
;;;;  3 365 Clave         : 042 044
;;;;  4 361 Agogo Bell    : 045 047
;;;;  5 362 Triangle      : 048 050
;;;;  6 363 TriangleMute  : 051 053
;;;;  7 383 Mambo Open    : 054 056
;;;;  8 384 Mambo Closed  : 057 059
;;;;  9 385 Campana Open  : 060 062
;;;; 10 386 Campana Heel  : 063 065
;;;; 11 387 ChaCha Open   : 066 068
;;;; 12 388 ChaCha Closed : 069 071
;;;; 13 389 Cabasa Front  : 072 074
;;;; 14 390 Cabasa Back   : 075 077
;;;; 15 391 Cabasa Roll   : 078 080
;;;; 16 393 Guiro up      : 081 083
;;;; 17 394 ShakerNet     : 084 086
;;;; 18 395 Shaker Snap   : 087 089
;;;; 19 396 Shaker Back   : 090 092
;;;; 20 397 Shaker Front  : 093 096
;;;; 21 365 Clave         : 098 098 *
;;;; 22 365 Clave         : 098 098 *
;;;; 23 392 Guiro Down    : 100 100
;;;; 24 424 VlctyGuiro    : 101 101
;;;;
;;;; PERCUSSION-1
;;;;    p1-block
;;;;    p1-temple
;;;;    p1-agogo
;;;;    p1-mambo
;;;;    p1-campana
;;;;    p1-chacha
;;;;    p1-triangle
;;;;    p1-cabasa
;;;;    p1-shaker
;;;;    p1-clave
;;;;    p1-guiro


(let* ((block-list '(39 40 41))
       (temple-list '(36 37 38))
       (agogo-list  '(45 46 47))
       (mambo-list '((open      . (054 ))
       		     (closed    . (057 ))
       		     (open-2    . (055 ))
       		     (closed-2  . (058 ))
       		     (mambo-3   . (056 ))
       		     (closed-3  . (059 ))))
       (campana-list '((open    . (060 ))
       		       (heel    . (063 ))
       		       (open-2  . (061 ))
       		       (heel-2  . (062 ))
       		       (open-3  . (062 ))
       		       (heel-3  . (065 ))))
       (chacha-list '((open     . (066 ))
       		     (closed    . (069 ))    
       		     (open-2    . (067 ))
       		     (closed-2  . (070 ))
       		     (open-3    . (068 ))
       		     (closed-3  . (071 ))))
       (triangle-list '((open   . (048 ))
       			(mute   . (051 ))
       			(open-2 . (049 ))
       			(mute-2 . (052 ))
       			(open-3 . (050 ))
       			(mute-3 . (053 ))))
       (cabasa-list '((front    . (072))
       		      (back     . (075))
       		      (roll     . (078))
       		      (front-2  . (073))
       		      (back-2   . (076))
       		      (roll-2   . (079))
       		      (front-3  . (074))
       		      (back-3   . (077))
       		      (roll-3   . (080))))
       (shaker-list '((net      . (084))
       		      (snap     . (087))
       		      (back     . (090))
       		      (front    . (093))
       		      (net-2    . (085))
       		      (snap-2   . (088))
       		      (back-2   . (091))
       		      (front-2  . (094))
       		      (net-3    . (086))
       		      (snap-3   . (089))
       		      (back-3   . (092))
       		      (front-3  . (096))))
       (clave-list '(98 42 43 44))
       (guiro-list '(101 81 100 82 83)))
	
  (defun percussion-1 (&key (parent procussion)(channel nil))
    (let ((p1 (make-instrument 'percussion-1
			       :parent parent
			       :channel channel
			       :keynumber-map (procussion-keymap 36 101)
			       :program (procussion-program 'percussion1))))
      (defparameter p1-block (make-instrument 'p1-block
					      :parent p1
					      :remarks "Wood Block"
					      :keynumber-map (circular-list-keynumber-map block-list)))
      (defparameter p1-temple (make-instrument 'p1-temple
					       :parent p1
					       :remarks "Temple Block"
					       :keynumber-map (circular-list-keynumber-map temple-list)))
      (defparameter p1-agogo (make-instrument 'p1-agogo
					      :parent p1
					      :remarks "Bell"
					      :keynumber-map (circular-list-keynumber-map agogo-list)))
      (defparameter p1-mambo (make-instrument 'p1-mambo
					      :parent p1
					      :remarks "Bell"
					      :keynumber-map (procussion-subkey-map mambo-list)))
      (defparameter p1-campana (make-instrument 'p1-campana
						:parent p1
						:remarks "Bell"
						:keynumber-map (procussion-subkey-map campana-list)))
      (defparameter p1-chacha (make-instrument 'p1-chacha
					       :parent p1
					       :remarks "Bell"
					       :keynumber-map (procussion-subkey-map chacha-list)))
      (defparameter p1-triangle (make-instrument 'p1-triangle
						 :parent p1
						 :keynumber-map (procussion-subkey-map triangle-list)))
      (defparameter p1-cabasa (make-instrument 'p1-cabasa
					       :parent p1
					       :keynumber-map (procussion-subkey-map cabasa-list)))
      (defparameter p1-shaker (make-instrument 'p1-shaker
					       :parent p1
					       :remarks "Shaker net"
					       :keynumber-map (procussion-subkey-map shaker-list)))
      (defparameter p1-clave (make-instrument 'p1-clave
					      :parent p1
					      :keynumber-map (circular-list-keynumber-map clave-list)))
      (defparameter p1-guiro (make-instrument 'p1-guiro
					      :parent p1
					      :keynumber-map (circular-list-keynumber-map guiro-list)))
      (param percussion-1 p1)
      p1)))




       

