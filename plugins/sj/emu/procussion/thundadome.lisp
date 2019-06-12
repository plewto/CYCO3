;;;; CYCO plugins sj emu procussion thundadome
;;;;
;;;;                             3   4         5         6         7         8         9         
;;;; Zone/Stack        Key Range 6789012345678901234567890123456789012345678901234567890123456
;;;; 01 476 golden gate 36  59*  ************************
;;;; 02 440 hallowell   60  96                           *************************************
;;;; 03 442 thundadome  36  36
;;;; 04 442 thundadome  48  48
;;;; 05 442 thundadome  60  60
;;;; 06 060 tam kick 2  61  61
;;;; 07 072 rapclav     63  63
;;;; 08 072 kettldrum   66  66
;;;; 09 258 elec hat 2  68  68
;;;; 10 259 elec hat 3  70  70    
;;;; 11 458 silvoon     72  73
;;;; 12 308 fingercym   75  75
;;;; 13 222 syn tom     78  78
;;;; 14 385 campana op  80  80
;;;; 15 318 hyperreal   82  82
;;;; 16 507 gong pow    85  85
;;;; 17 508 cymbaldrum  87  87
;;;; 18 226 falling tom 90  90
;;;; 19 463 springz     92  92
;;;; 20 376 carillian   94  94
;;;; 21 309 gong o doom 42  42
;;;; 22 309 gong o doom 54  54
;;;; 23 029 wet kick 5  96  96
;;;; 24 536 qoll klang  84  84
;;;;
;;;; thundadome
;;;;    |
;;;;    +-- td-golden-gate
;;;;    +-- td-hallowell
;;;;

(defun thundadome (&key (parent PROB)(channel nil) articulation-map dynamic-map)
  (let* ((inst (instrument thundadome
			   :parent parent
			   :channel channel
			   :program (procussion-program 'thundadome)
			   :articulation-map articulation-map
			   :dynamic-map dynamic-map
			   :transient t)))
    (instrument td-golden-gate
		:remarks "Procussion Thudadome instrument, key range (60..83)"
		:parent inst
		:transient t
		:keynumber-map (basic-keynumber-map :min 60 :max 83))
    (instrument td-hallowell
		:remarks "Procussion Thudadome instrument, key range (60..96)"
		:parent inst
		:transient t
		:keynumber-map (basic-keynumber-map :min 60 :max 96))
    inst))

