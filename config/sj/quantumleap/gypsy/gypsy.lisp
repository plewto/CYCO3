;;;; PigIron CYCO sj config quantumleap gypsy gypsy
;;;;
;;;; Instruments
;;;;
;;;;     cimbalon
;;;;     trombone
;;;;     violin
;;;;     bandoneon               accordians
;;;;     campana 
;;;;     excelsior 
;;;;     silvestri
;;;;     classical-guitar        guitars
;;;;     django 
;;;;     django-chords 
;;;;     flamenco 
;;;;     flamenco-chords 
;;;;     spanish-guitar

(defun cimbalon (&key (parent ql-gypsy)(channel nil)
		      keynumber-map articulation-map dynamic-map
		      remarks)
  (let ((inst (make-instrument 'cimbalon
			       :parent parent
			       :channel (meta-channel channel)
			       :remarks (or remarks "Gypsy Dulcimer")
			       :transient t
			       :keynumber-map keynumber-map
			       :articulation-map articulation-map
			       :dynamic-map dynamic-map)))
    (--make-ql-program-map inst '(sus double tremolo))
    (param cimbalon inst)
    inst))

(defun trombone (&key (parent ql-gypsy)(channel nil)
		      keynumber-map articulation-map dynamic-map
		      remarks)
  (let ((inst (make-instrument 'trombone
			       :parent parent
			       :channel (meta-channel channel)
			       :remarks (or remarks "Gypsy Trombone")
			       :transient t
			       :keynumber-map keynumber-map
			       :articulation-map articulation-map
			       :dynamic-map dynamic-map)))
    (--make-ql-program-map inst '(sus sus-farty staccato staccato-long marc
				      dimuendo sforzando crescendo))
    (param trombone inst)
    inst))

(defun violin (&key (parent ql-gypsy)(channel nil)
		    keynumber-map articulation-map dynamic-map
		    remarks)
  (let ((inst (make-instrument 'violin
			       :parent parent
			       :channel (meta-channel channel)
			       :remarks (or remarks "Gypsy Violin")
			       :transient t
			       :keynumber-map keynumber-map
			       :articulation-map articulation-map
			       :dynamic-map dynamic-map)))
    (--make-ql-program-map inst '(sus-vib sus-vib2 sus-vib3 passionato expressive 
					  expressive-slow sus-vib-accent sforzando 
					  sul-tasto sul-tasto-exp accent-novib 
					  sforzando-novib sus-novib martele1 
					  martele2 spiccato spiccato2 
					  spiccato-long spiccato-long2 
					  left-pizzacato repitions harmonics 
					  ponticello bounce)) 
    (param violin inst)
    inst))

