;;;; CYCO plugins ion korg wavestation
;;;;


(param wavestation (make-instrument 'wavestation
				    :channel (meta-channel :wavestation)))


(defmacro wavestation (name bank program &key
			    (parent wavestation)
			    (channel nil)
			    remarks
			    keynumber-map
			    articulation-map
			    dynamic-map)
