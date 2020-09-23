;;;; CYCO Plugins ion
;;;;

;; General assignments
(meta-channel! :TXA    01 "Yamaha TX816 A")
(meta-channel! :TXB    02 "Yamaha TX816 B")
(meta-channel! :TXC    03 "Yamaha TX816 C")
(meta-channel! :SY35   04 "Yamaha SY35")
(meta-channel! :R3     05 "Korg R3")
(meta-channel! :BASS   06 "MOR Bass")
(meta-channel! :GUITAR 07 "MOR/Roma guitar")
(meta-channel! :ROMA   08 "Roma")
(meta-channel! :VOP    09 "Voices of Passion")
(meta-channel! :DRUMS  10 "MOR drum kits")
(meta-channel! :PROA   11 "Procussion standard kits")
(meta-channel! :PROB   12 "Procussion cymbals")
(meta-channel! :SDA    13 "Stormdrum")
(meta-channel! :WAVESTATION 14 "Korg Wavestation")
(meta-channel! :MU100R 15 "Yamaha MU100")
(meta-channel! :BEEP   16 "Metronome")

;; Overloaded rhythm track channels
;;
(meta-channel! :SAMPLER 09 "RT Sampler")
(meta-channel! :SDC     15 "RT Stormdrums")
(meta-channel! :SDD     16 "RT Stormdrums")

;; Overloaded pad channels
;;
(meta-channel! :ROMAB  15 "PAD ROMA B")
(meta-channel! :VOPB   16 "PAD Voices of passion B")


(prune-orchestra :force t)

(plugin general-midi)
(load-plugin-file "yamaha/yamaha")
(load-plugin-file "korg/korg")
(load-plugin-file "oberheim/matrix1000")
(load-plugin-file "emu/procussion/procussion")
(load-plugin-file "eastwest/eastwest")

(channel! *metronome* :beep)
(keynumber-map! *metronome* (metronome-keynumber-map
			     :phrase 3
			     :bar 1
			     :beat 0))
(program-number! *metronome* 127)
(articulation-map! *metronome*
		   #'(lambda (d)
		       (dismiss d)
		       0.01))



(param rr-reset nil)
(defun rr-reset ()
  (let* ((event-list '())
	 (controller-number 32))
    (dotimes (channel-index 16)
      (push (cons 0.0 (midi-control-change channel-index controller-number 0)) event-list)
      (push (cons 0.0 (midi-control-change channel-index controller-number 127)) event-list))
    (setf rr-reset
	  (make-raw-part 'rr-reset
			 :events event-list
			 :remarks "Round-robin reset controler 32 on all MIDI channels."))))
	 

(defun snapshot (&optional (fname "~/bin/cyco3"))
  "Saves CYCO snapshot to default location."
  (save-snapshot fname))


(load-plugin-file "legacy")
