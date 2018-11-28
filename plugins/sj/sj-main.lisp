;;;; sj-config

;;; Primary MIDI Channels
;;;
(meta-channel! :TXA     1 "Yamaha TX816 A")
(meta-channel! :TXB     2 "Yamaha TX816 B")
(meta-channel! :TXC     3 "Yamaha TX816 C")
(meta-channel! :SY35    4 "Yamaha SY35")
(meta-channel! :R3      5 "Korg R3")
(meta-channel! :Q1      6 "Quantum Leap 1")
(meta-channel! :Q2      7 "Quantum Leap 2")
(meta-channel! :Q3      8 "Quantum Leap 3")
(meta-channel! :Q4      9 "Quantum Leap 4")
(meta-channel! :Q5     10 "Quantum Leap 5")
(meta-channel! :PRO1   11 "Emu Procussion 1")
(meta-channel! :PRO2   12 "Emu Procussion 2")
(meta-channel! :PRO3   13 "Emu Procussion 3")
(meta-channel! :LLIA   14 "SuperCollider")
(meta-channel! :MU100R 15 "Yamaha MU100R ")
(meta-channel! :BEEP   16 "Metronome")

;;; Secondary Channels
;;;
(meta-channel! :BASS   :Q1 "QL MOR Bass")
(meta-channel! :GUITAR :Q2 "QL MOR/Gypsy guitar")
(meta-channel! :GYPSY  :Q3 "QL Gypsy")
(meta-channel! :VOP    :Q4 "QL VOP")
(meta-channel! :DRUMS  :Q5 "QL MOR Percussion")
(meta-channel! :PROKIT :PRO1 "Procussion Standard kit")
(meta-channel! :PROCYM :PRO2 "Procussion cymbals")

;;; Piggyback Channels
;;;
(meta-channel! :OBX        :R3   "Oberheim Matrix1000")
(meta-channel! :SAMPLER    :SY35 "Korg Micro sampler")
(meta-channel! :VOP-ALT    :TXC  "VOP alternate")
(meta-channel! :MU100R-ALT :SY35 "MU100R Alternate")
(meta-channel! :LLIA-ALT   :SY35 "LLia Alternate")

(prune-orchestra :force t)

(sub-plugin 'general-midi)       
(load-plugin-file "emu/procussion/procussion")
(load-plugin-file "yamaha/tx816")
(load-plugin-file "yamaha/sy35")
(load-plugin-file "yamaha/mu100r")
(load-plugin-file "quantumleap/quantumleap")
(load-plugin-file "quantumleap/keyswitch")
(load-plugin-file "quantumleap/mor2/basses")
(load-plugin-file "quantumleap/mor2/guitars")
(load-plugin-file "quantumleap/mor2/percussion")
(load-plugin-file "quantumleap/gypsy/gypsy")
(load-plugin-file "quantumleap/gypsy/accordians")
(load-plugin-file "quantumleap/gypsy/guitars")
(load-plugin-file "quantumleap/vop/american")
(load-plugin-file "quantumleap/vop/bulgarian")
(load-plugin-file "quantumleap/vop/indian")
(load-plugin-file "quantumleap/vop/syrian")
(load-plugin-file "quantumleap/vop/welsh")
(load-plugin-file "korg/r3")
(load-plugin-file "korg/micro-sampler")


