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

(load-config-file "general-midi/gm-program-map")
(load-config-file "sj/emu/procussion/procussion")
(load-config-file "sj/yamaha/tx816")
(load-config-file "sj/yamaha/sy35")
(load-config-file "sj/yamaha/mu100r")
(load-config-file "sj/quantumleap/quantumleap")
(load-config-file "sj/quantumleap/keyswitch")
(load-config-file "sj/quantumleap/mor2/basses")
(load-config-file "sj/quantumleap/mor2/guitars")
(load-config-file "sj/quantumleap/mor2/percussion")
(load-config-file "sj/quantumleap/gypsy/gypsy")
(load-config-file "sj/quantumleap/gypsy/accordians")
(load-config-file "sj/quantumleap/gypsy/guitars")
(load-config-file "sj/quantumleap/vop/american")
(load-config-file "sj/quantumleap/vop/bulgarian")
(load-config-file "sj/quantumleap/vop/indian")
(load-config-file "sj/quantumleap/vop/syrian")
(load-config-file "sj/quantumleap/vop/welsh")
(load-config-file "sj/korg/r3")
(load-config-file "sj/korg/micro-sampler")


