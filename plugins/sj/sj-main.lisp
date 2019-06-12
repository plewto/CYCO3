;;;; CYCO plugins sj sj-main.lisp  05-June-2019
;;;;


;; general      rhythm   pad
;; 01 TXA       TXA      TXA
;; 02 TXB       TXB      TXB
;; 03 TXC       TXC      TXC
;; 04 SY35      SY35     SY35
;; 05 R3        R3       R3
;; 06 BASS      BASS     BASS
;; 07 GUITAR    GUITAR   GUITAR
;; 08 ROMA      ROMA     ROMA 
;; 09 VOP       SAMPLER  VOP
;; 10 DRUMS     DRUMS    DRUMS
;; 11 PROA      PROA     PROA
;; 12 PROB      PROB     CASIO
;; 13 SDA       SDA      ROMAB
;; 14 SDB       SDB      VOPB
;; 15 MU100R    SDC      MU100R
;; 16 BEEP      SDD      OBX/SAMPLER


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
(meta-channel! :SDB    14 "Stormdrum")
(meta-channel! :MU100R 15 "Yamaha MU100")
(meta-channel! :BEEP   16 "Metronome")

;; Overloaded rhythm track channels
;;
(meta-channel! :SAMPLER 09 "RT Sampler")
(meta-channel! :SDC     15 "RT Stormdrums")
(meta-channel! :SDD     16 "RT Stormdrums")

;; Overloaded pad channels
;;
(meta-channel! :CASIO  12 "")
(meta-channel! :ROMAB  15 "PAD ROMA B")
(meta-channel! :VOPB   16 "PAD Voices of passion B")


(prune-orchestra :force t)

(sub-plugin 'general-midi)
(load-plugin-file "emu/procussion/procussion")
(load-plugin-file "yamaha/yamaha")
(load-plugin-file "korg/korg")
(load-plugin-file "oberheim/oberheim")
(load-plugin-file "casio/casio")
(load-plugin-file "eastwest/eastwest")
 
