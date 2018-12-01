;;;; CYCO General MIDI program map
;;;;

(constant +general-midi-programs+
	  '((PIANO1            . (  0 "Acoustic Piano"))
            (PIANO2            . (  1 "Bright Acoustic Piano"))
            (PIANO3            . (  2 "Electric Grand Piano"))
            (HONKYTONK         . (  3 "Honky tonk Piano"))
            (EPIANO1           . (  4 "Electric Piano 1"))
            (EPIANO2           . (  5 "Electric Piano 2"))
            (HARPSI            . (  6 "Harpsichord"))
            (CLAVI             . (  7 "Clavi"))
            (CELESTA           . (  8 "Celesta"))
            (GLOCK             . (  9 "Glockenspiel"))
            (MBOX              . ( 10 "Music Box"))
            (VIBES             . ( 11 "Vibraphone"))
            (MARIMBA           . ( 12 "Marimba"))
            (XYLOPHONE         . ( 13 "Xylophone"))
            (TUBE-BELLS        . ( 14 "Tubular Bells"))
            (DULCIMAR          . ( 15 "Dulcimer"))
            (ORGAN1            . ( 16 "Drawbar Organ"))
            (ORGAN2            . ( 17 "Percussive Organ"))
            (ORGAN3            . ( 18 "Rock Organ"))
            (ORGAN4            . ( 19 "Church Organ"))
            (REED-ORGAN        . ( 20 "Reed Organ"))
            (ACCORD1           . ( 21 "Accordion"))
            (HARMONICA         . ( 22 "Harmonica"))
            (ACCORD2           . ( 23 "Accordion 2"))
            (GTR-NYLON         . ( 24 "Acoustic Guitar (nylon)"))
            (GTR-STEEL         . ( 25 "Acoustic Guitar (steel)"))
            (GTR-JAZZ          . ( 26 "Electric Guitar (jazz)"))
            (GTR-CLEAN         . ( 27 "Electric Guitar (clean)"))
            (GTR-MUTED         . ( 28 "Electric Guitar (muted)"))
            (GTR-OVERDRIVE     . ( 29 "Overdriven Guitar"))
            (GTR-DISTORTION    . ( 30 "Distortion Guitar"))
            (GTR-HARMONICS     . ( 31 "Guitar Harmonics"))
            (BASS-ACSTC        . ( 32 "Acoustic Bass"))
            (BASS-FINGER       . ( 33 "Electric Bass (finger)"))
            (BASS-PICK         . ( 34 "Electric Bass (pick)"))
            (FRETLESS          . ( 35 "Fretless Bass"))
            (SLAP-1            . ( 36 "Slap Bass 1"))
            (SLAP-2            . ( 37 "Slap Bass 2"))
            (SYNBASS-1         . ( 38 "Synth Bass 1"))
            (SYNBASS-2         . ( 39 "Synth Bass 2"))
            (VIOLIN            . ( 40 "Violin"))
            (VIOLA             . ( 41 "Viola"))
            (CELLO             . ( 42 "Cello"))
            (CONTRABASS        . ( 43 "Contrabass"))
            (STRINGS-TREM      . ( 44 "Tremolo Strings"))
            (STRINGS-PIZZ      . ( 45 "Pizzicato Strings"))
            (HARP              . ( 46 "Harp "))
            (TIMPANI           . ( 47 "Timpani"))
            (STRINGS-ENS1      . ( 48 "String Ensemble 1"))
            (STRINGS-ENS2      . ( 49 "String Ensemble 2"))
            (SYNSTRINGS1       . ( 50 "SynthStrings 1"))
            (SYNSTRINGS2       . ( 51 "SynthStrings 2"))
            (CHOIR             . ( 52 "Choir Aaah"))
            (OOO               . ( 53 "Voice ooo"))
            (SYNVOX            . ( 54 "Synth Voice"))
            (ORCH-HIT          . ( 55 "Orchestra Hit"))
            (TRUMPET           . ( 56 "Trumpet"))
            (TROMBONE          . ( 57 "Trombone"))
            (TUBA              . ( 58 "Tuba"))
            (TRUMPET-MUTE      . ( 59 "Mute Trumpet"))
            (FRENCH-HORN       . ( 60 "French Horn"))
            (BRASS-SECTION     . ( 61 "Brass section"))
            (SYNBRASS1         . ( 62 "Synth Brass 1"))
            (SYNBRASS2         . ( 63 "Synth Brass 2"))
            (SAX-SORPANO       . ( 64 "Sorpano Sax"))
            (SAX-ALTO          . ( 65 "Alto Sax"))
            (SAX-TENOR         . ( 66 "Tenor Sax"))
            (SAX-BARI          . ( 67 "Baritone Sax"))
            (OBOE              . ( 68 "Oboe"))
            (ENGLISH-HORN      . ( 69 "English Horn"))
            (BASSOON           . ( 70 "Bassoon"))
            (CLARINET          . ( 71 "Clarinet"))
            (PICCOLO           . ( 72 "Piccolo"))
            (FLUTE             . ( 73 "Flute"))
            (RECORDER          . ( 74 "Recorder"))
            (PANFLUTE          . ( 75 "Pan Flute"))
            (BOTTLE            . ( 76 "Blown Bottle"))
            (SHAUACHI          . ( 77 "Shakuachi"))
            (WHISTLE           . ( 78 "Whistle"))
            (OCRINA            . ( 79 "Ocrina"))
            (SQUARE            . ( 80 "Lead 1 square"))
            (SAW               . ( 81 "Lead 2 Saw"))
            (CALLIOPE          . ( 82 "Lead 3 Calliope"))
            (CHIFF             . ( 83 "Lead 4 chiff"))
            (CHARANG           . ( 84 "Lead 5 charang"))
            (VOICE             . ( 85 "Lead 6 Voice"))
            (FIFTHS            . ( 86 "Lead 7 Fifths"))
            (BASSLEAD          . ( 87 "Lead 8 bass+lead"))
            (PAD1              . ( 88 "Pad 1"))
            (PAD2              . ( 89 "Pad 2"))
            (PAD3              . ( 90 "Pad 3 (polysynth)"))
            (PAD4              . ( 91 "Pad 4"))
            (PAD5              . ( 92 "Pad 5"))
            (PAD6              . ( 93 "Pad 6"))
            (PAD7              . ( 94 "Pad 7"))
            (PAD8              . ( 95 "Pad 8"))
            (FX1               . ( 96 "FX1 (Rain)"))
            (FX2               . ( 97 "FX2"))
            (FX3               . ( 98 "FX3"))
            (FX4               . ( 99 "FX4"))
            (FX5               . (100 "FX5"))
            (FX6               . (101 "FX6"))
            (FX7               . (102 "FX7"))
            (FX8               . (103 "FX8"))
            (SITAR             . (104 "Sitar"))
            (BANJO             . (105 "Banjo"))
            (SHAMISEN          . (106 "Shamisen"))
            (KOTO              . (107 "Koto"))
            (KALIMBA           . (108 "Kalimba"))
            (BAGPIPE           . (109 "Bag Pipe"))
            (FIDDLE            . (110 "Fiddle"))
            (SHANI             . (111 "Shani"))
            (TINKLE-BELL       . (112 "Tinkle Bell"))
            (AGOGO             . (113 "Agogo"))
            (STEELDRUM         . (114 "Steel Drum"))
            (WOODBLOCK         . (115 "Woodblock"))
            (TAIKODRUM         . (116 "Taiko Drum"))
            (MELODIC-TOM       . (117 "Melodic Tom"))
            (SYNDRUM           . (118 "Synth Drum "))
            (REVCYMBAL         . (119 "Reverse Cymbal"))
            (FRETNSE           . (120 "Guitar Fret Noise"))
            (BREATH            . (121 "Breath Noise"))
            (SEASHORE          . (122 "Seashore"))
            (BIRDS             . (123 "Bird Tweets"))
            (TELEPHONE         . (124 "Telephone Ring"))
            (HELICOPTER        . (125 "Helicopter"))
            (APPLAUSE          . (126 "Applause"))))
            
(flet ((format-assignment (index)
	 (let ((frmt "[~3D] '~25A ~15s~%")
	       (q (nth index +general-midi-programs+)))
	   (format t frmt (second q)(first q)(third q)))))

  (defun ?general-midi-programs ()
    "Displays list of general MIDI programs."
    (format t "General MIDI Programs~%")
    (dotimes (i 128)
      (format-assignment i))))

(defun general-midi-program (p)
  "Returns general MIDI program number.
The argument p may be an integer between 0 and 127 inclusive, or
a symbolic general-midi program name.
Use (?GENEAL-MIDI-PROGRAMS) for a list of valid programs."
  (or (and (integerp p)(<= 0 p)(< p 128) p)
      (second (assoc p +general-midi-programs+))
      (progn
	(cyco-warning
	 (sformat "Invalid General MIDI program: ~A" p)
	 "Using default program 0."
	 "Try (?GENERAL-MIDI-PROGRAMS) for list of options.")
	0)))