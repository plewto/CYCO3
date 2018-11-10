# CYCO Readme

CYCO is a Lisp based music composition language with MIDI and OSC
support. Scores are prepared by creating *Projects* which are saved to
standard MIDI files to be played by an external MIDI file player
(such as [PigIron](https://github.com/plewto/PigIron)).


CYCO began several years ago as a set of extensions for
[Common Music 2](http://commonmusic.sourceforge.net/cm/res/doc/cm.html)
called CMU (Common Music Utility).  CMU added higher level 
structures which better suited my composition style.   Eventually Common
Music moved to version 3 and it became increasingly difficult to port CMU
with each operating system update.  In reality CMU used a small subset
of Common Music's features and I decided to reimplement these from scratch,
thus severing the dependence of Common Music.



CYCO provides a hierarchical composition structure.  At the highest level are
**Projects**.  Each project is composed of **Sections**, where a section
corresponds to a major division of the piece; intro, movement, verse,
chorus etc...  Each section has one or more parallel **Parts**. There are a
few different types of Part but they all combine a set of instruments with
instructions on what those instruments are to play.

#####Some Key Features:
* Project centric design.
* Flexible functional time-signature support.
* Algorithmic pattern generation.
* Symbolic key-numbers, dynamics and note durations.
* "Metric Expressions"  i.e. add a half-note and a thirty-second-note
triplet, scale the whole thing by 0.6.
* Chord strumming patterns.
* Recombinative patterns, i.e. a key sequence (C E G) and dynamic sequence
(MF PP) may combine to produce (C MF)(E PP)(G MF)(C PP)(E MF)(G PP) among
other possibilities.
* Flexible instrument definitions.
* Open configuration for customization to fit specific needs.
* Save snapshot executables (Under SBCL only)
* OSC control over external MIDI player.




