;;;; CYCO qball docs
;;;;
;;;; A QBall is a recombinative note generator.
;;;; The user specifies instrument, time, key-number, duration and
;;;; dynamic patterns.  These are recombined according to their
;;;; individual pattern types to produce MIDI note events.
;;;;

(in-package :cyco-part)

(constant +qball-docstring+
       "Creates new instance of QBALL.
Most keyname arguments default to the parent node values.
name
instruments - Pattern of instruments. A single instrument or list
              of instruments is converted to a Cycle pattern.
:section  - Parent section, defaults to current-section of *project*
:cuefn    - Cueing function
:shuffle  - Shuffle function
:shift    - Fixed offset in seconds added to initial time.
:tempo    - Tempo, beats per minute, defaults to parent tempo
:unit     - Time signature beat unit
:bars     - Number of bars
:beats    - Number of beats per bar
:subbeats - Number of sub-beats per beat
:render-once  -
:transposable - Boolean, 
:cue  - List of times in a format required by cuefn.
        For the default #'bar cuefn, times are specified as a list
        (bar beat subbeat).  Assuming 4/4 time, the first four beats
        of the first bar is specified by ((1 1 1)(1 2 1)(1 3 1)(1 4 1)).
:shuffle - ISSUE add documentation
:key  - Pattern of keynumbers. A single key or list of keys is converted
        to a Cycle pattern.  Nested list are treated as chords.
        A C major arpeggio, followed by a G major chord is specified by 
        (C3 E3 G3 (G3 B3 D4)).   Patterns other then Cycles are allowed.
:dur  - Note on duration. A single value or list of values are converted to 
        a Cycle.  Numeric values are treated as absolute.  Metric expressions
        are scaled by the current tempo.
:amp  - Note amplitudes, A single value or list of values are converted to 
        a Cycle. 
:reset-on-repeat - Boolean.  If true all patterns are reset on each 
        repetition and the qball will produce the same results on
        each call to render-once.  If false the patterns are not reset
        and each call to render-once may produce a different set of events,
        depending on the relative lengths of the patterns.
:remarks - Optional text.

Each :key :dur and :amp values are processed by the keynumber-map,
articulation-map and dynamic-map respectively of each instrument as they are
used.") 
