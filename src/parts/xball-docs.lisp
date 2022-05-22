;;;; cyco parts xball-docs
;;;;

(in-package :cyco-part)

(setf (documentation 'make-xball 'function)
      "Create new instance of XBALL.
An XBALL is similar to a QBALL but has extended options for using chords. 

NAME           : Symbol, part's name.
INSTRUMENT     : Instrument.  Unlike QBALL, an XBALL may only use a single
                 instrument. 
:SECTION       : Parent section, defaults to current section of current project.
:CUEFN         : Cueing function, defaults to section value.
:SHUFFLE       : Shuffle function, defaults to section value.
:SHIFT         : Time or metric-expression, offset time added to rendered events.
:RENDER-ONCE   : Boolean, if true do not repeat events when section is rendered.
:TEMPO         : Float, defaults to section value.
:UNIT          : Time signature beat unit, defaults to section value.
:BARS          : Time signature bar count, defaults to section value.
:BEATS         : Time signature beats per bar, defaults to section value.
:SUBBEATS      : Time signature subbeats per beat, defaults to section value.
:TRANSPOSABLE  : Boolean, if true allow keynumber value transposition.
:REVERSIBLE    : Boolean, if true allow order of keynumbers to be reversed.
:CHORD-MODEL   : The chord-model, defaults to section value.
:CUE           : List of time-cue points, has identical usage to QBALL.
                 Format must be compatible with the current cueing function.
                 For the default BAR cue function time cues have the form:
                    (bar beat subbeat [optional tick])
                 The following cue-list produces each downbeat of bar 3
                    ((3 1 1)(3 2 1)(3 3 1)(3 4 12))
:KEY          *: Keynumber pattern. Each element must be a valid keynumber.
                 Unlike QBALL, nested chords are not allowed.
                    (c4 60 70)    ;; OK
                    (c4 (c5 70))  ;; NOT ALLOWED
:DUR          *: Duration pattern, elements may be an absolute time in
                 seconds or a metric-expression.
                    Example (0.5 q s) 
:AMP          *: Amplitude pattern, elements may be a 'normalized' float or
                 a valid dynamic symbol.
                    Example (0.1 pp mf fff)
:CHORD        *: Chord type pattern.  May be a chord-name from the current
                 chord-model or a list of key-offsets/keynumbers, the
                 default chord is '[SOLO]
                    Example '([min] (0 1 2))
                For the default *CHORD-TABLE* list are treated as keynumber
                offsets and for keynumber c4, the list (0 1 2) produces the 
                cluster (c4 cs4 d4).

                Foe 'absolute' chord models, such as used by the
                guitar-chords plugin, a list is treated as absolute key
                numbers, and the exact keynumber is irrelevant,  (60 63 67)
                produces a c-minor chord.
                 
:INVERSION    *: Integer chord inversion, has same usage as with STRUMMER.
:OCTAVE       *: Integer chord added octave. has same usage as with STRUMMER.
:STRUM        *: Float or metric-expression. Set chord strum delay either
                 in absolute seconds or relative to the current tempo. 
:DIRECTION    *: Symbol, sets strumming direction, may be one of the
                 following UP DOWN DICE or RANDOM.
:END-TOGETHER    : Boolean, if true strummed notes end at the same time,
                   otherwise the end times are staggered by the strum time.
:RESET-ON-REPEAT : Boolean. If true all internal patterns are reset each
                   time the part is rendered and produce the same series of
                   events.   If nil each render begins with the patterns in
                   their current state and each repetition may produce
                   different combination of events.
:REMARKS         : Optional remarks text.
 

Arguments marked with *: may take many forms.

    1) Literal (single value)
       :key 'c4

    2) List of values are converted to a cycle
       :key '(c4 e4 g4)   --> (cycle :of '(c4 e4 g4))

    3) Pattern/generators are used un-changed
       :key (dice :of '(c4 e4 g4))

    4) As a 'pattern-comprehension' which allows complex nested patterns to
       be expressed more succinctly.

       :key '(cycle (c4 (dice (e4 g4))(line (g4 (dice '(g4 bf4))))))

       Produces the same patterns as

       :key (cycle :of (list 'c4 (dice :of '(e4 g4))(line :of (list 'g4 (dice :of '(g4 bf4))))))


NOTES on differences between QBALL and XBALL:       

  1) XBALL may only use a single instrument.
      QBALL may use a pattern of instruments

      (QBALL name (list piano organ) ... )     ;; **ALLOWED**
      (XBALL name (list piano organ) ... )     ;; **ERROR**

   2) XBALL key pattern may not include embedded chords.
      QBALL key patterns may include embedded chords.

      (QBALL name piano :key '(bf3 (c4 e4 g4) c5) ...)  ;; **ALLOWED**
      (XBALL name piano :key '(bf3 (c4 e4 g4) c5) ...)  ;; **ERROR**

   3) XBALL may use 'pattern-comprehension'. 
      QBALL does not, though it may be added later.

      (QBALL name piano :key '(cycle (60 40 50)) ...)   ;; **ERROR**
      (XBALL name piano :key '(cycle (60 40 50)) ...)   ;; **ALLOWED**")
      
(setf (documentation 'xball 'function)
      "The XBALL macro has nearly the same usage as MAKE-XBALL except it
      binds the new part to the name symbol.  The name argument should be
      quoted for MAKE-XBALL and unquoted for XBALL.")
