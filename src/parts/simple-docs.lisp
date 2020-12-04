;;;; CYCO parts simple-docs.lisp
;;;;
;;;; Documentation for SIMPLE-PART
;;;;
;;;; SIMPLE-PART provides a straight forward event specifications
;;;; comparable to a stripped down STRUMMER part.  A SIMPLE-PART may
;;;; produce note (including chords), channel-pressure, controller, bend
;;;; and program-change events.  
;;;; 
;;;; Mechanics.
;;;;
;;;;   The SIMPLE-PART class extends PART with a list of SIMPLE-STATE
;;;;   objects.   The simple-state struct contains fields for both "real"
;;;;   events, and several support values which do not directly correspond
;;;;   to MIDI events.
;;;;   
;;;;    source             : The user supplied source, for debugging only
;;;;    time-specification : Event time specification in whatever format
;;;;                         the part cue-function expects.
;;;;    time               : floating point time corresponding to the
;;;;                         time-specification.  This value may be modified
;;;;                         by the parts shuffle function.
;;;;    chord-type         : chord specification, may either be a symbol
;;;;                         naming a chord type from the part's chord-model
;;;;                         or a list of keynumber offsets.
;;;;    chord-inversion    : integer -12 ... +12, see chord inversion
;;;;    chord-octave       : integer -3 ... +3, see chord inversion
;;;;    articulation       : metric-expression sets note duration.
;;;;    dynamic            : sets note velocity.
;;;;
;;;;  Changes made to any of the above fields remain in effect until they
;;;;  are explicitly updated.
;;;;
;;;;  The following fields generate one or more MIDI events.  After the
;;;;  events are generated the field is immediately cleared.
;;;;
;;;;    key               : Note event keynumber, this value is proceeded
;;;;                        by each instruments keynumber-map prior to use.
;;;;    pressure          : normalized float, channel-pressure event
;;;;    controller-number : int MIDI controller number 0..127
;;;;    controller-value  : normalized float, only used with controller-number
;;;;    bend              : signed normalized pitch bend -1..+1
;;;;    program-number    : Generates program-change events.  This value is
;;;;                        processed by each instruments program-change-map 
;;;;                        prior to use.
;;;;    program-bank      : Used only in conjunction with program-number
;;;;
;;;;   When a SIMPLE-PART is created there is an initial SIMPLE-STATE object
;;;;   with default values and an empty events list.   As the user supplied
;;;;   events are evaluated they update the parameters of the state object.
;;;;   If the user specifies a "real" event, the state object is updated and
;;;;   a cloned copy is pushed to the events list.  A soft-reset is then
;;;;   applied to the state object which clears the key, pressure,
;;;;   controller, bend and program values while leaving the other fields
;;;;   alone.
;;;;
;;;; Definitions:
;;;; 
;;;;  The events list supplied by the user is a nested list
;;;;  Each sub-list is called an "event" and is composed of one or more
;;;;  "clauses", where each clause begins with a keyword followed by zero,
;;;;  one or two values, depending on the specific keyword.
;;;;

(in-package :cyco-part)

(constant +make-simple-part-docstring+
"Creates an instance of SIMPLE-PART, a part type for generating explicit
MIDI events  

name          - Symbol
instruments   - Instrument or list of instruments.
                All instruments are layered.
:section      - Parent section, defaults to current section of *project*
:cuefn        - Cuing function, defaults to BAR
:shuffle      - Shuffle function, defaults to NO-SHUFFLE
:shift        - float, offset added to all events times, default 0.0
:tempo        - float, defaults to section value.
:unit         - symbol, time-signature unit, defaults to section value.
:bars         - int, bar count, defaults to section value.
:beats        - int, beats per bar, defaults to section value.
:subbeats     - int, subbeats per beat, defaults to section value.
:render-once  - bool, if true do not repeat
:transposable - bool, if true allow transpose and invert operations
:reversible   - bool, if true allow retrograde operations.
:chord-model  - defaults to section value
:remarks      - optional remarks text
:events       - event specification list,  see below.


Events are specified as a nested lists of event 'clauses'

   ((clause-1 clause-2 ...)
    (clause-x clause-y ...)
    .......................)

   where a clause is a keyword followed by 0 to 2 arguments.

       :command [arguments-1 argument-2]

   The argument count is dependent on the specific keyword.


Commands fall into two general classes; 

   1) Those that set the 'environment' include the following.
      :reset :time :chord :inversion :octave :dur :amp
       
      The values set by these commands remain in effect until 
      explicitly changed.

   2) Those that generate events, including
      :key :pressure :cc :bend :program :bank

      These commands generate actual events.  Once the events have been 
      generated the commands are cleared.


:reset     - No arguments, sets all values to default.

:time      - :time time-specification
             Sets the event time.  The format for time-specification must
             match whatever the parts cue function is expecting.  For the 
             default BAR function, the format is a list 
             (bar-number beat-number sub-beat-number)

:chord     - :chord chord-name or :chord template-list
             Chords may be specified either by name or as a list of
             keynumber offsets.   If a name is used it must be recognized
             by the parts chord-model.   

             Chords specified by list are interpreted differently by absolute
             and non-absolute chord-models.

             For non-absolute models the list is treated as a template of 
             keynumber offsets.  (0 3 7) produces a three note minor chord.  
             For a keynumber of 60 the resulting notes are 60, 63 and 67. 

             For absolute-chord models the template is used to directly 
             specify the final keynumbers, (60 63 67) will produce a 
             c-minor chord  no matter what the actual keynumber is.  

             The ABSOLUTE-CHORDS-P predicate may be used to determine if a 
             chord-model is absolute or not.  +CHORD-TABLE+ is the default 
             chord-model and is not absolute.
             
:inversion - :inversion degree    -12 <= degree <= +12
             Sets the chord inversion by rotating notes in the chord template 
             list.  For chord (0 3 7)

             degree -1   --> (7 0 3)
             degree  0   --> (0 3 7)
             degree +1   --> (3 7 0)

             Inversion has no audible effect unless paired with an octave 
             value.  Note, for STRUMMER parts inversion does make a 
             difference with or without an octave value.
             
:octave    - :octave n    -3 <= n <= +3
             Inserts a note shifted n octaves from the first note in the 
             chord template.  For chord (0 3 7)

             octave -1 --> (0 3 7 -12) 
             octave  0 --> (0 3 7)
             octave +1 --> (0 3 7 12)

             The octave is applied after any inversion, for an inversion.
             original (0 3 7)
             inversion 1 --> (3 7 0)
             octave 1    --> (3 7 0 15)

:dur       - :dur metric-expression   or  :dur float
             Sets note duration by metric-expression or by floating point 
             number. If a metric-expression is used the duration is scaled 
             by the tempo.  When a number is specified it is absolute time
             in seconds and independent of the tempo.  
             The duration is processed independently by each instruments
             articulation-map.

:amp       - :amp dynamic-value
             The amplitude is processed independently by each instruments
             dynamic-map.

:key      - :key keynumber
            Generates note on/off events using keynumber and the current 
            chord.  The keynumber is processed independently by each 
            instruments keynumber-map.

:pressure - :pressure value   0.0 <= value <= 1.0
            Generates MIDI channel-pressure event on each instrument
            channel.

:cc       - :cc controller-number value   
            0 <= controller-number <= 127  0.0 <= value <= 1.0
            Generates MIDI control-change event on each instruments
            channel

:bend     - :bend value   -1.0 <= value +1.0
            Generates pitch-bend event on each instruments channel.

:program  - :program program-number
            Generates program-change event for each instrument.
            The program-number is processed by the instruments 
            program-change-map

:bank     - :bank bank-number program-number
            Generates bank and program change events for each instrument.")


(constant +simple-part-docstring+
       (sformat "~A~%~%~A"
		"The SIMPLE-PART macro has the same usage as the MAKE-SIMPLE-PART
function except it binds the new part to the symbol name."
		+make-simple-part-docstring+))
