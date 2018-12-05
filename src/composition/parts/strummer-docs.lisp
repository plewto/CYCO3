
(constant +strummer-documentation+
"Creates new instance of STRUMMER, a part type with explicit event
specifications.

name          - Symbol, part name
instruments   - Instrument, single instrument only.
:section      - Parent section, defaults to current-section of *project*
:cuefn        - Cueing-function, default to parent value.
:shift        - Offset time for initial event.  The format of shift is
                dependent on cuefn.  When rendering an Strummer within a
                section, the first iteration is offset by shift amount.
                Defaults to 0.0.
:render-once  - Flag, if true, render-n only renders strummer events one time. 
:transposable - Flag, if true this part is subject to transpose and invert.
:reversible   - Flag, if true this part is subject to retrograde operations.
:chord-model  - Establish chord-model, defaults to chord-model of 
                parent section.
:remarks      - String, remarks text.
:events       - See below
:tempo        - Time signature tempo, defaults to parent's value.
:unit         - Time signature unit, defaults to parent's value.
:bars         - Time signature bars, defaults to parent's value.
:beats        - Time signature beats, defaults to parent's value.
:subbeats     - Time signature subbeats, defaults to parent's value. 

 A Strummer may generate note events and has a rich set of options for
 strumming chords.   They may also generate simple touch, MIDI controller,
 bend and program events.
 
 Events are specified as a nested list of event commands.
 
     ((:command n :command n ...)
      (:command n :command n ...)
      ...........................)
 
 Where each clause within a single event sub-list has the form
 
      :command [arguments..]
 
 and :command is one of the keywords below. Argument counts are between 
 0 and 3, depending on the specific command.
 
    explicit MIDI program numbers.	  
")
