;;;; CYCO parts controllers-docs.lisp
;;;;
;;;; Docstrings for CONTROLLERS.
;;;;

(in-package :cyco-part)

(constant +controllers-docstring+
	  "Creates new instance of CONTROLLERS part.

A CONTROLLERS is a part for generating MIDI control-change events.
Individual or curve events may be generated.

name         - Symbol
instruments  - Instrument or list of instruments.
:section     - Parent section, defaults to current section of *PROJECT*.
:cuefn       - Cueing function, defaults to section value.
:shuffle     - Shuffle function, defaults to section value.
:shift       - Metric-expression, defaults to section value.
:tempo       - float, tempo in BPM, defaults to section value.
:unit        - symbol, time-signature beat-unit, defaults to section value.
:bars        - Integer, bars per phrase, defaults to section value.
:beats       - Integer, beats per bar, defaults to section value.
:subbeats    - Integer, sub-beats per beat, defaults to section value.
:render-once - Boolean, if true generate events once, default nil.
:thin        - Boolean, if true remove redundent controller events, default t.
:no-thin     - Boolean, if true do not remove redundent controller events, default t.
:remarks     - Optional remarks text
:events      - Event specification list, see below.

Events are specified as a nested list of event 'clauses'.

    ((clause-1 clause-2 ...)
     (clause-x clause-y ...)
      ......................)

    where a clause is a keyword followed by a prescribed number of
    arguments.

        :command arguments...

Commands fall into two general categories:

    1) Those that set the 'environment' include the following
       :reset :time :value :ctrl :cycles :phase & :width

       The values set by these commands remain in effect until explicitly
       changed. 

   2) Those that generate events include
      :cc :ramp :saw :tri & :pulse

      Once the event(s) have been generated these commands are cleared.


:reset  - Sets all values to defaults

:time   - :TIME start end interval

          start and end are the initial and final times over which the
          events occur.  The format must be accepted by the cue function.

          interval is a metric-expression that sets the rate at which
          events occur.

          The special value '* reuses the current value.
          Additionally the start time may have the value '< which set it
          to the current end time.


:value  - :VALUE start end

          The initial and final controller values.   Both are integers
          0 <= value < 128.   The special value '* indicates to use the
          current value.
          
:ctrl   - :CTRL controller-number
          
          The controller number either as an integer 0 <= number < 128
          or as a symbolic name.  Use (?CONTROLLERS) for list of defined
          controllers.

:cycles - Number of cycles, default 1.
:phase  - Pattern phase shift in degrees, default 0.
:width  - Pulse curve width, 0.0 <= width <= 1.0, default 0.5

:cc     - :CC time controller value

          Generates a single control-change event for each instrument.
          time - must be in format accepted by the cue function.
          controller - same as :ctrl above.
          value - same as :value above.

:ramp  - :RAMP
         
         Generates linear ramp for each instrument.

:saw   - :SAW
           
         Generates sawtooth pattern for each instrument.
         Uses cycles and phase values.

:tri   - :TRI

        Generates triangle pattern for each instrument.
        Uses cycles and phase values.

:pulse - :PULSE

       Generates pulse pattern for each instrument.
       Uses cycles, phase and width values.

All values effecting the :RAMP, :SAW, :TRI and :PULSE clauses must be set
at the time these commands are specified.


Examples:

Generate single mod wheel event at time (1 1 1) with value 64.


         :cc (1 1 1) wheel 64


Generate ramp between times (1 1 1) and (2 1 1) every 16th note
with initial value 0 and final value 128

        :time (1 1 1)(2 1 1) s :value 0 128 :ctrl wheel :ramp

Generate another ramp starting at the end-time of the previous one

        :time < (3 1 1) s :ramp

        ;; Note the value 0 128 and ctrl clauses are reused.

Generate single-cycle triangle curve

       :time (1 1 1)(2 1 1) s :value 0 128 :cycle 1 :phase 0 :tri

Generate 2 pulse cycles with 33% pulse-width

      :time (1 1 1)(2 1 1) s :value 0 128 :cycle 2 :phase 0 :width .33 :pulse")
