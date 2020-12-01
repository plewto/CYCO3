;;;; CYCO cball docs
;;;;
;;;; A CBALL is a PART for generating MIDI controller, channel-pressure and
;;;; bend events.


(in-package :cyco-part)

(constant +cball-docstring+
	  "Creates new instance of CBALL.
name        - symbol, part name
controller  - MIDI controller number (0..127) inclusive, :PRESSURE or :BEND
instruments - Instrument or list of instruments.
start       - Start time, must be in a format accepted by cuefun.
end         - End time, must be in a format accepted by cuefn.
:section    - Parent section, defaults to current section of *PROJECT*
:cuefn      - Time cue function, defaults to section value.
:shift      - Metric-expression, defaults to 0.0
:tempo      - Defaults to parent section value.  
:unit       - Defaults to parent section value.
:bars       - Defaults to parent section value.
:beats      - Defaults to parent section value.
:subbeats   - Defaults to parent section value.
:increment  - Metric-expression, default 's
:pattern    - Value pattern, defaults to nil
:initial    - cons (value shift), default nil
:final      - cons (value shift), default nil
:remarks    - Optional remarks text
:render-once     - Boolean, if true do not repeat, default false.
:reset-on-repeat - Boolean, if true reset pattern on repeat, default t.

The pattern argument specifies the controller values to be generated and must 
either be nil or a PATTERN which produces numbers only.   All data values
are 'normalized'  0.0 <= value <= 1.0  for controller and pressure and
-1.0 <= value <= +1.0 for bend.  Out of range values are clipped.  

The RAMP, SAWTOOTH, TRIANGLE and PULSE patterns are particularly useful.

If pattern is nil then no events are generated.

The initial and final arguments specify single events to be generated
prior to the start time and after the end time, respectively.  Both
must either be a cons (value shift) or nil.

:initial (cons value shift) generates an event of the given value
shift-seconds before the start time.

:final (cons value shift) generates an event of the given value
shift-seconds after the end time.")
