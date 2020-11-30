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
:section    - Parent section, defaults to current section of *PROJECT*
:cuefn      - Time cue function, defaults to section value.
:shift      - Metric-expression, defaults to 0.0
:tempo      - Defaults to parent section value.  
:unit       - Defaults to parent section value.
:bars       - Defaults to parent section value.
:beats      - Defaults to parent section value.
:subbeats   - Defaults to parent section value.
:start      - Start time, must be in a format accepted by cuefun.
:end        - End time, must be in a format accepted by cuefn,
:increment  - Metric-expression
:pattern    - Value pattern, defaults to (line :of 0)
:remarks    - Optional remarks text
:render-once     - Boolean, if true do not repeat, default false.
:reset-on-repeat - Boolean, if true reset pattern on repeat, default t.

Values must be provided for :START :END and :INCREMENT")

