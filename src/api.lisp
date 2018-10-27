;;;; CYCO3 api
;;;;


;; patterns/pattern-generics.lisp:
(defgeneric ->cycle (obj)
  (:documentation
   "Coerce object to Cycle pattern.
If object is an instance of Cycle, return it.
If object is some non-cycle Pattern, return a new Cycle with the elements of object.
If object is a list, return new Cycle :of object.
For all other types, coerce object to a list and return new Cycle."))

;; generics.lisp:
(defgeneric ->list (obj)
  (:documentation
   "Coerce object to a list.
If object is a list, return it.
If object is some other sequence type, return new list of object's elements.
For all other types, create new list with object as it's only element.
see ->vector"))

;; patterns/pattern-generics.lisp:
(defgeneric ->pattern (obj &key ptype)
  (:documentation
   "Coerce object to a Pattern of type ptype.
If object is a ptype Pattern, return it.
If object is a non-ptype Pattern, return new ptype Pattern with elements taken
from obj.
For all other types return (ptype :of obj)
The default ptype is Cycle."))


;; generics.lisp:
(defgeneric ->smf (contents &key filename offset repeat pad)
  (:documentation
   "Creates new MIDI file from contents and write to file.
:filename - Optional String, if not specified the filename is derived from
            the name of contents and the current project.
:offset   - Optional, initial offset time in seconds, default 0.
:repeat   - Optional int, number of times to repeat contents, default 1.
:pad      - Optional float, number of seconds to add to end of file, default 2."))

;; generics.lisp:
(defgeneric ->string (obj)
  (:documentation
   "Returns string representation of object."))

;; generics.lisp:
(defgeneric ->symbol (arg)
  (:documentation
   "Intern new symbol using name of argument."))

;; generics.lisp:
(defgeneric ->vector (obj)
  (:documentation
   "Coerce object to vector.
If object is a vector, return it.
If object is a non-vector sequence, return new vector using elements of object.
For all other cases return new vector with obj as it's only element.
See ->list"))

;; generics.lisp:
(defgeneric ? (obj)
  (:documentation
   "Generalized help/inspection function."))

;; composition/project.lisp:
(defgeneric add-group (section group)
  (:documentation
   "Adds group to section."))

;; generics.lisp:
(defgeneric alist-p (obj)
  (:documentation
   "Predicate true if argument is an association list."))

;; orchestra/instrument.lisp:
(defgeneric articulation-map (inst)
  (:documentation
   "Returns instrument's articulation-map."))

;; orchestra/instrument.lisp:
(defgeneric articulation-map! (inst mapfn)
  (:documentation
   "Sets instrument's articulation-map"))

;; patterns/pattern-generics.lisp:
(defgeneric bag-p (obj)
  (:documentation
   "Predicate, true if argument is an instance of Bag."))

;; composition/time-signature.lisp:
(defgeneric bar-duration (obj)
  (:documentation
   "Returns duration of single bar from time-signature."))

;; composition/time-signature.lisp:
(defgeneric bars (obj)
  (:documentation
   "Returns time-signature bar count."))

;; composition/time-signature.lisp:
(defgeneric bars! (obj value)
  (:documentation
   "Sets time-signature bar count."))

;; composition/time-signature.lisp:
(defgeneric beat-duration (obj)
  (:documentation
   "Returns time-signature beat duration."))

;; composition/time-signature.lisp:
(defgeneric beats (obj)
  (:documentation
   "Returns time-signature beat count."))

;; composition/time-signature.lisp:
(defgeneric beats! (obj value)
  (:documentation
   "Sets time-signature beat count."))

;; generics.lisp:
(defgeneric butfinal (obj)
  (:documentation
   "Returns all but final element of sequence.
(butfinal '(a b c)) --> (a b)"))

;; patterns/pattern-generics.lisp:
(defgeneric cardinality (obj)
  (:documentation
   "Returns number of elements in a Pattern."))

;; generics.lisp:
(defgeneric channel (obj &optional resolve)
  (:documentation
   "Returns MIDI channel of object.
MIDI channels may be symbolic and one symbolic channel may map 
to another symbol.  I.E. channel 'snare may map to channel 'drum.
If the optional resolve argument is true the result is the actual 
MIDI channel, an integer between 1 and 16 inclusive."))

;; orchestra/instrument.lisp:
(defgeneric channel! (inst channel)
  (:documentation
   "Sets instrument's MIDI channel.  
channel may be a symbolic meta-channel or an integer between 1 and 16 
inclusive."))

;; generics.lisp:
(defgeneric channel-index (obj)
  (:documentation
   "Returns MIDI channel-index of object.
The channel-index is the lower 4-bits of a MIDI channel message status byte
and is always in the range 0..15 inclusive.  It is 1 less then the MIDI 
channel.   User facing functions should generally use MIDI channel instead 
of the channel index."))

;; node.lisp:
(defgeneric child-of-p (parent child)
  (:documentation
   "Predicate, true if child is a direct child of parent.
See cyco-node."))

;; chords/chord-model.lisp:
(defgeneric chord-model-p (obj)
  (:documentation
   "Predicate, true if object is an instance of chord-model."))

;; chords/chord-model.lisp:
(defgeneric chord-template (model name)
  (:documentation
   "Returns list of key-number offsets for named chord in model.
A chord template is defined as a list. For example a major triad has the 
template (0 4 7)."))

;; chords/chord-model.lisp:
(defgeneric chord-types (model)
  (:documentation
   "Returns list of chord types defined by chord-model."))

;; generics.lisp:
(defgeneric clone (source &key new-name new-parent)
  (:documentation
   "Creates copy of source.
Clone is defined for all types. However, for types where a specific clone
method has not been defined, it returns the source argument.


:new-name   - Format string, inclusion of the format ~A directive inserts 
              the source name into the result.  For object types which do not
              have a name, new-name is ignored.
:new-parent - By default the cloned object has the same parent as the source,
              new-parent explicitly sets the parent of the result.  For objects
              which do not have a parent/child relationship, new-parent is 
              ignored."))

;; generics.lisp:
(defgeneric cnth (n seq)
  (:documentation
   "Returns nth element of sequence in a circular manner.
(cnth 2 '(a b c)) --> C
(cnth 3 '(a b c)) --> A
(cnth 4 '(a b c)) --> B"))

;; node.lisp:
(defgeneric connect (parent child)
  (:documentation
   "Make parent node the parent of child.
If child has an existing parent, that connection is severed."))

;; patterns/pattern-generics.lisp:
(defgeneric cycle-p (obj)
  (:documentation
   "Predicate, true if object is an instance of Cycle."))

;; midi/midi.lisp:
(defgeneric data (obj index)
  (:documentation
   "Returns the indexed value from object."))

;; midi/midi.lisp:
(defgeneric data-count (obj)
  (:documentation
   "Returns number of data values in object."))

;; chords/chord-model.lisp:
(defgeneric define-chord (model name template &optional description)
  (:documentation
   "Defines a new chord-mode chord.
mode - An instance of chord-model.   
name - Symbol
template - List of key-number offsets.
description - Optional text."))

;; chords/chord-model.lisp:
(defgeneric defines-chord-p (model name)
  (:documentation
   "Predicate, true if chord-model defines the named chord."))

;; patterns/pattern-generics.lisp:
(defgeneric dice-p (obj)
  (:documentation
   "Predicate, true if object is an instance of Dice."))

;; node.lisp:
(defgeneric disconnect (child)
  (:documentation
   "Severs connection between child and it's parent, if any. 
After a disconnect the child becomes a root node."))

;; chords/chord-model.lisp:
(defgeneric dump-chords (model)
  (:documentation
   "Displays list of chords defined by chord-model."))

;; generics.lisp:
(defgeneric dump-events (obj &key range filter render)
  (:documentation
   "Display listing of MIDI events.
obj - Source of event
:range - time range (start end)
:filter - function, predicate on events.  If the result it true do not 
display the event.
;render - If true display actual MIDI bytes."))

;; midi/midi.lisp:
(defgeneric dump-smf-track-chunk (trk pad)
  (:documentation
   "Diagnostic displays contents of MIDI file track."))

;; generics.lisp:
(defgeneric duration (obj)
  (:documentation
   "Returns duration of time-signature phrase."))

;; generics.lisp:
(defgeneric dynamic (obj)
  (:documentation
   "Returns dynamic value of object or list of objects."))

;; generics.lisp:
(defgeneric dynamic->velocity (dy)
  (:documentation
   "Convert dynamic value to MIDI velocity."))

;; orchestra/instrument.lisp:
(defgeneric dynamic-map (inst)
  (:documentation
   "Returns instruments dynamic-map function."))

;; orchestra/instrument.lisp:
(defgeneric dynamic-map! (inst mapfn)
  (:documentation
   "Sets instrument's dynamic-map function."))

;; generics.lisp:
(defgeneric dynamic-name (obj)
  (:documentation
   "Returns symbolic name of dynamic value.
Note this function is generally inefficient and there may be slight 
rounding errors."))

;; generics.lisp:
(defgeneric dynamic-p (obj)
  (:documentation ""))

;; generics.lisp:
(defgeneric final (obj)
  (:documentation
   "Predicate, true if object can be interpreted as a dynamic value."))

;; node.lisp:
(defgeneric find-child (parent child)
  (:documentation
   "Find indicated child of parent.
Returns nil if no such child exists."))

;; composition/project.lisp:;;*
(defgeneric group-p (obj)
  (:documentation
   "Predicate, true if object is an instance of Group."))

;; node.lisp:
(defgeneric hah-property-p (node key)
  (:documentation
   "Predicate, true if node has indicated property."))

;; composition/project.lisp:
(defgeneric has-group-p (section group-name)
  (:documentation
   "Predicate, true if section contains named group."))

;; composition/time-signature.lisp:
(defgeneric init-time-signature (obj)
  (:documentation
   "Initialize time-signature."))

;; patterns/instrument-layer.lisp:
(defgeneric instrument-layer-p (obj)
  (:documentation
   "Predicate, true if object is an instance of Instrument-layer."))

;; orchestra/instrument.lisp:
(defgeneric instrument-p (obj)
  (:documentation
   "Predicate, true if object is an instance of Instrument."))

;; generics.lisp:
(defgeneric invert (obj pivot)
  (:documentation
   "Apply key-number inversion to object.
Inversion may be applied to any object without producing an error.
If the object has the property :transposable with a nil value, then no 
inversion is applied. 

The pivot point is a key-number around which the inversion takes place.
All negative values have an inversion of +REST+ = -1.

(invert '(-1234 59 60 70) 60) --> (-1 61 60 50)"))

;; generics.lisp:
(defgeneric keyname (obj)
  (:documentation
   "Returns symbolic name for key-number."))

;; generics.lisp:
(defgeneric keynumber (obj)
  (:documentation
   "Converts object, or list of objects, to MIDI key-numbers."))

;; orchestra/instrument.lisp:
(defgeneric keynumber-map (inst)
  (:documentation
   "Returns instrument's keynumber-map function."))

;; orchestra/instrument.lisp:
(defgeneric keynumber-map! (inst mapfn)
  (:documentation
   "Sets instrument's keynumber-map function."))

;; generics.lisp:
(defgeneric keynumber-p (obj)
  (:documentation
   "Predicate, true if object can be interpreted as a key-number."))

;; patterns/pattern-generics.lisp:
(defgeneric line-p (obj)
  (:documentation
   "Predicate, true if object is an instance of Line."))

;; node.lisp:
(defgeneric local-properties (obj)
  (:documentation
   "Returns list of object's local-property names.  
A local property is a property defined directly by an object.
Properties defined by an object's parent are not considered local."))

;; orchestra/channel-assignments.lisp:
(defgeneric meta-channel (name  &optional resolve)
  (:documentation
   "Returns value of named MIDI channel.  A symbolic meta-channel may
map to another symbolic channel.  If optional resolve argument is true
then the actual MIDI channel, as an integer between 1 and 16 inclusive, 
is returned."))

;; generics.lisp:
(defgeneric metric (obj)
  (:documentation
   "Converts object, or list of objects, to float metric values."))

;; generics.lisp:
(defgeneric metric-expression (exp)
  (:documentation
   "Evaluates metric-expression and return it's value as a float."))

;; generics.lisp:
(defgeneric metric-expression-p (exp)
  (:documentation
   "Returns non-nil if exp may be interpreted as a metric-expression.
Parsing metric-expressions may be slow and exp is evaluated to determine
if it is a valid expression.  If exp is invalid the result is nil.  If
exp is a valid metric-expression, the result is its float value."))

;; generics.lisp:
(defgeneric metric-p (obj)
  (:documentation
   "Predicate, true if argument may be interpreted as a basic metric value."))

;; midi/midi.lisp:
(defgeneric midi-channel-message-p (obj)
  (:documentation
   "Predicate, true if object is a MIDI channel message."))

;; midi/midi.lisp:
(defgeneric midi-channel-pressure-p (obj)
  (:documentation
   "Predicate, true if object is a MIDI channel pressure message."))

;; midi/midi.lisp:
(defgeneric midi-control-chnage-p (obj)
  (:documentation
   "Predicate, true if object is a MIDI control change message."))

;; midi/midi.lisp:
(defgeneric midi-end-of-track-p (obj)
  (:documentation
   "Predicate, true if object is a MIDI end of track message."))

;; midi/midi.lisp:
(defgeneric midi-end-system-exclusive-p (obj)
  (:documentation
   "Predicate, true if object is a MIDI end of system-exclusive message."))

;; midi/midi.lisp:
(defgeneric midi-key-message-p (obj)
  (:documentation
   "Predicate, true if object is a keyed MIDI message.  Keyed messages 
include: note-off, note-on and poly-pressure."))

;; midi/midi.lisp:
(defgeneric midi-key-signature-p (obj)
  (:documentation
   "Predicate, true if object is a MIDI key-signature message."))

;; midi/midi.lisp:
(defgeneric midi-message-p (obj)
  (:documentation
   "Predicate, true if object is any type of MIDI message."))

;; midi/midi.lisp:
(defgeneric midi-meta-copyright-p (obj)
  (:documentation 
"Predicate, true if object is a midi-meta-copyright message."))

;; midi/midi.lisp:
(defgeneric midi-meta-cue-p (obj)
  (:documentation 
"Predicate, true if object is a midi-meta-cue message."))

;; midi/midi.lisp:
(defgeneric midi-meta-instrument-name-p (obj)
  (:documentation 
"Predicate, true if object is a midi-meta-instrument-name message."))

;; midi/midi.lisp:
(defgeneric midi-meta-lyric-p (obj)
  (:documentation 
"Predicate, true if object is a midi-meta-lyric message."))

;; midi/midi.lisp:
(defgeneric midi-meta-marker-p (obj)
  (:documentation 
"Predicate, true if object is a midi-meta-marker message."))

;; midi/midi.lisp:
(defgeneric midi-meta-message-p (obj)
  (:documentation 
"Predicate, true if object is any type of MIDI meta message."))

;; midi/midi.lisp:
(defgeneric midi-meta-text-p (obj)
  (:documentation 
"Predicate, true if object is any type of MIDI meta text message."))

;; midi/midi.lisp:
(defgeneric midi-meta-track-name-p (obj)
  (:documentation 
"Predicate, true if object is a midi-meta-track-name message."))

;; midi/midi.lisp:
(defgeneric midi-note-off-p (obj)
  (:documentation 
"Predicate, true if object is a midi-note-off message."))

;; midi/midi.lisp:
(defgeneric midi-note-on-p (obj)
  (:documentation 
"Predicate, true if object is a midi-note-on message."))

;; midi/midi.lisp:
(defgeneric midi-pitch-bend-p (obj)
  (:documentation 
"Predicate, true if object is a midi-pitch-bend message."))

;; midi/midi.lisp:
(defgeneric midi-poly-pressure-p (obj)
  (:documentation 
"Predicate, true if object is a midi-poly-pressure message."))

;; midi/midi.lisp:
(defgeneric midi-program-change-p (obj)
  (:documentation 
"Predicate, true if object is a midi-program-change message."))

;; midi/midi.lisp:
(defgeneric midi-system-common-message-p (obj)
  (:documentation 
"Predicate, true if object is a midi-system-common-message message."))

;; midi/midi.lisp:
(defgeneric midi-system-exclusive-p (obj)
  (:documentation 
"Predicate, true if object is a midi-system-exclusive message."))

;; midi/midi.lisp:
(defgeneric midi-tempo-message-p (obj)
  (:documentation 
"Predicate, true if object is a midi-tempo-message message."))

;; midi/midi.lisp:
(defgeneric midi-time-signature-p (obj)
  (:documentation 
"Predicate, true if object is a midi-time-signature message."))

;; midi/midi.lisp:
(defgeneric mnemonic (obj)
  (:documentation
   "Returns short mnemonic string for object 
Mnemonic is primarily used for MIDI message type abbreviations."))

;; composition/project.lisp:
(defgeneric mute (obj &optional state)  ;;* state one of :mute :unmute :solo or nil
  (:documentation
   "Sets mute state of object.
Possible states are :mute :unmute :solo and nil
For solo this object is switched to non-muted while all sibling objects
are muted.  A nil value leaves the current state as is."))

;; composition/project.lisp:
(defgeneric mute-all (obj)
  (:documentation
   "Sets all child objects to muted."))

;; composition/project.lisp:
(defgeneric muted-p (obj)
  (:documentation
   "Predicate, true if object is muted."))

;; generics.lisp:
(defgeneric name (obj)
  (:documentation
   "Returns object's name.  Name may be applied to any object without error.
The default name is an object's string representation."))

;; generics.lisp:
(defgeneric name! (obj new-name)
  (:documentation
   "Sets name of object."))

;; patterns/pattern-generics.lisp:
(defgeneric next (obj &optional n)
  (:documentation
   "Returns next value(s) from Pattern or pattern-like object.
By default a single value is returned (see next-1).
If n is a positive integer, returns list of the next n values.
If n is the keyword :rest, returns list of all remaining values
If n is the keyword :all, returns list of all elements.
See next-1 and next-n."))

;; patterns/pattern-generics.lisp:
(defgeneric next-1 (obj)
  (:documentation
   "Returns the next value from a Pattern or pattern-like object."))

;; patterns/pattern-generics.lisp:
(defgeneric next-n (obj n)
  (:documentation
   "Returns a list of the next n values from a Pattern or pattern-like 
object."))

;; orchestra/instrument.lisp:
(defgeneric note-events (inst time keynum duration dynamic &key time-scale)
  (:documentation
   "note-events is a low-level method to convert time, key, duration and 
dynamics to MIDI events using a specific instrument.  The instrument's 
keynumber, articulation and dynamic maps are applied to the result.
The result is a two-element list 
    ((time . note-on)
     (time+duration . note-off))"))

;; generics.lisp:
(defgeneric octave (obj)
  (:documentation
   "Returns octave number of key number or list of key numbers.
All negative key numbers have octave -1."))

;; generics.lisp:
(defgeneric palindrome (obj &key elide)
  (:documentation
   "Creates palindrome from source sequence.
The :elide argument determine how end-values are treated.
(palindrome '(A B C D))  --> (A B C D D C B A)
(palindrome '(A B C D) :elide :last)  --> (A B C D C B A)
(palindrome '(A B C D) :elide :first) --> (A B C D D C B)
(palindrome '(A B C D) :elide :both)  --> (A B C D C B)"))

;; generics.lisp:
(defgeneric parent (obj)
  (:documentation
   "Returns object's parent.   If object is a root node, returns nil."))

;; composition/project.lisp
(defgeneric part-p (obj)
  (:documentation
   "Predicate, true if object is an instance of any type of Part."))


;; node.lisp:
(defgeneric path-to-root (obj)
  (:documentation
   "Returns list of all nodes between this object and the root node (inclusive)."))

;; patterns/pattern-generics.lisp:
(defgeneric pattern-p (obj)
  (:documentation
   "Predicate, true if object is any type of Pattern."))

;; generics.lisp:
(defgeneric permute (obj)
  (:documentation
   "Returns permutation of sequence."))

;; composition/time-signature.lisp:
(defgeneric phrase-duration (obj)
  (:documentation
   "Returns time-signature phrase-duration."))

;; generics.lisp:
(defgeneric pick (obj)
  (:documentation
   "Select an object at random from collection."))

;; generics.lisp:
(defgeneric pitch-class (obj)
  (:documentation
   "Returns the pitch-class, an integer between 0 and 11 inclusive, 
of a key-number or list of key numbers.  All negative key-numbers
have a pitch class of -1."))

;; node.lisp:
(defgeneric print-tree (node &optional depth)
  (:documentation
   "Print hierarchal tree starting at node.  
The depth argument is used internally."))

;; generics.lisp:
(defgeneric priority (obj)
  (:documentation
   "Returns the priority of a MIDI message.  If two MIDI events have the 
same time stamp, the event-message with the lowest priority appears first in 
the events list.  There are primarily two applications.  First it ensures 
certain meta events appear in the correct location.  I.E. end-of-track always
appears after all other events.  Secondly, note-off events always appear after 
note on-events to prevent stuck notes."))

;; generics.lisp:
(defgeneric program-bank (obj)
  (:documentation
   "Returns an instruments program-bank.
There is no pre-defined format for a program-bank, it is up to the specific
instrument.   

NOTE: This is not a 'bank' of programs, rather it is the name of a bank on some 
external MIDI device."))

;; orchestra/instrument.lisp:
(defgeneric program-bank! (inst pbank)
  (:documentation
   "Sets instruments program bank."))

;; orchestra/instrument.lisp:
(defgeneric program-change-events (inst time &key bank program)
  (:documentation
   "Generates list of MIDI events to effect an instrument's program change.
The result format is dependent on the specific external MIDI instrument.
At it's simplest the result is a single program-change event.  
More complex results may include control changes for bank selection.  
The result may also include note events for 'key-switched' instruments."))

;; generics.lisp:
(defgeneric program-map (obj)
  (:documentation
   "Returns an instrument's program-map function."))

;; generics.lisp:
(defgeneric program-map! (obj fn)
  (:documentation
   "Sets instrument's program-map function."))

;; generics.lisp:
(defgeneric program-number (obj)
  (:documentation
   "Returns instrument's default program-number.  There is no predefined 
format, though integers between 0 and 127 are generally treated as MIDI 
program numbers.  The exact format is dictated by the program-map 
function."))

;; orchestra/instrument.lisp:
(defgeneric program-number! (inst pnumber)
  (:documentation
   "Returns instrument's program-number."))

;; generics.lisp:
(defgeneric project-p (obj)
  (:documentation 
   "Predicate, true if object is an instance of Project."))

;; node.lisp:
(defgeneric properties (obj &optional acc)
  (:documentation
   "Returns list of all property keys defined by object."))

;; node.lisp:
(defgeneric property (obj key)
  (:documentation
   "Returns object's property value assigned to key.
It is an error if the object does not define key as a property."))

;; node.lisp:
(defgeneric property* (obj key) 
  (:documentation
   "Private function for internal use."))

;; node.lisp:
(defgeneric prune (node &optional force)
  (:documentation
   "Starting at node recursively disconnect all child nodes for which the 
:TRANSIENT property is true.  If force is true, disconnect all nodes,
ignoring their transient value."))


;; midi/smf-track.lisp:
(defgeneric push-event (time message obj)
  (:documentation
   "Push a new MIDI event to object. 
time - float
message - MIDI message
obj - destination."))

;; node.lisp:
(defgeneric put (obj key value)
  (:documentation
   "Assign's value to an object,s property.
obj - the object
key - symbol, property name.  It is an error if the object does not 
define key as a property.
value - the new value."))


;; patterns/pattern-generics.lisp:
(defgeneric remaining (obj)
  (:documentation
   "Returns list of the remaining values yet to be returned by a Pattern
or pattern-like object."))

;; generics.lisp:
(defgeneric remarks (obj)
  (:documentation
   "Returns object's remarks text."))

;; generics.lisp:
(defgeneric remarks! (obj text)
  (:documentation
   "Sets object's remarks text."))

;; midi/midi.lisp:
(defgeneric render-midi-message (obj)
  (:documentation
   "Convert MIDI-XXX-message object to list of MIDI bytes."))

;; generics.lisp:
(defgeneric render-n (obj count &key offset)
  (:documentation
   "Convert object to list of MIDI events.
If count is greater then 1, place each additional copy shifted by the 
duration of the object.  

:offsets shifts all events by offset seconds, default 0."))

;; generics.lisp:
(defgeneric render-once (obj &key offset)
  (:documentation
   "Convert object to a list of MIDI events.
:offset shifts all events by offset seconds, default 0."))

;; midi/midi.lisp:
(defgeneric render-smf (obj &key pad)
  (:documentation
   "Converts Standard MIDI file object to list of MIDI events.
:pad adds pad seconds at end of events."))

;; midi/midi.lisp:
(defgeneric render-smf-header (obj track-count)
  (:documentation
   "Low-level method, renders MIDI file header."))

;; midi/midi.lisp:
(defgeneric render-smf-track (trk pad)
  (:documentation
   "Low-level method, renders MIDI file track."))

;; generics.lisp:
(defgeneric reset (obj)
  (:documentation
   "Restore internal state of object to some initial condition.
See also soft-reset."))
   

;; generics.lisp:
(defgeneric rest-p (obj)
  (:documentation
   "Predicate, true if object is interpreted as a rest.
Generally any negative number, and the symbol 'r are treated as rest."))

;; generics.lisp:
(defgeneric retrograde (obj)
  (:documentation
   "Reverse object's elements. 
If the object has the :RESERVABLE property with a value of nil, do not 
apply.   Retrograde may be called with any object without producing an
error."))

;; node.lisp:
(defgeneric root-p (obj)
  (:documentation
   "Predicate, true if object is a root node."))

;; generics.lisp:
(defgeneric section-p (obj)
  (:documentation
   "Predicate, true if object is an instance of Section or one of it's
subclasses."))

;; composition/project.lisp:
(defgeneric seq-order (sections &key project)
  (:documentation
   "Sets the sequence order of Sections within a project."))

;; generics.lisp:
(defgeneric slice (seq start &optional end)
  (:documentation
   "Returns a slice of a sequence.
seq - 
start - Staring index 
end   - Ending index, may be negative to index from end, defaults to end
of sequence."))

;; midi/midi.lisp:
(defgeneric smf-track (obj &optional index)
  (:documentation
   "Low-level method, returns indexed track from MIDI file.
NOTE: At least for now CYCO only supports single-track MIDI files."))

;; midi/midi.lisp:
(defgeneric smf-track! (obj trk &optional index)
  (:documentation
   "Low-level method to set MIDI file track.
NOTE: At least for now CYCO only supports single-track MIDI files."))

;; midi/midi.lisp:
(defgeneric smf-track-count (obj)
  (:documentation
   "Returns number of tracks in MIDI file."))

;; composition/parts/epart-state.lisp:
(defgeneric soft-reset (obj)
  (:documentation
   "Performs a soft-reset on object.   A soft reset initializes some of an 
objects parameters but not all of them.  This is in contrast to a normal 
reset which initializes all values.  The exact behavior is dependent on the 
specific object type."))

;; composition/project.lisp:
(defgeneric solo (obj)
  (:documentation
   "Convenience method same as (mute obj :solo)"))

;; composition/time-signature.lisp:
(defgeneric subbeat-duration (obj)
  (:documentation
   "Returns time-signature sub-beat duration."))

;; composition/time-signature.lisp:
(defgeneric subbeats (obj)
  (:documentation
   "Returns time-signature sub-beat count."))

;; composition/time-signature.lisp:
(defgeneric subbeats! (obj value)
  (:documentation
   "Sets time-signature sub-beat count."))

;; composition/time-signature.lisp:
(defgeneric tempo (obj)
  (:documentation
   "Returns time-signature tempo in BPM."))

;; composition/time-signature.lisp:
(defgeneric tempo! (obj value)
  (:documentation
   "Sets time signature tempo in BPM."))

;; composition/time-signature.lisp:;;
(defgeneric tick-duration (obj)
  (:documentation
   "Returns duration of a single tick for time-signature."))

;; composition/time-signature.lisp:
(defgeneric ticks-per-beat (obj)
  (:documentation
   "Returns the time-signature ticks per beat count."))

;; generics.lisp:
(defgeneric transpose (obj x)
  (:documentation
   "Apply key-number transposition on object.
Transpose may be called on any object type without producing an error.
If an object has :TRANSPOSABLE value set to nil, it is not transposed.
obj - The object or list of objects.
x - integer, transposition amount."))

;; composition/time-signature.lisp:
(defgeneric unit (obj)
  (:documentation
   "Returns the time-signature beat unit."))

;; composition/time-signature.lisp:
(defgeneric unit! (obj value)
  (:documentation
   "Sets time-signature beat-unit."))

;; composition/project.lisp:
(defgeneric unmute (obj)
  (:documentation
   "Convenance method, same as (mute obj :unmute)"))

;; composition/project.lisp:
(defgeneric unmute-all (obj)
  (:documentation
   "Unmute all child elements of object."))

;; midi/midi.lisp:
(defgeneric write-smf (obj filename &key pad no-overwrite)
  (:documentation
   "Write MIDI file object to disc."))
