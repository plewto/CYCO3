;;;; CYCO
;;;;
;;;; Defines all generic CYCO functions.
;;;; In a few cases implements simpler methods.
;;;;

(defmacro def-type-predicate (name &optional documentation)
  "Defines a new generic predicate function. 
Implements the default, false, method.
A true method must be manually implemented."
  `(progn
     (defgeneric ,name (obj)
       (:documentation (or ,documentation "Predicate")))
     (defmethod ,name ((obj t)) nil)))

(defgeneric ->cycle (obj)
  (:documentation
   "Coerce object to Cycle pattern.
If object is an instance of Cycle, return it.
If object is some non-cycle Pattern, return a new Cycle with the elements of object.
If object is a list, return new Cycle :of object.
For all other types, coerce object to a list and return new Cycle."))

(defgeneric ->list (obj)
  (:documentation
   "Coerce object to a list.
If object is a list, return it.
If object is some other sequence type, return new list of object's elements.
For all other types, create new list with object as it's only element.
see ->vector"))

(defgeneric ->markov-link (obj)
  (:documentation "Coerce object to a markov-link"))

(defgeneric ->pattern (obj &key ptype)
  (:documentation
   "Coerce object to a Pattern of type ptype.
If object is a ptype Pattern, return it.
If object is a non-ptype Pattern, return new ptype Pattern with elements taken
from obj.
For all other types return (ptype :of obj)
The default ptype is Cycle."))


(defgeneric ->smf (contents &key filename offset repeat pad)
  (:documentation
   "Creates new MIDI file from contents and write to file.
:filename - Optional String, if not specified the filename is derived from
            the name of contents and the current project.
:offset   - Optional, initial offset time in seconds, default 0.
:repeat   - Optional int, number of times to repeat contents, default 1.
:pad      - Optional float, number of seconds to add to end of file, default 2."))

(defgeneric ->string (obj)
  (:documentation
   "Returns string representation of object."))

(defmethod ->string ((obj t))(format nil "~A" obj))
(defmethod ->string ((s string)) s)
(defmethod ->string ((lst list)) (mapcar #'->string lst))

(defgeneric ->symbol (arg)
  (:documentation
   "Intern new symbol using name of argument."))

(defmethod ->symbol ((s symbol)) s)
(defmethod ->symbol ((s string))(intern (string-upcase s)))
(defmethod ->symbol ((n number))(->symbol (->string n)))

(defgeneric ->vector (obj)
  (:documentation
   "Coerce object to vector.
If object is a vector, return it.
If object is a non-vector sequence, return new vector using elements of object.
For all other cases return new vector with obj as it's only element.
See ->list"))

(defgeneric ? (obj)
  (:documentation
   "Generalized help/inspection function."))

(defmethod ? ((obj t))
  (format t "~A~%" (type-of obj)))

(defmethod ? ((obj symbol))
  (if (boundp obj)
      (format t "Symbol ~A --> value ~A~%" obj (symbol-value obj)))
  (if (fboundp obj)
      (describe obj)))

(defgeneric add-group (section group)
  (:documentation
   "Adds group to section."))

(def-type-predicate alist-p
   "Predicate true if argument is an association list.")

(defmethod alist-p ((lst list))
  (every #'(lambda (q)(and (consp q)(symbolp (car q)))) lst))

(defgeneric articulation-map (inst)
  (:documentation
   "Returns instrument's articulation-map."))

(defgeneric articulation-map! (inst mapfn)
  (:documentation
   "Sets instrument's articulation-map"))

(def-type-predicate bag-p
   "Predicate, true if argument is an instance of Bag.")

(defgeneric bar-duration (obj)
  (:documentation
   "Returns duration of single bar from time-signature."))

(defgeneric bars (obj)
  (:documentation
   "Returns time-signature bar count."))

(defgeneric bars! (obj value)
  (:documentation
   "Sets time-signature bar count."))

(defgeneric beat-duration (obj)
  (:documentation
   "Returns time-signature beat duration."))

(defgeneric beats (obj)
  (:documentation
   "Returns time-signature beat count."))

(defgeneric beats! (obj value)
  (:documentation
   "Sets time-signature beat count."))

(defgeneric butfinal (obj)
  (:documentation
   "Returns all but final element of sequence.
(butfinal '(a b c)) --> (a b)"))

(defgeneric cardinality (obj)
  (:documentation
   "Returns number of elements in a Pattern."))

(defgeneric channel (obj &optional resolve)
  (:documentation
   "Returns MIDI channel of object.
MIDI channels may be symbolic and one symbolic channel may map 
to another symbol.  I.E. channel 'snare may map to channel 'drum.
If the optional resolve argument is true the result is the actual 
MIDI channel, an integer between 1 and 16 inclusive."))

(defmethod channel ((obj null) &optional resolve)
  (dismiss resolve)
  1)

(defgeneric channel! (inst channel)
  (:documentation
   "Sets instrument's MIDI channel.  
channel may be a symbolic meta-channel or an integer between 1 and 16 
inclusive."))

(defgeneric channel-index (obj)
  (:documentation
   "Returns MIDI channel-index of object.
The channel-index is the lower 4-bits of a MIDI channel message status byte
and is always in the range 0..15 inclusive.  It is 1 less then the MIDI 
channel.   User facing functions should generally use MIDI channel instead 
of the channel index."))

(defmethod channel-index ((n null)) 0)
(defmethod channel-index ((channel integer))(logand (1- channel) #x7F))

(defgeneric child-of-p (parent child)
  (:documentation
   "Predicate, true if child is a direct child of parent.  See cyco-node."))

(def-type-predicate chord-model-p
   "Predicate, true if object is an instance of chord-model.")

(defgeneric chord-template (model name &optional variation)
  (:documentation
   "Returns list of key-number offsets for named chord in model.
A chord template is defined as a list. For example a major triad has the 
template (0 4 7).

Optional variation argument not supported by all methods."))

(defgeneric chord-types (model)
  (:documentation
   "Returns list of chord types defined by chord-model."))

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

(defmethod clone ((obj t) &key new-name new-parent)
  (dismiss new-name new-parent)
  obj)

(defmethod clone ((src hash-table) &key new-name new-parent)
  (dismiss new-name new-parent)
  (let ((dst (make-hash-table :size (hash-table-count src))))
    (maphash #'(lambda (k v)(setf (gethash k dst)(clone v))) src)
    src))

(defmethod clone ((c cons) &key new-name new-parent)
  (dismiss new-name new-parent)
  (cons (clone (car c))(clone (cdr c))))

(defmethod clone ((v vector) &key new-name new-parent)
  (dismiss new-name new-parent)
  (->vector (clone (->list v))))

(defgeneric cnth (n seq)
  (:documentation
   "Returns nth element of sequence in a circular manner.
(cnth 2 '(a b c)) --> C
(cnth 3 '(a b c)) --> A
(cnth 4 '(a b c)) --> B"))

(defgeneric connect (parent child)
  (:documentation
   "Make parent node the parent of child.
If child has an existing parent, that connection is severed."))

(def-type-predicate cycle-p
   "Predicate, true if object is an instance of Cycle.")

(defgeneric data (obj index)
  (:documentation
   "Returns the indexed value from object."))

(defgeneric data-count (obj)
  (:documentation
   "Returns number of data values in object."))

(defgeneric define-chord (model name template &optional description)
  (:documentation
   "Defines a new chord-mode chord.
mode - An instance of chord-model.   
name - Symbol
template - List of key-number offsets.
description - Optional text."))

(defgeneric defines-chord-p (chord name)
  (:documentation 
   "Predicate, true if chord-model defines the named chord."))

(def-type-predicate dice-p
   "Predicate, true if object is an instance of Dice.")

(defgeneric disconnect (child)
  (:documentation
   "Severs connection between child and it's parent, if any. 
After a disconnect the child becomes a root node."))

(defgeneric dump-chords (model)
  (:documentation
   "Displays list of chords defined by chord-model."))

(defgeneric dump-events (obj &key range filter render)
  (:documentation
   "Display listing of MIDI events.
obj - Source of event
:range - time range (start end)
:filter - function, predicate on events.  If the result it true do not 
display the event.
;render - If true display actual MIDI bytes."))

(defgeneric dump-smf-track-chunk (trk pad)
  (:documentation
   "Diagnostic displays contents of MIDI file track."))

(defgeneric duration (obj)
  (:documentation
   "Returns duration of time-signature phrase."))

(defmethod duration ((n number))(float n))

(defgeneric dynamic (obj)
  (:documentation
   "Returns dynamic value of object or list of objects."))

(defgeneric dynamic->velocity (dy)
  (:documentation
   "Convert dynamic value to MIDI velocity."))

(defgeneric dynamic-map (inst)
  (:documentation
   "Returns instruments dynamic-map function."))

(defgeneric dynamic-map! (inst mapfn)
  (:documentation
   "Sets instrument's dynamic-map function."))

(defgeneric dynamic-name (obj)
  (:documentation
   "Returns symbolic name of dynamic value.
Note this function is generally inefficient and there may be slight 
rounding errors."))

(def-type-predicate dynamic-p
  "Predicate, true if argument can be interpreted as a dynamic value.")

(defgeneric final (obj)
  (:documentation
   "Returns final element from sequence."))

(defgeneric find-child (parent child)
  (:documentation
   "Find indicated child of parent.
Returns nil if no such child exists."))

(def-type-predicate group-p
   "Predicate, true if object is an instance of Group.")

(defgeneric hash-property (node key)
  (:documentation
   "Predicate, true if node has indicated property."))

(defgeneric has-group-p (section grp)
  (:documentation 
   "Predicate, true if section contains named group."))

(defgeneric init-time-signature (obj)
  (:documentation
   "Initialize time-signature."))

(def-type-predicate instrument-layer-p
   "Predicate, true if object is an instance of Instrument-layer.")

(def-type-predicate instrument-p
   "Predicate, true if object is an instance of Instrument.")

(defgeneric invert (obj pivot)
  (:documentation
   "Apply key-number inversion to object.
Inversion may be applied to any object without producing an error.
If the object has the property :transposable with a nil value, then no 
inversion is applied. 

The pivot point is a key-number around which the inversion takes place.
All negative values have an inversion of +REST+ = -1.

Ignore transformation if pivot is nil.

(invert '(-1234 59 60 70) 60) --> (-1 61 60 50)"))

(defmethod invert ((obj t) pivot) obj)

(defgeneric keyname (obj)
  (:documentation
   "Returns symbolic name for key-number.
For equivalent enharmonic notes, the sharp value is always used.
Keyname will not return names for user defined keynumbers."))


(defgeneric keynumber (obj)
  (:documentation
   "Converts object, or list of objects, to MIDI key-numbers."))

(defgeneric keynumber-map (inst)
  (:documentation
   "Returns instrument's keynumber-map function."))

(defgeneric keynumber-map! (inst mapfn)
  (:documentation
   "Sets instrument's keynumber-map function."))

(def-type-predicate keynumber-p
   "Predicate, true if object can be interpreted as a key-number.")

(def-type-predicate line-p
   "Predicate, true if object is an instance of Line.")

(defgeneric local-properties (obj)
  (:documentation
   "Returns list of object's local-property names.  
A local property is a property defined directly by an object.
Properties defined by an object's parent are not considered local."))

(def-type-predicate markov-chain-p)
(def-type-predicate markov-link-p)


(defgeneric markov-add-link (source destination weight)
  (:documentation
   "Adds potentail destinations for a Markov-link.
source - the markov-link
destination - potentail next value.  The destination may be any type
but is coerced to a markov-link.
weight - The number of times destination is added to sources' links list."))

(defgeneric markov-walk (mrkv)
  (:documentation
   "Returns the next markov-link from the argument."))

(defgeneric meta-channel (name  &optional resolve)
  (:documentation
   "Returns value of named MIDI channel.  A symbolic meta-channel may
map to another symbolic channel.  If optional resolve argument is true
then the actual MIDI channel, as an integer between 1 and 16 inclusive, 
is returned."))

(defgeneric metric (obj)
  (:documentation
   "Converts object, or list of objects, to float metric values."))

(defgeneric metric-expression (exp)
  (:documentation
   "Evaluates metric-expression and return it's value as a float."))

(defgeneric metric-expression-p (exp)
  (:documentation
   "Returns non-nil if exp may be interpreted as a metric-expression.
Parsing metric-expressions may be slow and exp is evaluated to determine
if it is a valid expression.  If exp is invalid the result is nil.  If
exp is a valid metric-expression, the result is its float value."))

(def-type-predicate metric-p
   "Predicate, true if argument may be interpreted as a basic metric value.")

(def-type-predicate midi-channel-message-p
   "Predicate, true if object is a MIDI channel message.")

(def-type-predicate midi-channel-pressure-p
   "Predicate, true if object is a MIDI channel pressure message.")

(def-type-predicate midi-control-chnage-p
   "Predicate, true if object is a MIDI control change message.")

(def-type-predicate midi-end-of-track-p
   "Predicate, true if object is a MIDI end of track message.")

(def-type-predicate midi-end-system-exclusive-p
   "Predicate, true if object is a MIDI end of system-exclusive message.")

(def-type-predicate midi-key-message-p
   "Predicate, true if object is a keyed MIDI message.  Keyed messages 
include: note-off, note-on and poly-pressure.")

(def-type-predicate midi-key-signature-p
   "Predicate, true if object is a MIDI key-signature message.")

(def-type-predicate midi-message-p
   "Predicate, true if object is any type of MIDI message.")

(def-type-predicate midi-meta-copyright-p
"Predicate, true if object is a midi-meta-copyright message.")

(def-type-predicate midi-meta-cue-p
"Predicate, true if object is a midi-meta-cue message.")

(def-type-predicate midi-meta-instrument-name-p
"Predicate, true if object is a midi-meta-instrument-name message.")

(def-type-predicate midi-meta-lyric-p
"Predicate, true if object is a midi-meta-lyric message.")

(def-type-predicate midi-meta-marker-p
"Predicate, true if object is a midi-meta-marker message.")

(def-type-predicate midi-meta-message-p
"Predicate, true if object is any type of MIDI meta message.")

(def-type-predicate midi-meta-text-p
"Predicate, true if object is any type of MIDI meta text message.")

(def-type-predicate midi-meta-track-name-p
"Predicate, true if object is a midi-meta-track-name message.")

(def-type-predicate midi-note-off-p
"Predicate, true if object is a midi-note-off message.")

(def-type-predicate midi-note-on-p
"Predicate, true if object is a midi-note-on message.")

(def-type-predicate midi-pitch-bend-p
"Predicate, true if object is a midi-pitch-bend message.")

(def-type-predicate midi-poly-pressure-p
"Predicate, true if object is a midi-poly-pressure message.")

(def-type-predicate midi-program-change-p
"Predicate, true if object is a midi-program-change message.")

(def-type-predicate midi-system-common-message-p
"Predicate, true if object is a midi-system-common-message message.")

(def-type-predicate midi-system-exclusive-p
"Predicate, true if object is a midi-system-exclusive message.")

(def-type-predicate midi-tempo-message-p
"Predicate, true if object is a midi-tempo-message message.")

(def-type-predicate midi-time-signature-p
"Predicate, true if object is a midi-time-signature message.")

(defgeneric mnemonic (obj)
  (:documentation
   "Returns short mnemonic string for object 
Mnemonic is primarily used for MIDI message type abbreviations."))

(defgeneric mute (obj &optional state)
  (:documentation
   "Sets mute state of object.
Possible states are :mute :unmute :solo and nil
For solo this object is switched to non-muted while all sibling objects
are muted.  A nil value leaves the current state as is."))

(defgeneric mute-all (obj)
  (:documentation
   "Sets all child objects to muted."))

(defgeneric muted-p (obj)
  (:documentation 
   "Predicate, true if object is muted."))

(defgeneric name (obj)
  (:documentation
   "Returns object's name.  Name may be applied to any object without error."))

(defmethod name ((obj t)) nil)
(defmethod name ((s symbol)) (symbol-name s))
(defmethod name ((s string)) s)

(defgeneric name! (obj new-name)
  (:documentation
   "Sets name of object."))

(defgeneric next (obj &optional n)
  (:documentation
   "Returns next value(s) from Pattern or pattern-like object.
By default a single value is returned (see next-1).
If n is a positive integer, returns list of the next n values.
If n is the keyword :rest, returns list of all remaining values
If n is the keyword :all, returns list of all elements.
See next-1 and next-n."))

(defgeneric next-1 (obj)
  (:documentation
   "Returns the next value from a Pattern or pattern-like object."))

(defgeneric next-n (obj n)
  (:documentation
   "Returns a list of the next n values from a Pattern or pattern-like 
object."))

(defgeneric note-events (inst time keynum duration dynamic &key time-scale)
  (:documentation
   "note-events is a low-level method to convert time, key, duration and 
dynamics to MIDI events using a specific instrument.  The instrument's 
keynumber, articulation and dynamic maps are applied to the result.
The result is a two-element list 
    ((time . note-on)
     (time+duration . note-off))"))

(defgeneric octave (obj)
  (:documentation
   "Returns octave number of key number or list of key numbers.
All negative key numbers have octave -1."))

(defgeneric palindrome (obj &key elide)
  (:documentation
   "Creates palindrome from source sequence.
The :elide argument determine how end-values are treated.
(palindrome '(A B C D))  --> (A B C D D C B A)
(palindrome '(A B C D) :elide :last)  --> (A B C D C B A)
(palindrome '(A B C D) :elide :first) --> (A B C D D C B)
(palindrome '(A B C D) :elide :both)  --> (A B C D C B)"))

(defmethod palindrome (obj &key elide)
  (dismiss elide)
  (clone obj))

(defgeneric parent (obj)
  (:documentation
   "Returns object's parent.   If object is a root node, returns nil."))

(def-type-predicate part-p
   "Predicate, true if object is an instance of any type of Part.")


(defgeneric path-to-root (obj)
  (:documentation
   "Returns list of all nodes between this object and the root node (inclusive)."))

(def-type-predicate pattern-p
   "Predicate, true if object is any type of Pattern.")

(defgeneric permute (obj)
  (:documentation
   "Returns permutation of sequence."))

(defmethod permute ((obj t)) obj)

(defgeneric phrase-duration (obj)
  (:documentation
   "Returns time-signature phrase-duration."))

(defgeneric pick (obj)
  (:documentation
   "Select an object at random from collection."))

(defmethod pick ((obj t)) obj)
(defmethod pick ((n integer)) (random n))

(defgeneric pitch-class (obj)
  (:documentation
   "Returns the pitch-class, an integer between 0 and 11 inclusive, 
of a key-number or list of key numbers.  All negative key-numbers
have a pitch class of -1."))

(defgeneric print-tree (node &optional depth)
  (:documentation
   "Print hierarchal tree starting at node.  
The depth argument is used internally."))

(defgeneric priority (obj)
  (:documentation
   "Returns the priority of a MIDI message.  If two MIDI events have the 
same time stamp, the event-message with the lowest priority appears first in 
the events list.  There are primarily two applications.  First it ensures 
certain meta events appear in the correct location.  I.E. end-of-track always
appears after all other events.  Secondly, note-off events always appear after 
note on-events to prevent stuck notes."))

(defgeneric program-bank (obj)
  (:documentation
   "Returns an instruments program-bank.
There is no pre-defined format for a program-bank, it is up to the specific
instrument.   

NOTE: This is not a 'bank' of programs, rather it is the name of a bank on some 
external MIDI device."))

(defgeneric program-bank! (inst pbank)
  (:documentation
   "Sets instruments program bank."))

(defgeneric program-change-events (inst time &key bank program)
  (:documentation
   "Generates list of MIDI events to effect an instrument's program change.
The result format is dependent on the specific external MIDI instrument.
At it's simplest the result is a single program-change event.  
More complex results may include control changes for bank selection.  
The result may also include note events for 'key-switched' instruments."))

(defgeneric program-map (obj)
  (:documentation
   "Returns an instrument's program-map function."))

(defgeneric program-map! (obj fn)
  (:documentation
   "Sets instrument's program-map function."))

(defgeneric program-number (obj)
  (:documentation
   "Returns instrument's default program-number.  There is no predefined 
format, though integers between 0 and 127 are generally treated as MIDI 
program numbers.  The exact format is dictated by the program-map 
function."))

(defgeneric program-number! (inst pnumber)
  (:documentation
   "Returns instrument's program-number."))

(def-type-predicate project-p
   "Predicate, true if object is an instance of Project.")

(defmethod project-p ((obj null)) nil)

(defgeneric properties (obj &optional acc)
  (:documentation
   "Returns list of all property keys defined by object."))

(defgeneric property (obj key)
  (:documentation
   "Returns object's property value assigned to key.
It is an error if the object does not define key as a property."))

(defgeneric property* (obj key) 
  (:documentation
   "Private function for internal use."))

(defgeneric prune (node &optional force)
  (:documentation
   "Starting at node recursively disconnect all child nodes for which the 
:TRANSIENT property is true.  If force is true, disconnect all nodes,
ignoring their transient value."))


(defgeneric push-event (time message obj)
  (:documentation
   "Push a new MIDI event to object. 
time - float
message - MIDI message
obj - destination."))

(defgeneric put (obj key value)
  (:documentation
   "Assign's value to an object,s property.
obj - the object
key - symbol, property name.  It is an error if the object does not 
define key as a property.
value - the new value."))


(defgeneric remaining (obj)
  (:documentation
   "Returns list of the remaining values yet to be returned by a Pattern
or pattern-like object."))

(defgeneric remarks (obj)
  (:documentation
   "Returns object's remarks text."))

(defgeneric remarks! (obj text)
  (:documentation
   "Sets object's remarks text."))

(defgeneric render-midi-message (obj)
  (:documentation
   "Convert MIDI-XXX-message object to list of MIDI bytes."))

(defgeneric render-n (obj count &key offset)
  (:documentation
   "Convert object to list of MIDI events.
If count is greater then 1, place each additional copy shifted by the 
duration of the object.  

:offsets shifts all events by offset seconds, default 0."))

(defgeneric render-once (obj &key offset)
  (:documentation
   "Convert object to a list of MIDI events.
:offset shifts all events by offset seconds, default 0."))

(defgeneric render-smf (obj &key pad)
  (:documentation
   "Converts Standard MIDI file object to list of MIDI events.
:pad adds pad seconds at end of events."))

(defgeneric render-smf-header (obj track-count)
  (:documentation
   "Low-level method, renders MIDI file header."))

(defgeneric render-smf-track (trk pad)
  (:documentation
   "Low-level method, renders MIDI file track."))

(defgeneric reset (obj)
  (:documentation
   "Restore internal state of object to some initial condition.
See also soft-reset."))
   
(defmethod reset ((obj t)) obj)

(defgeneric rest-p (obj)
  (:documentation 
   "Predicate, true if object is interpreted as a rest.
Generally any negative number, and the symbol 'r are treated as rest."))

(defgeneric retrograde (obj)
  (:documentation
   "Reverse object's elements. 
If the object has the :RESERVABLE property with a value of nil, do not 
apply.   Retrograde may be called with any object without producing an
error."))

(defmethod retrograde ((obj t)) obj)
(defmethod retrograde ((seq sequence))(reverse seq))

(defgeneric root-p (obj)
  (:documentation 
   "Predicate, true if object is a root node."))

(def-type-predicate section-p
   "Predicate, true if object is an instance of Section or one of it's
subclasses.")

(defgeneric seq-order (sections &key project)
  (:documentation
   "Sets the sequence order of Sections within a project."))

(def-type-predicate slew-p)

(defgeneric slice (seq start &optional end)
  (:documentation
   "Returns a slice of a sequence.
seq - 
start - Staring index 
end   - Ending index, may be negative to index from end, defaults to end
of sequence."))

(defgeneric smf-track (obj &optional index)
  (:documentation
   "Low-level method, returns indexed track from MIDI file.
NOTE: At least for now CYCO only supports single-track MIDI files."))

(defgeneric smf-track! (obj trk &optional index)
  (:documentation
   "Low-level method to set MIDI file track.
NOTE: At least for now CYCO only supports single-track MIDI files."))

(defgeneric smf-track-count (obj)
  (:documentation
   "Returns number of tracks in MIDI file."))

(defgeneric soft-reset (obj)
  (:documentation
   "Performs a soft-reset on object.   A soft reset initializes some of an 
objects parameters but not all of them.  This is in contrast to a normal 
reset which initializes all values.  The exact behavior is dependent on the 
specific object type."))

(defmethod soft-reset ((obj t)) obj)

(defgeneric solo (obj)
  (:documentation
   "Convenience method same as (mute obj :solo)"))

(defgeneric subbeat-duration (obj)
  (:documentation
   "Returns time-signature sub-beat duration."))

(defgeneric subbeats (obj)
  (:documentation
   "Returns time-signature sub-beat count."))

(defgeneric subbeats! (obj value)
  (:documentation
   "Sets time-signature sub-beat count."))

(defgeneric tbeats (obj)
  (:documentation
   "Returns time-signature triplet-beat count"))

(defgeneric tbeat-duration (obj)
  (:documentation
   "Returns time-signature triplet-beat duration."))

(defgeneric tempo (obj)
  (:documentation
   "Returns time-signature tempo in BPM."))

(defgeneric tempo! (obj value)
  (:documentation
   "Sets time signature tempo in BPM."))

(defgeneric tick-duration (obj &key unit)
  (:documentation
   "Returns duration of a single tick for time-signature."))

(defgeneric ticks-per-beat (obj)
  (:documentation
   "Returns the time-signature ticks per beat count."))

(defgeneric tsubbeats (obj)
  (:documentation
   "Returns time-signature subbeat triplet count."))

(defgeneric tsubbeat-duration (obj)
  (:documentation
   "Returns time-signature subbeat triplet duration."))

(defgeneric transpose (obj x)
  (:documentation
   "Apply key-number transposition on object.
Transpose may be called on any object type without producing an error.
If an object has :TRANSPOSABLE value set to nil, it is not transposed.
obj - The object or list of objects.
x - integer, transposition amount."))

(defmethod transpose ((obj t) x) obj)

(defgeneric unit (obj)
  (:documentation
   "Returns the time-signature beat unit."))

(defgeneric unit! (obj value)
  (:documentation
   "Sets time-signature beat-unit."))

(defgeneric unmute (obj)
  (:documentation
   "Convenance method, same as (mute obj :unmute)"))

(defgeneric unmute-all (obj)
  (:documentation
   "Unmute all child elements of object."))

(defgeneric value (obj))
(defmethod value ((obj t)) obj)

(def-type-predicate walker-p)
(def-type-predicate wrapper-p)

(defgeneric write-smf (obj filename &key pad no-overwrite)
  (:documentation
   "Write MIDI file object to disc."))


