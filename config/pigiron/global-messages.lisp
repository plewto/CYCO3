;;;; PigIron Profile
;;;;
;;;; Global application-level messages.

(defpig pig-ping ping
  "Checks OSC connectivity, PigIron displays status message in response.")

(defpig ?pig-forest display-forest
  "Instructs PigIron to display current operator forest.")

(defpig ?pig-transmitters 'query-midi-transmitters
  "Instructs PigIron to display list of available MIDI transmitters (input devices).")

(defpig ?pig-receivers 'query-midi-receivers
  "Instructs PigIron to display list of available MIDI receivers (output devices).")

(defpig ?pig-operators 'query-operator-names
  "Instructs PigIron to display names of all active Operators")

(defpig ?pig-optypes 'query-operator-types
  "Instructs PigIron to display all available Operator types.")

(defpig pig-panic 'panic
  "Send fear to PigIron - this should kill any stuck notes.")

(defpig pig-reset 'reset
  "Reset all active operators to their default state.")

(defpig pig-clear-forest 'forget-all
  "Removes all operators.")

(defpig pig-sync-ui 'sync-ui
  "Instructs PigIron to refresh the GUI.")

(defpig-1 pig-isolate-operator name 'isolate-operator
  "Instructs PigIron to remove all connections to named operator.")

(defpig-1 pig-forget-operator name 'forget-operator
  "Instructs PigIron to remove named operator. 
The operator must be 'isolated' first.")

(defpig-2 pig-connect parent child 'connect
  "Instructs PigIron to make connection between parent and child operators.")

(defpig-2 pig-disconnect parent child 'disconnect
  "Instructs PigIron to sever connection between parent and child operators.")

(defun pig-create-operator (optype &key (parent nil)(id nil))
  "Instructs PigIron to create a new operator.
optype - Symbol, the operator type. Use ?pig-optypes to see whats available.
parent - Optional symbol, name of parent operator
id     - Optional int, Sets an explicit operator id.
The operator's name will be 'optype-id'.  If an id is not specified PigIron 
will assign one.  Operator names must be unique.  If your id clashes with an 
existing one, PigIron will ignore your suggestion and use another value."
  (osc-send (list (string-downcase (name optype))
		  (if parent
		      (string-downcase (name parent))
		    "NIL")
		  (if id (->string id) "NIL"))
	    :address (join-address 'create-operator)))


