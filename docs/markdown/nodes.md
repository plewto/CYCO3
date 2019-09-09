# CYCO Nodes

A **CYCO-NODE** is an object used in the construction of various trees.  A
node has at most one parent and any number of child nodes.<br>

Each node type defines a pre-established set of *properties*, and a node may
either directly set a property value or inherit the value from it's
parent.  It is not possible for a node to define a property which has not
been established at the time it was constructed.<br>

CYCO maintains two major types of trees.

- Orchestra - a tree of instruments rooted at \*ROOT-INSTRUMENT\*
- Project trees.  The current project is rooted at \*PROJECT\*


While CYCO-NODE was not explicitly designed for the end-user, they could
conceivably have applications for algorithmic composition.

---- 
## Node Related Functions

**(NAME! node new-name)** <br>
**(NAME node)**

    Sets/retrieves node name.
	
**(REMARKS! node text)** <br>
**(REMARKS node)**

    Sets/retrieves optional remarks text.
	
**(ROOT-P node)**

    Predicate, true if node has no parent.
	
**(FIND-CHILD parent child)**  <br>
**(FIND-CHILD parent child-name)**

    Returns child node of parent.
	A nil result indicates parent does not have indicated child.
	
**(PATH-TO-ROOT node)**

    Returns a list of nodes, in order, starting with node back to the root
	node. 
	
**(DISCONNECT node)**

    Sever the connection between node and it's parent, effectively making
	it a root-node.
	
**(CONNECT parent child)**

    Make child a child node of parent.  The connection between child and
	it's previous parent is severed.
	
**(PRUNE node &optional (force nil))**

    Deconstruct tree starting with node.  Each instance of CYCO-NODE has a
	Boolean field named TRANSIENT.  By default PRUNE only disconnects
	those nodes with a true transient value.  If the optional force
	argument is true, then all nodes are disconnected regardless of their
	transient value.
	
	An aside: why all this transient nonsense?
	
	    Typically CYCO is customized by defining a default orchestra tree
		matching the available local equipment.  These are more or less
		permanent instruments corresponding to physical hardware or virtual
		synthesizers and are defined in configuration "plugin" files.
		
		During the composition phase child instruments are defined under
		these permanent instruments.  IE a bass instrument may be
		allocated to a specific hardware synth.   As the composition is
		developed it is usual to repeatedly reload the various project
		files.  These in turn define specific instruments.  If the
		orchestra tree is not pruned each reload would add
		needless duplicate instruments to the tree.  On the other hand it
		is desirable to keep the permanent instruments in place.   The
		transient flag is used to distinguish these two classes of
		instruments.   Non-transient instruments are established by
		configuration.  Transient instruments are created each time a
		project is loaded.  The project is responsible for pruning the
		orchestra tree prior to creating new instruments. 

**(HAS-PROPERTY-P node key)**

    Predicate, true if node defines key as a property.
	
**(PUT node key value)**

    Sets node property key to value.  Is is an error if node does not
    define key as a property.  The new value will shadow the parent nodes
    value for key.
	
**(PROPERTY node key)**

    Returns the value of property key. If node does not define a value for
    key, use the parent node value.  It is an error if key is not an
    established property for node.
	
**(LOCAL-PROPERTIES node)**

    Returns list of property keys explicitly defined by node, parent
    properties are not included.
	
**(PRINT-TREE node &optional (depth 0))**

    Prints tree structure starting at node.  The optional depth argument is
    used internally. 
