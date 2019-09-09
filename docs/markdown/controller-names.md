# CYCO Symbolic Controller Names

Symbolic names may be assigned to MIDI controller numbers.

**(DEFINE-CONTROLLER name controller-number)**

    Assign name to MIDI controller number.
	
**(DEFINED-CONTROLLERS)**

    Returns list of assigned controller names.
	
**(GET-CONTROLLER-NUMBER name &key (default nil))**

    Returns controller number assigned to name.
	If name is not an assigned controller, return default.
	
## Default Controller Assignments

    BANK-SELECT   0
    WHEEL         1
    BREATH        2
    FOOT          4
    PORT-TIME     5
    VOLUME        7
    PAN          10
