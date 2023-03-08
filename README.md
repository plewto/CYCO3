# CYCO3 README
© 2021, 2022, 2023 Steven Jones, plewto@gmail.com

# CYCO4
Early development has begun on **CYCO4** to address issues that have
arisen with CYCO3, particularly the ill-conceived 't-subbeats' method for
handling triplets.  As such, updates to CYCO3 will be sporadic.

**CYCO3** is a Lisp based music composition language available from
[github/plewto/cyco3](https://github.com/plewto/CYCO3) 

Detailed HTML documentation may be found at https://plewto.github.io/cyco3-docs/

## Brief Overview

### Features

- Project centric design.
- Flexible functional time specification.
- Rich set of algorithmic pattern generators.
- Symbolic names for common music parameters.
- Large library of chords, includes plugins for chords based on fretted
  instruments.
- Outputs to MIDI file (external MIDI file player is required).  

### Requirements

- Common Lisp, CYCO was developed with [SBCL](http://www.sbcl.org/)
- A few of the optional plugins require [Quicklisp](https://www.quicklisp.org/beta/)


### Installation

1. Copy cyco3 folder to home directory  ~/cyco3/
2. Create the following directories:
   - ~/.config/cyco/
   - ~/.config/cyco/plugins/
   - ~/cyco-projects/


### Usage

1. From a terminal cd into the cyco3 directory and start SBCL. 

       $ cd ~/cyco3
       $ sbcl
       *
	   
2. At the Lisp prompt evaluate (load "build")

       * (load "build")
       Building CYCO...
	
3. After the source files load and the Lisp prompt returns, evaluate (cyco)

       * (cyco)

	
The Lisp prompt changes to CYCO: and is updated to reflect the current
project's name.


### Executable

If using SBCL you may create an executable of the current CYCO state with
the SNAPSHOT command.  Snapshot takes an optional filename argument.  If no
filename is specified the executable is saved to ~/bin/cyco

       * (snapshot)
	   
Note that creating a snapshot will terminate CYCO.


	

   
    
   
   


