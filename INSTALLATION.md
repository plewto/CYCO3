# CYCO Installation.

## ISSUE: Configuration details are out of date.
## TODO: Replace config section with plugins.

##### Requirements

The goal from the start was to reduce the number of dependencies to the
bare minimum.  CYCO itself is implemented in 100% Common Lisp and was
developed using [SBCL](http://www.sbcl.org/).  Originally that was it for
requirements; a good Lisp implementation.  The inclusion of OSC
entailed additional dependencies:

* Lisp implementation, CYCO has only been tested with SBCL.
<strike>
* [quicklisp](https://www.quicklisp.org/beta/)
* [OSC](https://github.com/zzkt/osc), Installed with quicklisp when CYCO is loaded.
* [USOCKET](https://common-lisp.net/project/usocket/), Installed with quicklisp when CYCO is loaded.
</strike>

##### Installation

1. Place CYCO distribution folder in a convenient location.

<strike>
2. Configuration profiles are not absolutely necessary but allows CYCO to be
customized to your needs.  By default profiles are located in  

   ~/.config/cyco/



   The folder CYCO/config/ contains example profile directories you may
   use,  see CYCO/config/README.  Initially you may wish to create a
   sym-link from CYCO/configuration to ~/.config/cyco
   
</strike>

3. Create the directory ~/cyco-projects/

<strike>
4. Install [quicklisp](https://www.quicklisp.org/beta/) as per instructions
in the link.  CYCO expects quicklisp to be at the default location
~/quicklisp/
</strike>


##### Starting CYCO
<pre>
   $     - command line prompt
   *     - Lisp REPL prompt
   CYCO: - Lisp REPL prompt while using CYCO

   $ cd ~/CYCO          # or wherever you have placed the CYCO directory.
   $ sbcl               # or whatever Lisp you are using.
   * (load "src/cyco")  
   * (cyco)
   CYCO: 
</pre>


<strike>
At this point CYCO is ready to use, but before doing anything else you may
want to load a profile


<pre>
    CYCO: (load-profile)

    ;; If the above does not work try

    CYCO: (load-profile 'general-midi)
</pre>   

After configuration you may save a snapshot executable (so far this has only
been tested with SBCL)
</strike>

<pre>
    CYCO: (save-snapshot "name-of-executable")
</pre>

CYCO will automatically terminate after creating a snapshot.

Starting CYCO from an executable is considerably faster, once the Lisp
prompt appears enter (CYCO) to use the CYCO package.   Configuration
values are "baked in" to the snapshots when they are created.  To alter the
configuration repeat the steps above.
