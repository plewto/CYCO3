# CYCO Installation.


##### Requirements

The goal from the start was to reduce the number of dependencies to the
bare minimum.  CYCO itself is implemented in 100% Common Lisp and was
developed using [SBCL](http://www.sbcl.org/).  Originally that was it for
requirements; a good Lisp implementation.  The inclusion of OSC
entailed additional dependencies:

* Lisp implementation, CYCO has only been tested with SBCL.
* [quicklisp](https://www.quicklisp.org/beta/)
* [OSC](https://github.com/zzkt/osc), Installed with quicklisp when CYCO is loaded.
* [USOCKET](https://common-lisp.net/project/usocket/), Installed with quicklisp when CYCO is loaded.

##### Installation

1. Place CYCO distribution folder in convenient location.

2. Configuration is not absolutely necessary but allows CYCO to be
customized to your needs. 

   A. Create folder ~/.config/cyco/<p>
   B. Create file ~/.config/cyco/cyco-config.lisp

   The folder CYCO/config/ contains example configuration files you may
   use,  see CYCO/config/README. 
3. Create the directory ~/cyco-projects/

4. Install [quicklisp](https://www.quicklisp.org/beta/) as per instructions
in the link.  CYCO expects quicklisp to be at the default location
~/quicklisp/


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

At this point CYCO is ready to use, but before doing anything else you may
want to load the configuration files.

<pre>
    CYCO: (load-config)
</pre>

After configuration you may save a snapshot executable (so far this has only
been tested with SBCL)

<pre>
    CYCO: (save-snapshot "name-of-executable")
</pre>


Starting CYCO from an executable is considerably faster, once the Lisp
prompt appears enter (CYCO) to use the CYCO package.   Configuration
values are "baked in" to the snapshots when they are created.  To alter the
configuration repeat the steps above.
