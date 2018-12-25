;;;; CYCO pdf plugin
;;;; 
;;;; The PDF plugin defines several random probability density functions.
;;;; Much of the core functionality was ported from Nyquist. 
;;;; https://www.cs.cmu.edu/~music/nyquist/
;;;;
;;;; Each distribution foo is provided in two forms:
;;;;
;;;;   1) foo-dist  
;;;;       Returns random numbers with the distribution foo.
;;;;       These functions take one or more keyword arguments.
;;;;
;;;;   2) foo-pdf
;;;;       Returns new function with the foo distribution.
;;;;       foo-pdf takes the same arguments as foo-dist and also
;;;;       limiting values.  The resulting function takes no arguments
;;;;       with the keyword parameters 'baked in'.  Depending on
;;;;       the distribution, factory functions are marginally
;;;;       faster then the general distribution function.
;;;;
;;;;

(in-package :cyco)

(load-plugin-file "continuous-distributions")
(load-plugin-file "discrete-distributions")
(load-plugin-file "factory")
