;;;; cyco local-config.lisp
;;;;
;;;; Modifies plugins search path.
;;;; 

(in-package :cyco)

(push-plugin-search-path (join-path *cyco-location* "plugins"))
(push-plugin-search-path (join-path *config-directory* "plugins"))

