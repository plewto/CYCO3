;;;; cyco3/local-config
;;;;


(setf *cyco-location* "~/dev/cyco3")
(setf *config-directory* "~/.config/cyco3")
(setf *default-project-directory* "~/cyco-projects")

(push-plugin-search-path (join-path *cyco-location* "plugins"))
(push-plugin-search-path (join-path *config-directory* "plugins"))

