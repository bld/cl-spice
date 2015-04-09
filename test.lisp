;;;; Validate against General Mission Analysis Tool

(ql:quickload :cl-spice)
(ql:quickload :cl-fad)

(in-package :cl-spice)

(defparameter *spk-dir* (merge-pathnames-as-directory (user-homedir-pathname) #P"src/gmat-git/application/data/planetary_ephem/spk/"))

(defparameter *spkDE421* (merge-pathnames-as-file *spk-dir* "DE421AllPlanets.bsp"))

