;;; Foreign function interface to JPL's SPICE library

(defpackage :cl-spice
  (:use :cl :cffi :alexandria)
  (:export
   ;; Error handling
   error-action failed reset
   ;; Kernel management
   furnsh unload kernel-total kernel-data kernel-info kernelp with-kernel with-kernels
   ;; Time & date
   string-to-ephemeris-time ephemeris-time-to-output ephemeris-time-to-utc delta-et j2000-jd unit-time seconds-per-day
   ;; Time systems for 'unit-time' conversion
   tai tdb tdt et jdtdb jdtdt jed
   ;; Ephemeris
   spk-ezr spk-pos
   ;; Physical data
   body-vrd
   ;; Frame transformation
   px-form))

(in-package :cl-spice)

(define-foreign-library libcspice
  (:unix (:or "libcspice.so.6.5" "libcspice.so.6"))
  (t (:default "libcspice")))

(use-foreign-library libcspice)

;;; Utility functions

(defparameter *type-lookup*
  (plist-hash-table
   '(:double double-float
     :int integer))
  "Lookup table for converting C types to Lisp")

(defun to-lisp-vector (ptr type length)
  "Convert a pointer/vector of given type & length to list vector"
  (if (= length 1) (mem-ref ptr type)
      (let ((v (make-array length :element-type (gethash type *type-lookup*))))
	(dotimes (i length)
	  (setf (aref v i) (mem-aref ptr type i)))
	v)))

;;; Error handling

(defcfun ("erract_c" erract) :void
  ;; Input
  (op :string)
  (lenout :int)
  ;; I/O
  (action (:pointer :char)))

(defun error-action (operation &key (lenout 8) (action (make-string lenout)))
  ":get or :set the :action to take when an error is reported.
Input arguments:
---------------
OPERATION - One of :set or :get. :get requires no other arguments. It returns the current setting.
:LENOUT - Length of the string to save action to.
:ACTION - One of :abort, :report, :return, :ignore, :default."
  (with-foreign-string (action-spice (string-upcase action))
    (erract (string-upcase operation) lenout action-spice)
    (convert-from-foreign action-spice :string)))

(defcfun ("failed_c" failed_c) :int)

(defun failed () (not (zerop (failed_c))))

(defcfun ("reset_c" reset) :void)

;; Start using error reporting instead of stopping the program
(error-action :set :action :return)

;;; SPICE kernel management

(defcfun ("furnsh_c" furnsh) :void
  "Furnish a SPICE kernel file, given filename."
  (file :string))

(defcfun ("unload_c" unload) :void
  "Unload a SPICE kernel file, given filename."
  (file :string))

(defcfun ("ktotal_c" ktotal) :void
  ;; Input
  (kind :pointer)
  ;; Output
  (count :pointer))

(defun kernel-total (&optional (kind :all))
  "Number of currently loaded kernels"
  (assert (or (keywordp kind) (stringp kind)))
  (with-foreign-string (kind-str (string-upcase kind))
    (with-foreign-object (count :int)
      (ktotal kind-str count)
      (mem-ref count :int))))

(defcfun ("kdata_c" kdata) :void
  ;; Input
  (which :int)
  (kind :string)
  (fillen :int)
  (typlen :int)
  (srclen :int)
  ;; Output
  (file :pointer)
  (filtyp :pointer)
  (source :pointer)
  (handle :pointer)
  (found :pointer))

(defun kernel-data (which &key (kind :all) (fillen 128) (typlen 32) (srclen 128))
  "Data on the nth kernel"
  (with-foreign-objects
      ((file :char fillen)
       (filtyp :char typlen)
       (source :char srclen)
       (handle :int)
       (found :int))
    (kdata which (string-upcase kind) fillen typlen srclen file filtyp source handle found)
    (values
     (convert-from-foreign file :string)
     (convert-from-foreign filtyp :string)
     (convert-from-foreign source :string)
     (mem-ref handle :int)
     (mem-ref found :int))))

(defcfun ("kinfo_c" kinfo) :void
  ;; Input
  (file :string)
  (typlen :int)
  (srclen :int)
  ;; Output
  (filtyp :pointer)
  (source :pointer)
  (handle :pointer)
  (found :pointer))

(defun kernel-info (file &key (typlen 32) (srclen 128))
  "Retrieve info of specified loaded kernel file"
  (with-foreign-objects
      ((filtyp :char typlen)
       (source :char srclen)
       (handle :int)
       (found :int))
    (kinfo file typlen srclen filtyp source handle found)
    (values
     (convert-from-foreign filtyp :string)
     (convert-from-foreign source :string)
     (mem-ref handle :int)
     (not (zerop (mem-ref found :int))))))

(defun kernelp (filename)
  "Test if kernel filename already loaded"
  (nth-value 3 (kernel-info filename)))

(defmacro with-kernel (filename &body body)
  "Execute forms that require a loaded ephemeris forms using the given
filename. Calls FURNSH and UNLOAD, and returns the result of the body."
  (let ((retval (gensym))
	(found (gensym)))
    `(let ((,found (nth-value 3 (kernel-info ,filename)))) ; check if already loaded
       (unwind-protect
	    (progn
	      (unless ,found (furnsh ,filename)) ; furnish if not already loaded
	      (let ((,retval (multiple-value-list (progn ,@body)))) ; handle multiple values
		(unless ,found (unload ,filename)) ; unload newly loaded kernel
		(apply #'values ,retval))) ; return multiple values if present
	 ;; make sure kernel unloads at the end if it wasn't before
	 (unless ,found (unload ,filename))))))

;;; Use multiple kernels
(defmacro with-kernels (filenames &body body &aux (n (length filenames)))
  (let ((retval (gensym))
	(founds (loop repeat n collect (gensym))))
    ;; Test if each kernel filename is loaded and assign to found variables
    `(let ,(loop for found in founds
	      for filename in filenames
	      collect `(,found (nth-value 3 (kernel-info ,filename))))
       (unwind-protect
	    (progn
	      ;; For each kernel not found, load it with FURNSH
	      ,@(loop for found in founds
		   for filename in filenames
		   collect `(unless ,found (furnsh ,filename)))
	      ;; Run body forms, handling multiple values
	      (let ((,retval (multiple-value-list (progn ,@body))))
		;; Unload kernels if not already
		,@(loop for filename in filenames
		     for found in founds
		     collect `(unless ,found (unload ,filename)))
		;; Return multiple values
		(apply #'values ,retval)))
	 ;; Make sure newly loaded kernels unload if body fails
	 ,@(loop for found in founds
	      for filename in filenames
	      collect `(unless ,found (unload ,filename)))))))

;;; Time & date

(defcfun ("str2et_c" str2et) :void
  (str :string)
  (et :pointer))

(defun string-to-ephemeris-time (string)
  "Date string to ephemeris time. See the following for string format:
http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/str2et_c.html"
  (with-foreign-object (et :double)
    (str2et string et)
    (assert (not (failed)))
    (mem-ref et :double)))

(defcfun ("timout_c" timout) :void
  (et :double)
  (pictur :string)
  (lenout :int)
  (output :string))

(defun ephemeris-time-to-output (et &key (picture "YYYY Mon DD, HR:MN:SC ::UTC") (lenout 128))
  (with-foreign-object (output :char lenout)
    (timout et picture lenout output)
    (assert (not (failed)))
    (convert-from-foreign output :string)))

(defcfun ("et2utc_c" et2utc) :void
  ;; Input
  (et :double)
  (format :string)
  (prec :int)
  (lenout :int)
  ;; Output
  (utcstr :string))

(defun ephemeris-time-to-utc (et &key (format "C") (prec 3) (lenout 25))
  (with-foreign-object (utcstr :char lenout)
    (et2utc et (string-upcase format) prec lenout utcstr)
    (assert (not (failed)))
    (convert-from-foreign utcstr :string)))

(defcfun ("deltet_c" deltet) :void
  ;; Input
  (epoch :double)
  (eptype :string)
  ;; Output
  (delta (:pointer :double)))

(defun delta-et (epoch &key (eptype "UTC"))
  (with-foreign-object (delta :double)
    (deltet epoch (string-upcase eptype) delta)
    (assert (not (failed)))
    (mem-ref delta :double)))

(defcfun ("j2000_c" j2000-jd) :double)  

(defcfun ("unitim_c" unitim) :double
  ;; Input
  (epoch :double)
  (insys :string)
  (outsys :string))

(defun unit-time (epoch in-system out-system)
  "Convert numeric time units given epoch, input system, and output system. Require LSK loaded."
  (unitim epoch (string-upcase in-system) (string-upcase out-system)))

(defcfun ("spd_c" seconds-per-day) :double)

;;; Positions of spacecraft & natural bodies

(defcfun ("spkezr_c" spkezr) :void
  ;; Input
  (targ :string)
  (et :double)
  (ref :string)
  (abcorr :string)
  (obs :string)
  ;; Output
  (starg :pointer)
  (lt :pointer))

(defun spk-ezr (target epht observer &key (ref :j2000) (abcorr :none))
  "State of target at ephemeris time relative to observer in specified
reference frame & optionally with aberration correction"
  (with-foreign-objects ((target-state :double 6)
			 (light-time :double))
    (spkezr (string-upcase target) epht (string-upcase ref) (string-upcase abcorr) (string-upcase observer) target-state light-time)
    (assert (not (failed)))
    (values
     (to-lisp-vector target-state :double 6)
     (mem-ref light-time :double))))

(defcfun ("spkpos_c" spkpos) :void
  ;; Input
  (targ :string)
  (et :double)
  (ref :string)
  (abcorr :string)
  (obs :string)
  ;; Output
  (ptarg :pointer)
  (lt :pointer))

(defun spk-pos (target epht observer &key (ref :j2000) (abcorr :none))
  (with-foreign-objects ((target-position :double 3)
			 (light-time :double))
    (spkpos (string-upcase target) epht (string-upcase ref) (string-upcase abcorr) (string-upcase observer) target-position light-time)
    (assert (not (failed)))
    (values
     (to-lisp-vector target-position :double 3)
     (mem-ref light-time :double))))

;;; Planetary constants

(defcfun ("bodvrd_c" bodvrd) :void
  ;; Inputs
  (bodynm :string)
  (item :string)
  (maxn :int)
  ;; Outputs
  (dim (:pointer :int))
  (values (:pointer :double)))

(defun body-vrd (bodynm item maxn)
  (with-foreign-objects ((dim :int)
			 (values :double maxn))
    (bodvrd (string-upcase bodynm) (string-upcase item) maxn dim values)
    (assert (not (failed)))
    (values (to-lisp-vector values :double maxn) (mem-ref dim :int))))

;; Reference frame conversion

(defcfun ("pxform_c" pxform) :void
  ;; Inputs
  (from :string)
  (to :string)
  (et :double)
  ;; Output
  (rotate :pointer))

(defun px-form (from to &optional (et 0d0))
  (with-foreign-object (rot :double 9)
    (pxform (string-upcase from) (string-upcase to) et rot)
    (let ((m (make-array '(3 3) :element-type 'double)))
      (dotimes (i 9)
	(setf (row-major-aref m i) (mem-aref rot :double i)))
      m)))
