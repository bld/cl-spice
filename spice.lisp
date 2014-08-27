;;; Foreign function interface to JPL's SPICE library

(ql:quickload :cffi)

(defpackage :cl-spice
  (:use :cl :cffi))

(in-package :cl-spice)

(define-foreign-library libcspice
  (t (:default "cspice")))

(use-foreign-library libcspice)

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
  (with-foreign-string (kind-str (string kind))
    (with-foreign-object (count :int)
      (ktotal kind-str count)
      (mem-ref count :int))))

(defcfun ("kdata_c" kdata) :void
  ;; Input
  (which :int)
  (kind :pointer)
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
  (with-foreign-string (kind-str (string kind))
    (with-foreign-objects
	((file :char fillen)
	 (filtyp :char typlen)
	 (source :char srclen)
	 (handle :int)
	 (found :int))
      (kdata which kind-str fillen typlen srclen file filtyp source handle found)
      (values
       (convert-from-foreign file :string)
       (convert-from-foreign filtyp :string)
       (convert-from-foreign source :string)
       (mem-ref handle :int)
       (mem-ref found :int)))))

(defcfun ("kinfo_c" kinfo) :void
  ;; Input
  (file :pointer)
  (typlen :int)
  (srclen :int)
  ;; Output
  (filtyp :pointer)
  (source :pointer)
  (handle :pointer)
  (found :pointer))

(defun kernel-info (file &key (typlen 32) (srclen 128))
  "Retrieve info of specified loaded kernel file"
  (with-foreign-string (file-str file)
    (with-foreign-objects
	((filtyp :char typlen)
	 (source :char srclen)
	 (handle :int)
	 (found :int))
      (kinfo file-str typlen srclen filtyp source handle found)
      (values
       (convert-from-foreign filtyp :string)
       (convert-from-foreign source :string)
       (mem-ref handle :int)
       (not (zerop (mem-ref found :int)))))))

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
		(unload ,filename) ; unload if it wasn't loaded already
		(apply #'values ,retval))) ; return multiple values if present
	 ;; make sure kernel unloads at the end if it wasn't before
	 (unless ,found (unload ,filename))))))

;;; Time & date

(defcfun ("str2et_c" str2et) :void
  (str :string)
  (et :pointer))

(defun string-to-ephemeris-time (string)
  "Date string to ephemeris time. See the following for string format:
http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/str2et_c.html"
  (if (> (kernel-total :text) 0)
      (with-foreign-object (et :double)
	(str2et string et)
	(mem-ref et :double))
      (warn "No TEXT kernel loaded")))

(defcfun ("timout_c" timout) :void
  (et :double)
  (pictur :string)
  (lenout :int)
  (output :string))

(defun ephemeris-time-to-output (et &key (picture "YYYY Mon DD, HR:MN:SC ::UTC") (lenout 128))
  (with-foreign-object (output :char lenout)
    (timout et picture lenout output)
    (convert-from-foreign output :string)))

;;; Positions of spacecraft & natural bodies
