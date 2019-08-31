cl-spice
========

Common Lisp interface to JPL's SPICE toolkit for accessing orbit and
other information on celestial bodies and spacecraft.

* [cl-spice](https://github.com/bld/cl-spice)
* [SPICE](http://naif.jpl.nasa.gov/naif/index.html)

Compile CSPICE as a dynamic library
-----------------------------------

To use the C version of JPL's SPICE library in Common Lisp, it needs
to be compiled as a dynamic library. By default, CSPICE compiles as a
static library. The "cspice" directory contains alternative build
scripts for Linux (mkprodct.csh) and Windows (mkprodct_dll.bat and
cspice.def). These files need to be copied to the "src/cspice"
subdirectory of the CSPICE code.

### Linux

* Download the C version of SPICE for Linux (cspice.tar.Z) in either
  32 or 64 bit format (depending on the version of Lisp you are
  using). Unpack the source file where you want to compile it.
* Install the C shell (csh), which CSPICE uses for its build scripts.
* Copy "cspice/mkprodct.csh" to the "src/cspice" sub-directory of the
  CSPICE code, replacing the existing script.
* Compile CSPICE and its utilities by executing "makeall.csh" from the
  CSPICE code directory, or just the library by executing
  "mkprodct.csh" from the "src/cspice" sub-directory.
* Change to the "lib/" sub-directory and run "sudo ldconfig &#96;pwd&#96;"
  to add the dynamic library to the dynamic load cache.

### Windows

Here is how I was able to compile a .DLL of CSPICE for Windows.
* Download C++ compiler, MS Visual Studio Community 2015
  - https://www.visualstudio.com/ "Download Community 2015"
* Install Visual Studio 2015
  - Custom installation -> Select additional compilers -> C++
* Download the C version of SPICE for "Windows, MS Visual C"
  (cspice.zip) in either 32 or 64 bit, depending on your version of
  Lisp. Unpack the archive where you want to compile it.
* Copy "cspice/mkprodct_dll.bat" and "cspice/cspice.def" into the
  "src/cspice" sub-directory of the CSPICE code.
* Run the command line environment for compiling
  - Start
    - All Programs
      - Visual Studio 2015
        - Visual Studio Tools
          - Windows Desktop Command Prompts
            - VS2015 x64 Native Tools Command Prompt (for 64 bit)
            - VS2015 x64 Native Tools Command Prompt (for 32 bit)
* "cd" to the directory where you unpacked cspice.zip, and the
  "src/cspice" sub-directory.
* Run "mkprodct_dll.bat" to compile, which will create the "cspice.dll" file.
* Copy "cspice.dll" to the location needed. For CL-SPICE, this is the
  "lib" subdirectory. 
* Rename "cspice.dll" to "libcspice.dll" for correct calling by CL-SPICE.

Getting SPICE kernels
---------------------

The SPICE library (and cl-spice) use data kernels for orbit, physical,
and other data. Pre-packaged kernels containing data on solar system
bodies and flight missions are available from:

[http://naif.jpl.nasa.gov/naif/data.html](http://naif.jpl.nasa.gov/naif/data.html)

You can generate your own SPK kernels for specific objects and dates
using the telnet interface to the JPL HORIZONS system:

[http://ssd.jpl.nasa.gov/?horizons](http://ssd.jpl.nasa.gov/?horizons)

Using cl-spice
--------------

This library creates a thin convenience layer to access the CSPICE API functions. Documentation for those is located at:

[NAIF CSPICE Toolkit Hypertext Documentation](http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/index.html)

### Failure and resetting

#### FAILED

Tests if the loaded kernels are in a failed state. Usage:

	(if (failed)
		(do stuff when failed)
		(do stuff when not failed))

* [FAILED_C documentation](http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/failed_c.html)

#### RESET

Resets currently loaded kernels to non-failed state. This needs to be run if a previous call to a SPICE kernel failed before it can be used again. Usage:

	(reset)

* [RESET_C documentation](http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/reset_c.html)

### Loading kernels

#### WITH-KERNEL
	
Macro to load and unload the named kernel file as a string:

	(with-kernel "/path/to/kernel/file.spk"
		(functions)
		(using)
		(kernel))

#### FURNSH and UNLOAD

Manually load and unload kernels with the **furnsh** and **unload**
functions:

	(furnsh "/path/to/kernel/file.spk")
	(functions)
	(using)
	(kernel)
	(unload "/path/to/kernel/file.spk")

* [FURNSH_C documentation](http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/furnsh_c.html)
* [UNLOAD_C documentation](http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/unload_c.html)

### Kernel information

#### KERNEL-TOTAL

Number of currently loaded kernels. Corresponds to the KTOTAL_C cspice
function. Optionally use :KIND key to specify what kind of kernel
(default: :ALL). See
[C documentation for a list of kernel kinds](http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/ktotal_c.html). Usage:

	(kernel-total)
	(kernel-total :spk)

* [KTOTAL_C documentation](http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/ktotal_c.html)

#### KERNEL-DATA

Data on the nth kernel. Optionally, specify kind of kernel (:KIND,
default :ALL), fill length (:fillen, default 128), and type length
(:typlen, default 32). Usage:

	(kernel-data n)
	(kernel-data n :kind :all :fillen 128 :typlen 32)

* [KDATA_C documentation](http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/kdata_c.html)

#### KERNEL-INFO

Retrieve info on the kernel specified by filename. Optionally specify type length (:typlen, default 32) and source length (:srclen, default 128). Usage:

	(kernel-info "/path/name/of/loaded/file.spk")
	(kernel-info "/path/name/of/loaded/file.spk" :typlen 32 :srclen 128)

* [KINFO_C documentation](http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/kinfo_c.html)

### Position and Velocity

#### SPK-EZR

Given the target name, ephemeris time (seconds past J2000), and
observer, return a vector containing the position and velocity vectors
components. The second return values is the time in seconds light
takes to travel from the target to the observer. Optionally specify
the reference frame (:ref, default :j2000) and aberration correction flag (:abcorr, default :none). Usage:

	(spk-ezr :earth 0d0 :ssb :ref :eclipj2000)

* [SPKEZR_C documentation](http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/spkezr_c.html)

#### SPK-POS

Given the target name, ephemeris time (seconds past J2000), and
observer, return a vector containing the position vector
components. The second return values is the time in seconds light
takes to travel from the target to the observer. Optionally specify
the reference frame (:ref, default :j2000) and aberration correction
flag (:abcorr, default :none). Usage:

	(spk-pos :sun 0d0 :ssb :ref :eclipj2000)

* [SPKEZR_C documentation](http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/spkpos_c.html)

### Body constants

#### BODY-VRD

Retrieve body constant data given the body name, data field, and maximum number of return values. These are stored in Planetary Constants Kernel (PCK) files. Usage:

	(with-kernel "/path/to/pckfile.tpc"
		(body-vrd :earth :gm 1))

* [BODYVRD_C documentation](http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/bodvrd_c.html)
