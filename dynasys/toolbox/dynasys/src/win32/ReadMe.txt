========================================================================
    Fortran Console Application : "AUTO07" Project Overview
========================================================================

The Intel Fortran Console Application Wizard has created this 
"AUTO07" project for you as a starting point.

This file contains a summary of what you will find in each of the files 
that make up your project.

AUTO07.vfproj
    This is the main project file for Fortran projects generated using an 
    Application Wizard.  It contains information about the version of 
    Intel Fortran that generated the file, and information about the 
    platforms, configurations, and project features selected with the 
    Application Wizard.

/////////////////////////////////////////////////////////////////////////////
Other notes:

FILE SETUP
----------
The following files need to reside in the directory where compilation 
takes place.

Make sure you have the following files included as "Source" files


AUTO07gateway.F     : mex gateway function that calls AUTO                                                                        
autlib1.F           : main program file for AUTO                                           
autlib2.F                                               
autlib3.F                                                
autlib4.F                                                      
autlib5.F                          
blas.F   
compat.F
eispack.F
func.F				: equations file such as "ab.f" renamed to "func.F"
nompi.F

Make sure you have the following files included as "Header" files

auto.h 
config.h
fcon.h 

COMPILATION
-----------
Choose the following menu option to compile the files

Build -> Build AUTO07 

RUNNING AUTO
------------
Change to the "Debug" directory and make sure the the continuation
settings file resides in this directory. Rename the continuation settings
file from e.g. "c.ab" to "fort.2"

Run AUTO07 by double clicking on it, or running it from the command line
with 

> AUTO07

A set of files will appear with the continuation results

fort.7
fort.8
fort.9



/////////////////////////////////////////////////////////////////////////////
