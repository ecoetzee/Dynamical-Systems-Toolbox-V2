**Background**

This is a MATLAB version of AUTO, where we have integrated AUTO into MATLAB via mex functions. 
One of the biggest reasons why Dynamical Systems Theory is not being applied in an engineering
context, is mainly due to the lack of bifurcation software that integrates with relative ease
with existing toolsets. Ample examples are also needed for a person new to the field. We 
therefore tried to address this by incorporating AUTO into MATLAB, and thus the Dynamical 
Sytems Toolbox was built. We hope that it would be useful teaching tool and can help 
popularise the methods amongst the engineering community.

At this stage we are still in the process of adding several engineering examples to the 
toolbox . Feel free to develop some examples for inclusion into the toolbox. There are 
template files that you can use for inclusion of your own examples.

**System Requirements**

1. MATLAB R2009A or higher
2. Intel Visual Fortran 9.1 or higher for compiling on Windows, if needed.
3. GCC 4.3.1 or higher for compiling on Linux.

**Authors**

This toolbox was written by Etienne Coetzee, Phani Thota and James Rankin from the 
University of Bristol. Obviously, credit must also be given to the authors of 
AUTO, Esebius Doedel et al.

**Main features**

1.  Look and feel of MATLAB. 
2.  Extensive use of objects. 
3.  Can be run in the new mode, or still with all the old AUTO files (.c,.7,.8.9) familiar 
    to the user. 
4.  Robust error checking. 
5.  Additional outputs can be detected and also passed out to MATLAB variables. 
6.  Any of the MATLAB toolsets can be used, i.e. the Symbolic toolbox, Simulink etc. 
7.  Similar notations to that of AUTO. A person familiar with AUTO should find it 
    straightforward to pick up the new toolsets. 
8.  Also works with the student version of MATLAB. 
9.  Ample documentation. 
10. Templates files for people willing to contribute their own examples for inclusion 
    into the demos. 

**Drawbacks**

1.  Limit Cycles are at least an order of magnitude slower. We had to make a trade-off 
    between robustness and speed. We therefore decided that if we want to popularise the methods, 
    then the code should work, and people should not have to struggle with decoding it. 
2.  No ample enginering examples yet.

**Installation Instructions**

To install the toolbox follow these steps:

1.  Download the toolbox and unzip.
2.  Open MATLAB and change to the directory where the toolbox was unzipped.
3.  Run the program  installdynasys.m; A user interface will appear.
4.  If you have admin rights keep the default values and install. The toolbox will be 
    installed in the MATLAB installation directory.
5.  If you do not have admin rights, install the toolbox to a directory where you have 
    access rights. A startup.m file will be created in this directory.
6.  Close MATLAB, and restart.
7.  Type dynasysroot and dynasyshelproot at the command line. If these commands are 
    working, it should indicate where the toolbox components were installed. If not, 
    something has gone wrong. Check that the paths are correctly defined.
8.  The Dynamical Systems Toolbox should appear on the menu. If not, either the paths 
    were not defined correctly, or the info.xml file in the **$dynasysroot/toolbox/dynasys** 
    directory has the wrong information on line 10. Add the correct path to the documentation 
    directory **$dynasysdocroot/toolbox/dynasys**.
9.  Close MATLAB and restart.

**Restrictions**

1.  We have only managed to compile on Windows with Intel Fortran 9.1 or higher. Also now 
    possible to use on Linux with gcc 4.3 or higher.
2.  I am not able to make many updates because I am trying to finish my PhD, hence assume 
    that the software will not be frequently updated.

**Workshop Presentations**

1.  Background information.  [[http://seis.bris.ac.uk/~ec1099/DST_Workshop_Part1.pdf]]
2.  Getting started.  [[http://seis.bris.ac.uk/~ec1099/DST_Workshop_Part2.pdf]]

