You can either compile on Windows or Linux using the compileauto07p 
function. Unfortunately we had a lot of trouble keeping the same 
code for both. My sincere thanks to John Pattinson at the 
Aerospace Engineering department of the University of Bristol, 
for figuring out how to compile on Linux.


The following compilers were used:

1. Windows : Intel Fortran 9.1

2. Linux   : gcc 4.3.1 

The main difference between the two versions is in the definition of the
continuation characters. This seems to be incompatible between the two 
versions.

Make sure that mex is working on your machine, before you try to compile
these files. Try out the yrpimef example first. Is this works, the toolbox
should compile. Unfortunately we have not done extensive testing of which
compilers can be used. Any feedback would be appreciated.