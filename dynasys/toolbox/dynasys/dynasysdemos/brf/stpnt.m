      function []= stpnt(ndim,u,par) 
%                      
% define the starting stationary solution on the spatial mesh
%
      include 'brf.inc';
      parameter (nn=nx-1);
      common /blppde/ d0(nn,nn),d2(nn,nn),di(nn,nn),dd(nn,nn),;
     *          ri(nn,nn);
%
% set the parameter values
        a=2.d0;
        b=5.45d0;
        dx=0.008d0;
        dy=0.004d0;
        rl=0.4;
%
        par(1)=a;
        par(2)=b;
        par(3)=dx;
        par(4)=dy;
        par(5)=rl;
%
% set the starting solution at space points i/nx, i=1,2,...,nx 1
        for 1 i=1,nx-1;
          u(i,1)=a;
          u(i,2)=b/a;
 1      continue;
% 
      return 
