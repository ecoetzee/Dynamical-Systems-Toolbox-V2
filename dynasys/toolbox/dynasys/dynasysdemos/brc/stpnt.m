      function []= stpnt(ndim,u,par) 
%                      
% define the starting stationary solution on the spatial mesh
%
      include 'brc.inc';
      parameter ( np=nn+1 );
      common /blppde/ d2(nn,0:np);
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
% set the starting solution at the chebyshev collocation points
        for 1 i=1,nn;
          u(i,1)=a;
          u(i,2)=b/a;
 1      continue;
% 
      return 
