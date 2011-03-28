      function []= stpnt(ndim,u,par)
%                     
%
%
% initialize the equation parameters
       par(1)=0.;
       par(2)=0.8;
       par(3)=3.0;
%
% initialize the solution (assuming par(1)=0 )
       u(1)=0.;
       u(2)=0.;
%
      return
