      function []= stpnt(ndim,u,par,x) 
%                      
% 
%
       gamma=1.2;
       eps=0.1;
       rl=0.;
%
       par(1)=gamma;
       par(2)=eps;
       par(3)=rl;
%
       s=gamma-1.5;
       u(1)=1.5 + s*x;
       u(2)=s;
% 
      return 
