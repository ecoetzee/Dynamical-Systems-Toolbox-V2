      function []= stpnt(ndim,u,par,t)  
%                     
% 
% 
% fhn parameters
       a = 0.;
       b = 0.8;
       c = 3.;
%
       par(1)=a;
       par(2)=b;
       par(3)=c;
%
% forcing parameters
       amp=0.;
       beta=10;
%
       par(4)=amp;
       par(5)=beta;
%
       tpi=8*datan(1.d0);
       par(11)=tpi/beta;
%
%      initial fhb stationary solution (assuming a=0)
       u(1)= 0.;
       u(2)= 0.;
%
% initialize the oscillator
       u(3)=dsin(tpi*t);
       u(4)=dcos(tpi*t);
% 
      return    
