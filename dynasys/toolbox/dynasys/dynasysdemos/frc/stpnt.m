      function []= stpnt(ndim,u,par,t)  
%                     
% 
% 
       a  =0.5;
       b  = a;
       r  =0.;
       eps=0.005;
       bet=100;
       d  =1.0;
%
       par(1)=a;
       par(2)=b;
       par(3)=r;
       par(4)=eps;
       par(5)=bet;
       par(6)=d;
       tpi=8*datan(1.0d0);
       par(11)=tpi/bet;
%
       u(1)=b;
       u(2)=b*(b-a)*(1-b);
       u(3)=dsin(tpi*t);
       u(4)=dcos(tpi*t);
% 
      return    
