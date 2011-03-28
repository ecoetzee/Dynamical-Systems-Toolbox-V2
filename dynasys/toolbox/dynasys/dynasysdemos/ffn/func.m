      function []= func(ndim,u,icp,par,ijac,f,dfdu,dfdp) 
%                     
% 
% 
% fhn parameters
       a = par(1);
       b = par(2);
       c = par(3);
%
% forcing parameters
       amp = par(4);
       beta= par(5);
%
% fhn variables
       x=u(1);
       y=u(2);
%
% forcing variables
       sn=u(3);
       cs=u(4);
       ss = sn**2 + cs**2;
% 
% fhn equations
       f(1)= c * ( x - x**3/3 + y - amp*cs );
       f(2)=-( x - a + b*y ) / c;
%
% oscillator
       f(3) =  sn + beta*cs - sn*ss;
       f(4) = -beta*sn + cs - cs*ss;
% 
      return    
