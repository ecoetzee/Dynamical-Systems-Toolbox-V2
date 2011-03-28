      function []= func(ndim,u,icp,par,ijac,f,dfdu,dfdp) 
%                     
% 
% 
       fhn(z) = z*(z-a)*(1-z);
% 
       a  = par(1);
       b  = par(2);
       r  = par(3);
       eps= par(4);
       bet= par(5);
       d  = par(6);
%
       v=u(1);
       w=u(2);
       x=u(3);
       y=u(4);
       ss = x**2 + y**2;
% 
       f(1) = ( fhn(v) - w )/eps;
       f(2) = v - d*w - ( b + r*x );
       f(3) =  x + bet*y - x*ss;
       f(4) = -bet*x + y - y*ss;
% 
      return    
