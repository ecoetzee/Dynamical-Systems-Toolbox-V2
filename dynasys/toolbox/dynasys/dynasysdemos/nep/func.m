      function []= func(ndim,u,icp,par,ijac,f,dfdu,dfdp) 
%                     
% 
% 
       f(1) =  u(2);
       f(2) = -par(2)*u(1);
       f(3) = -u(2)**2/2 - par(2);
% 
      return 
