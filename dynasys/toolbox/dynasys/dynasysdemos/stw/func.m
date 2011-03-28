      function []= func(ndim,u,icp,par,ijac,f,dfdu,dfdp) 
%                     
% 
% 
       period=par(11);
       call ffff(ndim,u,icp,par,ijac,f,dfdu);
       f(1)=period*f(1);
       f(2)=period*f(2);
% 
      return 
