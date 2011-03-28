      function []= func(ndim,u,icp,par,ijac,f,dfdu,dfdp) 
%                     
% 
%      
       period=par(11);
%      
       f(1)= period * (par(3) * (u(2)- u(1)));
       f(2)= period * (par(1)*u(1) - u(2) - u(1)*u(3));
       f(3)= period * (u(1)*u(2) -  par(2)*u(3));
% 
      return 
