      function []= func(ndim,u,icp,par,ijac,f,dfdu,dfdp) 
%                     
% 
% 
       f(1)=-par(1)*u(1) - u(2);
       f(2)= u(1) * (1 - u(1));
% 
      return 
