      function []= func(ndim,u,icp,par,ijac,f,dfdu,dfdp) 
%                     
% 
% 
      f(1)= u(1)*(1-u(1)) - par(4)*u(1)*u(2);
      f(2)=-par(2)*u(2)   + par(4)*u(1)*u(2) - par(5)*u(2)*u(3);
     *                    - par(1)*(1-exp(-par(6)*u(2)));
      f(3)=-par(3)*u(3)   + par(5)*u(2)*u(3);
% 
      return 
