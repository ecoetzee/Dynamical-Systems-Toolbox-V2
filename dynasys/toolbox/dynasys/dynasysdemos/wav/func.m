      function []= func(ndim,u,icp,par,ijac,f,dfdu,dfdp) 
%                     
% 
% 
       r= u(2)/(par(5)+u(2)) * u(1)/(1+u(1)+par(6)*u(1)*u(1));
       f(1)=-par(1)*( par(4)*r - (par(2)-u(1)) );
       f(2)=-par(1)*( par(4)*r - par(7)*(par(3)-u(2)) );
% 
      return 
