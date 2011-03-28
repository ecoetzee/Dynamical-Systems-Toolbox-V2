      function []= func(ndim,u,icp,par,ijac,f,dfdu,dfdp) 
%                     
% 
%
       u1=u(1);
       u2=u(2);
       x =u(3);
       p=par(1);
% 
       f(1)=u2;
       f(2)=-p*dexp(x**3*u1);
       f(3)=1.d0;
% 
      return 
