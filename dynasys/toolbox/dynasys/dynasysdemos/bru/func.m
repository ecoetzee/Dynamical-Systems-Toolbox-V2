      function []= func(ndim,u,icp,par,ijac,f,dfdu,dfdp) 
%                     
% 
% 
        x=u(1);
        y=u(2);
        a=par(1);
        b=par(2);
%
%      *set the nonlinear term
        f(1)= x**2*y - (b+1)*x + a;
        f(2)=-x**2*y + b*x;
% 
      return 
