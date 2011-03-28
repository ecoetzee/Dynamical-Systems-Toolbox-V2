      function []= ff(ne,u,par,f) 
%                   
%     define the nonlinear term
%
% 
        x=u(1);
        y=u(2);
        a=par(1);
        b=par(2);
%
        f(1)= x**2*y - (b+1)*x + a;
        f(2)=-x**2*y + b*x;
% 
      return 
