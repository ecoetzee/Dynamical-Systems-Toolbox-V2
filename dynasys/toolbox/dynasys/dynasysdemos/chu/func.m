      function []= func(ndim,u,icp,par,ijac,f,dfdu,dfdp) 
%                     
% 
%
      ab(x)=2*x*datan(rk*x)/pi;
      h(x)=a1*x + 0.5*(a0-a1) * ( ab(x+1) - ab(x-1) );
%
       pi=4*datan(1.d0);
       alpha=par(1);
       beta =par(2);
       rk   =par(3);
       a0   =par(4);
       a1   =par(5);
%
       x=u(1);
       y=u(2);
       z=u(3);
% 
       f(1)= alpha * ( y - h(x) );
       f(2)=  x - y + z;
       f(3)= -beta * y;
% 
      return 
