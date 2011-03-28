      function []= func(ndim,u,icp,par,ijac,f,dfdu,dfdp)
%                    
%
      real*8 par(*);
%
       u1=u(1);
       u2=u(2);
%
       rl=par(1);
%
       e=cdexp(u1);
       f(1)=u2;
       f(2)=-rl*e;
%
      return
