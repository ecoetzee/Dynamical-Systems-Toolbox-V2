      function []= func(ndim,u,icp,par,ijac,f,dfdu,dfdp)
%                    
%
%
       u1=u(1);
       u2=u(2);
       u3=u(3);
       u4=u(4);
       u5=u(5);
%
       gamma=par(1);
       zinf =par(3);
%
       f(1)=zinf*u2;
       f(2)=zinf*u3;
       f(3)=zinf*( - 2*gamma*u4 + u2*u2 - 2*u1*u3 - u4*u4);
       f(4)=zinf*u5;
       f(5)=zinf*(2*gamma*u2 + 2*u2*u4 - 2*u1*u5);
%
      return
