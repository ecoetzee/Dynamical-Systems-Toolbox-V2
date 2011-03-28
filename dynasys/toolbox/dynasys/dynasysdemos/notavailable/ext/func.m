      function []= func(ndim,u,icp,par,ijac,f,dfdu,dfdp)
%                    
%
%
       p= u(1) + u(1)**2 + u(1)**3;
       s=dsin(p);
       pi=4*datan(1.d0);
       q=(par(1)*pi)**2;
%
       f(1)=u(2);
       f(2)=-q*s;
%
      return
