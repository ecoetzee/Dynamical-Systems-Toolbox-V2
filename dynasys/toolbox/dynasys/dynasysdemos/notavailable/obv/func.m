      function []= func(ndim,u,icp,par,ijac,f,dfdu,dfdp)
%                    
%
%
       p=u(1) + par(2)*u(1)**2 + par(3)*u(1)**4;
       e=exp(p);
%
       f(1)= u(2);
       f(2)=-par(1)*e;
       f(3)= par(1)*e*(1+2*par(2)*u(1)+4*par(3)*u(1)**3 )*u(4);
     *                       + par(15)*2*(u(1)-1.0);
       f(4)=-u(3);
%
      return
