      function []= func(ndim,u,icp,par,ijac,f,dfdu,dfdp)
%                    
%
%
       a=par(1);
       b=par(2);
       c=par(3);
%
       x=u(1);
       y=u(2);
%
       f(1)= c * ( x - x**3/3 + y );
       f(2)=-( x - a + b*y ) / c;
%
      return
