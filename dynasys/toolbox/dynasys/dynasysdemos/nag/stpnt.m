      function []= stpnt(ndim,u,par,t)
%                     
%
      common /frst/ ifrst;
%
%
% the following initialization is done only in the first call
       if(ifrst.ne.1)then;
         ifrst=1;
%        select period and connection (read from the constants file)
         period=100.;
         sign=1.0;
         r = 0.5*dsqrt(2.d0);
         v1= 1./dsqrt(1+r**2);
         v2= r /dsqrt(1+r**2);
         x  = -0.5*period;
         e  = dexp(x/dsqrt(2.d0));
         u0 = e/(1+e);
         v0 = ( e/(1+e)**2 ) / dsqrt(2.d0);
         eps= dsqrt( u0**2 + v0**2 );
         par(1)= 0.5;
         par(2)= 0;
         par(3)= eps;
         par(4)= eps;
         par(5)= r;
         par(6)=-r;
         par(7)= sign*v1;
         par(8)= sign*v2;
         par(9)= -sign*v1;
         par(10)= sign*v2;
         par(11)= period;
         par(12)= sign;
       end;
%
% specify exact solution as starting point :
%
       period=par(11);
       sign  =par(12);
%
       x=period*sign*(t-0.5);
       e=dexp(x/dsqrt(2.d0));
       u(1)=e/(1+e);
       u(2)=sign*e/(1+e)**2/dsqrt(2.d0);
%
      return
