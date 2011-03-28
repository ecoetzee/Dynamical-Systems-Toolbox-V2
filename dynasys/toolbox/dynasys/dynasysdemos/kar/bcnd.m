      function []= bcnd(ndim,par,icp,nbc,u0,u1,f,ijac,dbc)
%                    
%
%
       gamma=par(1);
       finf=-par(2);
%
       c=dsqrt(finf**4 + 4*gamma**2);
       a=dsqrt(c + finf**2) / dsqrt(2.d0);
       b=dsqrt(c - finf**2) / dsqrt(2.d0);
%
       f(1)=u0(1);
       f(2)=u0(2);
       f(3)=u0(4)-1+gamma;
       f(4)=(finf+a)*u1(2) + u1(3) - gamma*u1(4)/a;
       f(5)=a*b**2*u1(2)/gamma + (finf+a)*u1(4) + u1(5);
       f(6)=u1(1)-finf;
%
      return
