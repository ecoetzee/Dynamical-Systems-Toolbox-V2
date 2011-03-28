      function []= bcnd(ndim,par,icp,nbc,u0,u1,f,ijac,dbc)
%                    
%
      real*8 par(*);
%
       f(1)=u0(1);
       f(2)=u1(1);
%
      return
