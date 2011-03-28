      function []= stpnt(ndim,u,par,t)
%                     
%
      common /frst/ ifrst;
%
      if(ifrst.ne.1)then;
        ifrst=1;
%       set the starting period, wave speed, and radius
        period=0.01;
        c=11.;
        ep1=0.001;
        d = dsqrt(c**2+4);
        par(2)= c;
        par(4)= ep1;
        par(11)= period;
        par(12)= (c-d)/2;
        par(13) =    1./dsqrt(1+par(12)**2);
        par(14)=par(12)/dsqrt(1+par(12)**2);
       end;
%
       c     =par(2);
       ep1   =par(4);
       period=par(11);
       d=dsqrt(c**2+4);
       rmu1= (c-d)/2;
       v11 =  1./dsqrt(1+rmu1**2);
       v12 =rmu1/dsqrt(1+rmu1**2);
%
       u(1)=1-ep1*v11;
       u(2)= -ep1*v12;
%
      return
