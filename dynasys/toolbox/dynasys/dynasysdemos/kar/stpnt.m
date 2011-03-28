      function []= stpnt(ndim,u,par,t)
%                     
%
%
       gamma=1.;
       finf =0.;
       zinf =500.;
%
       par(1)=gamma;
       par(2)=-finf;
       par(3)=zinf;
%
       u(1)=0.;
       u(2)=0.;
       u(3)=0.;
       u(4)=0.;
       u(5)=0.;
%
      return
