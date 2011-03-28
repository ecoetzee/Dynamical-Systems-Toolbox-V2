      function []= bcnd(ndim,par,icp,nbc,u0,u1,fb,ijac,dbc)
%                    
%
%
       fb(1)=u0(1);
       fb(2)=u1(1);
       fb(3)=u0(3)-par(13);
       fb(4)=u0(4);
       fb(5)=u1(3)+par(14);
       fb(6)=u1(4);
%
      return
