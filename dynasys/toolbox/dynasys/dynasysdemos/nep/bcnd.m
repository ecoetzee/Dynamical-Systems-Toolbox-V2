      function []= bcnd(ndim,par,icp,nbc,u0,u1,fb,ijac,dbc) 
%                     
% 
% 
       fb(1)=u0(1);
       fb(2)=u1(1);
       fb(3)=u0(3) - par(1);
       fb(4)=u1(3) + par(1);
% 
      return 
