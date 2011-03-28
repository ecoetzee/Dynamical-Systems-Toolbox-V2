      function []= bcnd(ndim,par,icp,nbc,u0,u1,fb,ijac,dbc) 
%                     
% 
% 
       fb(1)=u0(1)-1.5;
       fb(2)=u1(1)-par(1);
% 
      return 
