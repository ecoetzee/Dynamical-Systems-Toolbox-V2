      function []= func(ndim,u,icp,par,ijac,f,dfdu,dfdp) 
%                     
% 
% 
       x1=u(1);
       x2=par(1);
       x3=par(2);
       x4=par(3);
       x5=par(4);
% 
       f(1)=x1*x1 + x2*x2 + x3*x3 + x4*x4 + x5*x5 - 1;
% 
      if(ijac.eq.0)return;
% 
       dfdu(1,1)=2*x1;
% 
      if(ijac.eq.1)return;
%
%      *parameter derivatives
       dfdp(1,1)=2*x2;
       dfdp(1,2)=2*x3;
       dfdp(1,3)=2*x4;
       dfdp(1,4)=2*x5;
% 
      return 
