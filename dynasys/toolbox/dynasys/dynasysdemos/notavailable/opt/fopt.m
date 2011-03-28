      function []= fopt(ndim,u,icp,par,ijac,fs,dfdu,dfdp) 
%                     
% 
% 
       x1=u(1);
       x2=par(1);
       x3=par(2);
       x4=par(3);
       x5=par(4);
% 
       fs=x1 + x2 + x3 + x4 + x5;
% 
      if(ijac.eq.0)return;
% 
       dfdu(1)=1.0;
% 
      if(ijac.eq.1)return;
%
%      *parameter derivatives
       dfdp(1)=1.0;
       dfdp(2)=1.0;
       dfdp(3)=1.0;
       dfdp(4)=1.0;
% 
      return 
