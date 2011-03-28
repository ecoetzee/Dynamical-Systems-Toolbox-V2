      function []= func(ndim,u,icp,par,ijac,f,dfdu,dfdp) 
%                     
% 
%
       p=par(1);
%
       f(1)= u(2);
       f(2)= u(3);
       f(3)= u(4);
       f(4)= -p*u(3)-u(1)+u(1)**3;
%
       if(ijac.eq.0)return;
% 
       dfdu(1,1)=0.0d0;
       dfdu(1,2)=1.0d0;
       dfdu(1,3)=0.0d0;
       dfdu(1,4)=0.0d0;
% 
       dfdu(2,1)=0.0d0;
       dfdu(2,2)=0.0d0;
       dfdu(2,3)=1.0d0;
       dfdu(2,4)=0.0d0;
% 
       dfdu(3,1)=0.0d0;
       dfdu(3,2)=0.0d0;
       dfdu(3,3)=0.0d0;
       dfdu(3,4)=1.0d0;
% 
       dfdu(4,1)=-1.0+3.0d0*u(1)**2;
       dfdu(4,2)=0.0d0;
       dfdu(4,3)=-p;
       dfdu(4,4)=0.0d0;
% 
      if(ijac.eq.1)return;
%
%      *parameter derivatives
       dfdp(1,1)=0.0d0;
       dfdp(2,1)=0.0d0;
       dfdp(3,1)=0.0d0;
       dfdp(4,1)=-u(3);
%
      return 
