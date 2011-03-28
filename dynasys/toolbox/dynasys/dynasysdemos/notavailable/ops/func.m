      function []= func(ndim,u,icp,par,ijac,f,dfdu,dfdp) 
%                     
%
%
       x=u(1);
       y=u(2);
       z=u(3);
%
       p1=par(1);
       p2=par(2);
       p3=par(3);
       p4=par(4);
% 
       f(1)=( -p4*(x**3/3-x) + (z-x)/p2 - y ) / p1;
       f(2)=x-p3;
       f(3)=-(z-x)/p2;
%     
      if(ijac.eq.0)return;
%
       dfdu(1,1)=( -p4*(x**2-1) - 1/p2 ) /p1;
       dfdu(1,2)=-1/p1;
       dfdu(1,3)=1/(p2*p1);
%
       dfdu(2,1)=1;
       dfdu(2,2)=0;
       dfdu(2,3)=0;
%
       dfdu(3,1)=1/p2;
       dfdu(3,2)=0;
       dfdu(3,3)=-1/p2;
%     
      if(ijac.eq.1)return;
%
%      *parameter derivatives
       for 2 i=1,3;
         for 1 j=1,9;
           dfdp(i,j)=0.d0;
 1       continue;
 2     continue;
%
       dfdp(1,1)=-( -p4*(x**3/3-x) + (z-x)/p2 - y )/p1**2;
       dfdp(1,2)=-(z-x)/(p2**2*p1);
       dfdp(1,3)=0;
       dfdp(1,4)=-(x**3/3-x)/p1;
%
       dfdp(2,1)=0;
       dfdp(2,2)=0;
       dfdp(2,3)=-1;
       dfdp(2,4)=0;
%
       dfdp(3,1)=0;
       dfdp(3,2)=(z-x)/p2**2;
       dfdp(3,3)=0;
       dfdp(3,4)=0;
% 
      return 
