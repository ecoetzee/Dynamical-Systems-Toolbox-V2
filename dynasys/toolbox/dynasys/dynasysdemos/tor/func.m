      function []= func(ndim,u,icp,par,ijac,f,dfdu,dfdp) 
%                     
% 
%
       rn=par(1);
       be=par(2);
       ga=par(3);
       r =par(4);
       a3=par(5);
       b3=par(6);
%
       x=u(1);
       y=u(2);
       z=u(3);
%
       f(1)= ( -(be+rn)*x + be*y - a3*x**3 + b3*(y-x)**3 )/r;
       f(2)=  be*x - (be+ga)*y - z - b3*(y-x)**3;
       f(3)= y;
%
       if(ijac.eq.0)return;
% 
       dfdu(1,1)=( -(be+rn) -3*a3*x**2 - 3*b3*(y-x)**2  )/r;
       dfdu(1,2)=( be + 3*b3*(y-x)**2 )/r;
       dfdu(1,3)=0;
% 
       dfdu(2,1)=be + 3*b3*(y-x)**2;
       dfdu(2,2)=-(be+ga) - 3*b3*(y-x)**2;
       dfdu(2,3)=-1;
% 
       dfdu(3,1)=0;
       dfdu(3,2)=1;
       dfdu(3,3)=0;
% 
      if(ijac.eq.1)return;
%
%      *parameter derivatives
       dfdp(1,1)=-x/r;
       dfdp(2,1)=0;
       dfdp(3,1)=0;
%
       dfdp(1,2)=( -x + y )/r;
       dfdp(2,2)=x-y;
       dfdp(3,2)=0;
%
       dfdp(1,3)=0;
       dfdp(2,3)=-y;
       dfdp(3,3)=0;
%
       dfdp(1,4)=-f(1)/r;
       dfdp(2,4)=0;
       dfdp(3,4)=0;
%
       dfdp(1,5)=x**3/r;
       dfdp(2,5)=0;
       dfdp(3,5)=0;
%
       dfdp(1,6)=(y-x)**3 / r;
       dfdp(2,6)=-(y-x)**3;
       dfdp(3,6)=0;
%
      return 
