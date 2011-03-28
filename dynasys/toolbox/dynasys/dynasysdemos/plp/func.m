      function []= func(ndim,u,icp,par,ijac,f,dfdu,dfdp)
%                    
%
%
       s=u(1);
       a=u(2);
%
       s0=par(1);
       a0=par(2);
       al=par(3);
       rh=par(4);
       rk=par(5);
%
       d=1+s+rk*s**2;
       r=s*a/d;
%
       f(1)=   (s0-s) - rh*r;
       f(2)=al*(a0-a) - rh*r;
%
       if(ijac.eq.0)return;
%
       drds=( a*d - s*a*(1+2*rk*s) ) / d**2;
       drda=s/d;
       drdk=-s**3*a/d**2;
%
       dfdu(1,1)=-1 - rh*drds;
       dfdu(1,2)=   - rh*drda;
       dfdu(2,1)=   - rh*drds;
       dfdu(2,2)=-al- rh*drda;
% 
      if(ijac.eq.1)return;
%
%      *parameter derivatives
%
       dfdp(1,1)=1;
       dfdp(1,2)=0;
       dfdp(1,3)=0;
       dfdp(1,4)=-r;
       dfdp(1,5)=-rh*drdk;
%
       dfdp(2,1)=0;
       dfdp(2,2)=al;
       dfdp(2,3)=a0-a;
       dfdp(2,4)=-r;
       dfdp(2,5)=-rh*drdk;
%
      return
