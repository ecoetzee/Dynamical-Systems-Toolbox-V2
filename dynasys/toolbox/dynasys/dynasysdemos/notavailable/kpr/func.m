      function []= func(ndim,u,icp,par,ijac,f,dfdu,dfdp) 
%                     
% 
% 
        f(1)=par(3) * ( par(2)*u(2) - u(1)**3 + 3.0*u(1) - par(1) );
        f(2)=u(1) - 2*u(2) + u(3);
        f(3)=u(2) - u(3);
%
      if(ijac.eq.0)return;
%
        dfdu(1,1)=par(3)*(-3.0*u(1)**2 + 3.0);
        dfdu(1,2)=par(3)*par(2);
        dfdu(1,3)=0.0;
%
        dfdu(2,1)=1.0;
        dfdu(2,2)=-2.0;
        dfdu(2,3)=1.0;
%
        dfdu(3,1)=0.0;
        dfdu(3,2)=1.0;
        dfdu(3,3)=-1.0;
%
      if(ijac.eq.1)return;
%
        dfdp(1,1)=- par(3);
        dfdp(2,1)=0.d0;
        dfdp(3,1)=0.d0;
%
        dfdp(1,2)=par(3) *u(2);
        dfdp(2,2)=0.d0;
        dfdp(3,2)=0.d0;
%
        dfdp(1,3)=par(2)*u(2) - u(1)**3 + 3.0*u(1) - par(1);
        dfdp(2,3)=0.d0;
        dfdp(3,3)=0.d0;
;
      return
