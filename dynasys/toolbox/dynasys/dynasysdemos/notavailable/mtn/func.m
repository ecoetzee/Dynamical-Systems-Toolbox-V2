      function []= func(ndim,u,icp,par,ijac,f,dfdu,dfdp) 
%                     
% 
% 
      dem=u(2)**2+par(4)**2;
      psiz=0.4d0*u(1)/(0.6d0+u(1));
      psif=par(2)*u(2)**2/dem;
      psizu1=0.4d0*((1/(0.6d0+u(1)))- (u(1)/((0.6d0+u(1))**2)));
      psifu2=2*par(2)*( (u(2)/dem) -(u(2)**3/(dem**2) ));
%
      f(1)=(0.5d0*u(1)*(1.0d0-u(1)/par(1))-u(2)*psiz+par(3)*par(1));
      f(2)=(0.6d0*psiz*u(2)-0.15d0*u(2)-psif);
%
      if(ijac.eq.0)return;
%
        dfdu(1,1)=0.5d0-u(1)/par(1) -u(2)*psizu1;
        dfdu(1,2)=-psiz;
%
        dfdu(2,1)=0.6d0*psizu1*u(2);
        dfdu(2,2)=0.6d0*psiz-0.15d0-psifu2;
%
      if(ijac.eq.1)return;
%
        dfdp(1,1)=par(3);
        dfdp(2,1)=0.d0;
%
        dfdp(1,2)=0.0d0;
        dfdp(2,2)=-u(2)**2/dem;
%
        dfdp(1,3)=par(1);
        dfdp(2,3)=0.d0;
%
        dfdp(1,4)=0.0d0;
        dfdp(2,4)=2*par(4)*par(2)*u(2)**2/dem**2;
%
      return
