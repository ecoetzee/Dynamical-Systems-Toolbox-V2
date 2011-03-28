      function []= icnd(ndim,par,icp,nint,u,uold,ufort,upold,fi,ijac,dint)
%                    
%
%
       p=u(1) + par(2)*u(1)**2 + par(3)*u(1)**4;
       e=exp(p);
%
       fi(1)=u(3)**2 - par(16);
       fi(2)=par(10)-(u(1)-1.0)**2;
     *         - 0.1*( par(1)**2+par(2)**2+par(3)**2 );
       fi(3)=-e*u(4)-par(15)*0.2*par(1);
         if(nint.eq.3)return;
       fi(4)=-par(1)*e*u(1)**2*u(4)-par(15)*0.2*par(2) - par(17);
         if(nint.eq.4)return;
       fi(5)=-par(1)*e*u(1)**4*u(4)-par(15)*0.2*par(3) - par(18);
%
      return
