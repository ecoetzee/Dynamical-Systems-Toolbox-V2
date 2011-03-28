      function []= icnd(ndim,par,icp,nint,u,uold,ufort,upold,fi,ijac,dint) 
%                     
% 
% 
       call func(ndim,u,icp,par,0,ff,dfdu,dfdp);
       fi(1)=sqrt(ff(1)**2 + ff(2)**2 + ff(3)**2 ) - par(12);
%
      return 
