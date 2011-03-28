      function []= icnd(ndim,par,icp,nint,u,uold,ufort,upold,fi,ijac,dint)
%                    
%
%
% local
%
      call ffff(ndim,u   ,icp,par,1,f ,dfdu );
      call ffff(ndim,uold,icp,par,0,f0,dgdu0);
%
% the integral phase condition is defined here:
       fi(1)= ( f(1) - f0(1) ) * ( dfdu(1,1)*f(1) + dfdu(1,2)*f(2) );
     *      + ( f(2) - f0(2) ) * ( dfdu(2,1)*f(1) + dfdu(2,2)*f(2) );
%
      return
