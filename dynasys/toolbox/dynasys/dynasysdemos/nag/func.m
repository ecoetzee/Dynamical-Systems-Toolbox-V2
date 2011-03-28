      function []= func(ndim,u,icp,par,ijac,f,dfdu,dfdp)
%                    
%
%
%
       call ffff(2,u,icp,par,0,f,dummy);
%
       period=par(11);
       f(1)=period*f(1);
       f(2)=period*f(2);
%
      return
