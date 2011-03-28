      function []= func(ndim,u,icp,par,ijac,f,dfdu,dfdp)
%                    
%
%
       call ffff(2,u,icp,par,ijac,f,dummy);
       period=par(11);
       for 1 i=1,ndim;
         f(i)=period*f(i);
 1     continue;
%
      return
