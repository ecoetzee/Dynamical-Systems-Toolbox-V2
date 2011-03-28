      function []= func(ndim,u,icp,par,ijac,f,dfdu,dfdp)
%                    
%
%
       gamma = par(1);
       eps   = par(2);
       ri    = par(3);
%
       ph1=u(1);
       ph2=u(2);
       ps1=u(3);
       ps2=u(4);
%
       f(1)= ps1;
       f(2)= ps2;
       f(3)= -eps*ps1 - sin(ph1) + ri + gamma*(ph2-ph1);
       f(4)= -eps*ps2 - sin(ph2) + ri + gamma*(ph1-ph2);
%
      return
