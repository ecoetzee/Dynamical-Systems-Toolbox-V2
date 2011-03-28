      function []= ffff(ndm,u,icp,par,ijac,f,dfdu)
%                    
%
%
       c=par(2);
       f(1)= u(2);
       f(2)= c*u(2) - u(1) * (1-u(1));
%
      if(ijac.eq.0)return;
%
       dfdu(1,1)= 0;
       dfdu(1,2)= 1;
%
       dfdu(2,1)= -1 + 2*u(1);
       dfdu(2,2)= c;
%
      return
