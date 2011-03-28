      function []= func(ndim,u,icp,par,ijac,f,dfdu,dfdp) 
%                     
% 
% 
      f(1)= par(1)*u(1) + par(2)*u(2) - par(1)*u(1)*u(1);
     1    + (par(8) - par(4)*u(3)) * u(1)*(2.0d0 - 3.0d0*u(1));
      f(2)= par(2)*u(1) + par(1)*u(2);
     1    - 1.5d0*par(2)*u(1)*u(1) - 1.5d0*par(1)*u(1)*u(2);
     2    - (par(8) - par(4)*u(3)) * 2.0d0*u(2);
      f(3)= par(3)*u(3) + par(7)*u(1) + par(6)*u(1)*u(3);
     1   + par(4)*par(5)*(u(1)*u(1)*(1.0d0-u(1))-u(2)*u(2));
%
      if(ijac.eq.0)return;
%
      dfdu(1,1)= par(1) - 2.0d0*par(1)*u(1);
     1    + (par(8)-par(4)*u(3)) * (2.0d0-6.0d0*u(1));
      dfdu(1,2)= par(2);
      dfdu(1,3)= - par(4) * u(1)*(2.0d0-3.0d0*u(1));
%     
      dfdu(2,1)= par(2) - 3.0d0*par(2)*u(1) - 1.5d0*par(1)*u(2);
      dfdu(2,2)= par(1) - 1.5d0*par(1)*u(1);
     1    - (par(8)-par(4)*u(3)) * 2.0d0;
      dfdu(2,3)= 2.0d0*par(4)*u(2);
%
      dfdu(3,1)= par(7) + par(6)*u(3);
     1    + par(4)*par(5) * u(1)*(2.0d0-3.0d0*u(1));
      dfdu(3,2)= -2.0d0*par(4)*par(5) * u(2);
      dfdu(3,3)= par(3) + par(6)*u(1);
%
      if(ijac.eq.1)return;
%
% no parameter derivatives are specified with this example
%
      return
