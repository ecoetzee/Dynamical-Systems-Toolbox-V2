      function []= ffff(ndim,u,icp,par,ijac,f,dfdu)
%                    
%
% the reduced system for traveling waves is defined here. a separate subroutine 
% is used because the system and the jacobian are also needed in the subroutines
% bcnd and icnd below. the computation should be done with jac=0. the derivatives
% below are for use in bcnd and icnd only. 
%
%
% a0, a1, a2:
       par(2)= (1-par(1))* 2   + par(1)* 2;
       par(3)= (1-par(1))* 0   + par(1)* 1;
%
% b0, b1, b2:
       par(4)= (1-par(1))* 2   + par(1)* 0;
       par(5)= (1-par(1))* 0   + par(1)* 1;
       par(6)= (1-par(1))* 0   + par(1)* 0;
%
% c0, c1, c2:
       par(7)= (1-par(1))* 0   + par(1)* 0;
       par(8)= (1-par(1))* 1   + par(1)* 1;
       par(9)= (1-par(1))*(-1) + par(1)*(-1);
%
      fa     =          par(2)*u(1) + par(3)*u(1)**2;
      fb     = par(4) + par(5)*u(1) + par(6)*u(1)**2;
      fc     = par(7) + par(8)*u(1) + par(9)*u(1)**2;
%
      c      = par(10);
%
       f(1)= fa * u(2);
       f(2)= -c * u(2)  -  fb * u(2)**2  -  fc;
%
      if(ijac.eq.0)return;
%
      dfa    = par(2) + 2*par(3)*u(1);
      dfb    = par(5) + 2*par(6)*u(1);
      dfc    = par(8) + 2*par(9)*u(1);
%
       dfdu(1,1)= dfa*u(2);
       dfdu(1,2)= fa;
%
       dfdu(2,1)= -dfb*u(2)**2 - dfc;
       dfdu(2,2)= -c - 2*fb*u(2);
%
      return
