      function []= func(ndim,u,icp,par,ijac,f,dfdu,dfdp) 
%                     
% 
% 
      f(1)= u(2);
      f(2)= par(1)*u(2)+u(1)*(u(1)-par(2))*(u(1)-1.0)+u(3);
      f(3)= par(3)*u(1)/par(1);
%
      if(ijac.eq.0)return;
%
      dfdu(1,1)= 0.0;
      dfdu(1,2)= 1.0;
      dfdu(1,3)= 0.0;
%     
      dfdu(2,1)= 3*u(1)*u(1)-2*(1+par(2))*u(1)+par(2);
      dfdu(2,2)= par(1);
      dfdu(2,3)= 1.0;
%
      dfdu(3,1)= par(3)/par(1);
      dfdu(3,2)= 0.0;
      dfdu(3,3)= 0.0;
%
      if(ijac.eq.1)return;
%
% no parameter derivatives are specified with this example
%
      return
