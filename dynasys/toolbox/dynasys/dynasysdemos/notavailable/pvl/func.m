      function []= func(ndim,u,icp,par,ijac,f,dfdu,dfdp) 
%                     
% 
% 
       e=exp(u(1));
       f(1)=u(2);
       f(2)=-par(1)*e;
% 
      return 
