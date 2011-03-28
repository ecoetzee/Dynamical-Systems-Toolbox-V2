      function []= func(ndim,u,icp,par,ijac,f,dfdu,dfdp) 
%                     
% 
%
       u1=u(1);
       u2=u(2);
       eps=par(2);
       rl =par(3);
% 
       f(1)=u2;
       f(2)=rl * ( u1*(u1**2-1)*u2 + u1 ) / eps;
% 
      return 
