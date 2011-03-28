      function []= bcnd(ndim,par,icp,nbc,u0,u1,fb,ijac,dbc) 
%                     
% 
% 
%      *define the boundary conditions (dirichlet, in this example).
       fb(1)=u0(1)-par(1);
       fb(2)=u0(2)-par(2)/par(1);
       fb(3)=u1(1)-par(1);
       fb(4)=u1(2)-par(2)/par(1);
% 
      return 
