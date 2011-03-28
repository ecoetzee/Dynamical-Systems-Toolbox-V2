      function []= stpnt(ndim,u,par,z) 
%                      
% 
      pi=4*datan(1.d0);
%
%      *set the (constant) parameters
       a  = 2.;
       b  = 5.45;
       dx = 0.008;
       dy = 0.004;
       rl = 0.75;
%
       par(1)=a;
       par(2)=b;
       par(3)=rl;
%
%      *set the actual width of the space interval [0,par(11)]
       par(11) = 1.;
%
%      *set the initial data in the (scaled) interval [0,1]
       u(1) = a   - 0.5*dsin(pi*z);
       u(2) = b/a + 0.7*dsin(pi*z);
%
%      *also set the space derivative of the initial data
%      *note the scaling by par(11)
       u(3) = - 0.5*pi*dcos(pi*z)/par(11);
       u(4) =   0.7*pi*dcos(pi*z)/par(11);
%
%      *set the diffusion constants
       par(15) = dx/rl**2;
       par(16) = dy/rl**2;
% 
      return 
