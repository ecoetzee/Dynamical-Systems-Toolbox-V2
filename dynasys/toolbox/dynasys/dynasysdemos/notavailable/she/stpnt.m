      function []= stpnt(ndim,u,par) 
%                      
% 
% 
%                                                                      
% problem parameters (only par(1 9) are available to the user) :
%
       par(1)=0.163875d0          ! mu;
       par(2)=0.0d0               ! q;
       par(3)=0.5d0               ! sigma;
       par(4)=0.2                 ! zeta;
%
       par(11)=85.07	! truncated time interval;
%
%                                                                      
% if iequib = 2 put initial approximations to equilibria in 
%   par(11+i), i=1,...,ndim :        left hand equilibrium
%   par(11+i), i=ndim+1,...,2*ndim   right hand equilibrium                    
%
       par(12) = 0.4048147d0;
       par(13) = -0.163875d0;
       par(14) = 0.0;
       par(15) = 0.0;
       par(16) = 0.0;
%
       par(17) = 0.0;
       par(18) = 0.0;
       par(19) = 0.0;
       par(20) = 0.0;
       par(21) = 0.0;
% 
      return 
