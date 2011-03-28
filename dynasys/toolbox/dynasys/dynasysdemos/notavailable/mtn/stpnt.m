      function []= stpnt(ndim,u,par,t)
%                     
%
% sets parameter values for homoclinic bifurcation analysis (ips=9).
%
%
%                                                                      
% problem parameters (only par(1 9) are available to the user) :
%     par(11) is the trunction interval or `period'
%
        par(1) = 6.0d0           ! k;
        par(2) = 0.06729762d0    ! gf;
        par(3) = 0.01d0          ! d;
        par(4) = 0.5d0           ! hz;
        par(11)= 1046.178d0      ! truncated time interval;
%                                                                      
% since iequib>0 put the equilibrium in par(11+i), i=1,...,ndim :
        par(12) = 5.738626d0;
        par(13) = 0.5108401d0;
%                                                                      
%
      return
