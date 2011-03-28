      function []= stpnt(ndim,u,par,t)
%                     
%
% sets parameter values for homoclinic bifurcation analysis (ips=9).
%
%
% common block needed if ips=9 (homoclinic bifurcations) :
      common /blhom/ itwist,istart,iequib,nfixed,npsi,nunstab,nstab;
%
%                                                                      
% problem parameters (only par(1 9) are available to the user) :
%
        par(1) = -1.8512    ! lambda;
        par(2) = -0.15d0         ! kappa;
        par(3) = 10.0d0          ! 1/epsilon_1;
%
        par(11)=  0.1            ! truncated time interval;
%                                                                      
% if iequib=1 then put the equilibrium in par(11+i), i=1,...,ndim :
%
        if (iequib.ne.0) then;
          par(12) = -0.9591016;
          par(13) = -0.9591016;
          par(14) = -0.9591016;
        end;
%                                                                      
% distance along the unstable manifold :
%
        if (istart.eq.3) then;
          par(12+ndim*iequib)=-0.00001;
        end;
%                                                                      
%
      return
