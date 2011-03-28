      function []= stpnt(ndim,u,par,t)
%                     
%
% sets parameter values for homoclinic bifurcation analysis (ips=9).
%
%
%
% common block needed if ips=9 (homoclinic bifurcations) :
      common /blhom/ itwist,istart,iequib,nfixed,npsi,nunstab,nstab;
%
%                                                                      
% problem parameters (only par(1 9) are available to the user) :
%
        par(1) = 0.0d0           ! a;
        par(2) = 1.0d0           ! b;
        par(3) = -2.0d0          ! c;
        par(4) = 0.0d0           ! alpha;
        par(5) = 1.0d0           ! beta;
        par(6) = 0.0d0           ! gamma;
        par(7) = 0.0d0           ! mu;
        par(8) = 0.0d0           ! tilde mu;
%
        par(11)=  20.0d0         ! truncated time interval;
%                                                                      
% if iequib=1 then put initial equilibrium in par(11+i), i=1,...,ndim :
%
        if (iequib.ne.0) then;
          par(12) = 0.0;
          par(13) = 0.0;
          par(14) = 0.0;
        end;
%                                                                      
% if istart=2 then put analytic homoclinic orbit here with t in the
%   interval [0,1]
% 
% test example (a=0,b=1)
%
      s=(t-0.5)*par(11);
      u(1) = 1.0d0 - ( (1.0d0-dexp(s))/(1.0d0+dexp(s)) )**2;
      u(2) = 4.0d0 * dexp(s) * (1.0d0-dexp(s)) / (1.0d0+dexp(s))**3;
      u(3) = 0.0d0;
%
      return
