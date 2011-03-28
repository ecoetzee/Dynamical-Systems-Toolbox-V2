      function []= stpnt(ndim,u,par) 
%                      
% 
% 
% common block needed if ips=9 (homoclinic bifurcations) :
      common /blhom/ itwist,istart,iequib,nfixed,npsi,nunstab,nstab;
%
%                                                                      
% problem parameters (only par(1 9) are available to the user) :
%
       par(1)=1.6                ! p;
%
       par(11)=47.4464189 	 ! truncated time interval;
%
%                                                                      
% if iequib >0 put initial equilibrium in par(11+i), i=1,...,ndim :
%
      par(12) = 0.0;
      par(13) = 0.0;
      par(14) = 0.0;
      par(15) = 0.0;
% 
      return 
