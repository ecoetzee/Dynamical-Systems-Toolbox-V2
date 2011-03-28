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
       par(1)=-0.721309d0         ! nu;
       par(2)=0.6                 ! beta;
       par(3)=0.0                 ! gamma;
       par(4)=0.6                 ! r;
       par(5)=0.328578            ! a_3;
       par(6)=0.933578            ! b_3;
%
       par(11)=36.13	! truncated time interval;
%
%                                                                      
% if iequib >0 put initial equilibrium in par(11+i), i=1,...,ndim :
%
      par(12) = 0.0;
      par(13) = 0.0;
      par(14) = 0.0;
% 
      return 
