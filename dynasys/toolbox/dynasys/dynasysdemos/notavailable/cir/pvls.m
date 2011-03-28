      function []= pvls(ndim,u,par)
%                    
%
% homoclinic bifurcations common block needed here :
      common /blhom/ itwist,istart,iequib,nfixed,npsi,nunstab,nstab;
%
% if iequib =0 put analytic equilibrium in par(11+i), i=1..ndim
%
      if(iequib.eq.0)then;
        for i=1,ndim;
          par(11+i)= 0.0;
        endfor;
      end;
%
      return
