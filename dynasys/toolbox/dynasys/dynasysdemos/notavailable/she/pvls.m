      function []= pvls(ndim,u,par)
%                    
%
% homoclinic bifurcations common block needed here :
      common /blhom/ itwist,istart,iequib,nfixed,npsi,nunstab,nstab;
%
% if iequib = 1 put analytic expressions for equilibria in
%   par(11+i), i=1,..,ndim         left hand equilibrium
%   par(11+i), i=ndim+1,...,2*ndim right hand equilibrium
      if(iequib.eq.-1)then;
        par(12)=dsqrt(par(1));
        par(13)=-par(1);
        par(14)=0.0d0;
        par(15)=0.0d0;
        par(16)=0.0d0;
        for i=ndim+1,2*ndim;
          par(11+i)= 0.0;
        endfor;
      end;
%
      return
