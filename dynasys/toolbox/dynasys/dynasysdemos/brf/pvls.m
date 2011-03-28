      function []= pvls(ndim,u,par)
%                    
%
      include 'brf.inc';
      parameter (nn=nx-1);
%
      common /blppfr/ ifrst;
%
% problem independent initialization :
      if(ifrst.ne.1234)then;
	 call gencf(par);
         ifrst=1234;
      end;
;
      return 
