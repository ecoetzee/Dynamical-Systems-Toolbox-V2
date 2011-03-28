      function []= fopt(ndim,u,icp,par,ijac,fs,dfdu,dfdp)
%                    
% 
%
       fs=par(3);
%
      if(ijac.eq.0)return;
%
       for 1 i=1,ndim;
         dfdu(i)=0.d0;
 1     continue;
%     
      if(ijac.eq.1)return;
%
%      *parameter derivatives
       for 2 i=1,9;
         dfdp(i)=0.d0;
 2     continue;
%
       dfdp(3)=1.d0;
%
      return
