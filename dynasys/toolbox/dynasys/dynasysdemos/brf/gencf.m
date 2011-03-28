      function []= gencf(par)
%                     
%
      include 'brf.inc';
      parameter (nn=nx-1);
      common /blppde/ d0(nn,nn),d2(nn,nn),di(nn,nn),dd(nn,nn),;
     *          ri(nn,nn);
%
      for 2 i=1,nn;
        for 1 j=1,nn;
          d0(i,j)=0;
          d2(i,j)=0;
          di(i,j)=0;
          dd(i,j)=0;
          ri(i,j)=0;
 1      continue;
        d0(i,i)=10.d0/12.d0;
        d2(i,i)=-2*nx**2;
        ri(i,i)=1;
 2    continue;
%
      for 3 i=1,nn-1;
        d0(i+1,i)=1.d0/12.d0;
        d0(i,i+1)=1.d0/12.d0;
        d2(i+1,i)=nx**2;
        d2(i,i+1)=nx**2;
 3    continue;
%
      call ge(0,nn,nn,d0,nn,nn,di,nn,ri,ir,ic,det);
%
      for 6 i=1,nn;
        for 5 j=1,nn;
          s=0.d0;
          for 4 k=1,nn;
            s=s+di(i,k)*d2(k,j);
 4        continue;
          dd(i,j)=s;
 5      continue;
 6    continue;
%
      return
