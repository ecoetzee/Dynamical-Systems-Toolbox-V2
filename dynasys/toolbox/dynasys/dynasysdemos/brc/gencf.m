      function []= gencf(par)
%                     
%
      include 'brc.inc';
      parameter ( np=nn+1, m=nn+2);
      common /blppde/ d2(nn,m);
%
        pi=4*datan(1.d0);
        x(1)=0.d0;
        for 1 k=2,np;
          c=cos( (2*k-3)*pi/(2*nn) );
          x(k)=(1+c)/2;
 1      continue;
        x(m)=1.d0;
%
        for 3 i=1,m;
          for 2 j=1,m;
            ri(i,j)=0.d0;
            xx(i,j)=x(i)**(j-1);
 2        continue;
          ri(i,i)=1.d0;
 3      continue;
%
        call ge(0,m,m,xx,m,m,cc,m,ri,ir,ic,det);
%  
        for 6 i=1,nn;
          for 5 j=1,m;
            d2(i,j)=0.d0;
            for 4 k=2,m-1;
              d2(i,j)=d2(i,j)+cc(k+1,j)*k*(k-1)*x(i+1)**(k-2);
 4          continue;
 5        continue;
 6      continue;
%
      return
