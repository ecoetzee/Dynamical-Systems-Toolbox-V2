      function []= func(ndim,u,icp,par,ijac,f,dfdu,dfdp) 
%                     
% 
      include 'brf.inc';
      parameter (nn=nx-1);
      common /blppde/ d0(nn,nn),d2(nn,nn),di(nn,nn),dd(nn,nn),;
     *          ri(nn,nn);
      common /blppfr/ ifrst;
%
% problem independent initialization :
        if(ifrst.ne.1234)then;
          call gencf(par);
          ifrst=1234;
        end;
%
        call setdc(ne,dc,par);
        call setbc(ne,par,u0,u1);
        call ff(ne,u0,par,f0);
        call ff(ne,u1,par,f1);
%
        for 2 i=1,nn;
          for 1 j=1,ne;
            v(i,j)= di(i, 1)*( dc(j)*nx**2*u0(j) + f0(j)/12 );
     *            + di(i,nn)*( dc(j)*nx**2*u1(j) + f1(j)/12 );
 1        continue;
 2      continue;
%
        for 6 i=1,nn;
          for 3 k=1,ne;
            w(k)=u(i,k);
 3        continue;
          call ff(ne,w,par,fw);
          for 5 j=1,ne;
            f(i,j)=v(i,j) + fw(j);
            for 4 k=1,nn;
              f(i,j)=f(i,j)+dc(j)*dd(i,k)*u(k,j);
 4          continue;
 5        continue;
 6      continue;
% 
      return 
