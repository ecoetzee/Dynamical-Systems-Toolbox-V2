      function []= func(ndim,u,icp,par,ijac,f,dfdu,dfdp) 
%                     
% 
      include 'brc.inc';
      parameter ( np=nn+1 );
      common /blppde/ d2(nn,0:np);
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
%
        for 4 i=1,nn;
          for 1 k=1,ne;
            w(k)=u(i,k);
 1        continue;
          call ff(ne,w,par,fw);
          for 3 j=1,ne;
            f(i,j)=fw(j) + dc(j)*(u0(j)*d2(i,0)+u1(j)*d2(i,np));
            for 2 k=1,nn;
              f(i,j)=f(i,j)+dc(j)*d2(i,k)*u(k,j);
 2          continue;
 3        continue;
 4      continue;
% 
      return 
