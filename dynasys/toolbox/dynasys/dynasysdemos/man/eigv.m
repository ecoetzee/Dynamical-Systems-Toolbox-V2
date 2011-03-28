      function []= eigv(ndim,par,v1,v2) 
%                     
% 
      common /first/ ifirst;
%
% stable eigenvalue/vector 1
       v1(1)= 0.;
       v1(2)= 0.;
       v1(3)= 1./2.66667;
% 
% stable eigenvalue/vector 2
       e=-(11+dsqrt(1201.d0))/2;
       v2(1)=1+e;
       v2(2)=par(1);
       v2(3)= 0.;
       ss=dsqrt(v2(1)**2+v2(2)**2);
       v2(1)=v2(1)/(ss*22.8277);
       v2(2)=v2(2)/(ss*22.8277);
       if(ifirst.ne.123)then;
         write(10,*)v2(1),v2(2);
         ifirst=123;
       end;
%
      return 
