      function []= func(ndim,u,icp,par,ijac,f,dfdu,dfdp) 
%                     
% 
      foruble precision mus,q,pi2,p12cs,p01cs;
%
      p11ss=u(1);
      t02cs=u(2);
      p12cs=u(3);
      p01cs=u(4);
      a01cs=u(5);
%
      mus=par(1);
      q=par(2);
      sigma=par(3);
      zeta=par(4);
      pi2=3.141592654d0*3.141592654d0;
%
      f(1)=p11ss*t02cs+p11ss*mus-p12cs*p01cs;
      f(2)=-(p11ss**2+t02cs);
      f(3)=(4.*sigma*p11ss*p01cs+4.*sigma*p12cs*mus-9.;
     . *sigma*p12cs+4.*p11ss*p01cs+4.*p12cs*mus)/(4.*(;
     . sigma+1.));
      f(4)=(-q*sigma**2*a01cs-pi2*sigma**2*p01cs+3.*;
     . pi2*sigma*p11ss*p12cs+3.*pi2*p11ss*p12cs)/(4.*;
     . pi2*sigma);
      f(5)=(zeta*(p01cs-a01cs))/4.;
%
       if(ijac.eq.0)return;
%     
      for 100 i=1,5;
        for 100 j=1,5;
          dfdu(i,j)=0.0d0;
 100  continue;
%
      dfdu(1,1)=t02cs+mus;
      dfdu(1,2)=p11ss;
      dfdu(1,3)=-p01cs;
      dfdu(1,4)=-p12cs;
%
      dfdu(2,1)=-2.*p11ss;
      dfdu(2,2)=(-1.);
%
      dfdu(3,1)=p01cs;
      dfdu(3,3)=(4.*sigma*mus-9.*sigma+4.*mus)/(4.*(sigma+1.));
      dfdu(3,4)=p11ss;
%
      dfdu(4,1)=(3.*p12cs*(sigma+1.))/(4.*sigma);
      dfdu(4,3)=(3.*p11ss*(sigma+1.))/(4.*sigma);
      dfdu(4,4)=(-sigma)/4.;
      dfdu(4,5)=(-q*sigma)/(4.*pi2);
%
      dfdu(5,4)=zeta/4.;
      dfdu(5,5)=(-zeta)/4.;
%     
      if(ijac.eq.1)return;
%
% no parameter derivatives are specified with this example
%
      return 
