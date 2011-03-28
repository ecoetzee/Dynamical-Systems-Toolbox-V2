      function []= func(ndim,u,icp,par,ijac,f,dfdu,dfdp) 
%                     
% 
      foruble precision kp, kc;
%
       vi=30;
       vk=-75;
       vl=-40;
       vca=140;
       gk=0.3;
       gl=0.003;
       kp=0.5;
       kc=0.0085;
       rho=0.0003;
       txt=235;
       zeta=1;
%
       gi=par(1);
       gp=par(2);
       gt=par(3);
%
       v =u(1);
       xt=u(2);
       xk=u(3);
       yi=u(4);
       c =u(5);
%
       a=127/(vi-vk);
       b=(115*vk+12*vi)/(vi-vk);
       vs=a*v-b;
%
       am=0.1*(50-vs)/(dexp((50-vs)/10)-1);
       ah=0.07*dexp((25-vs)/20);
       an=0.01*(55-vs)/(dexp((55-vs)/10)-1);
       bm=4*dexp((25-vs)/18);
       bh=1./(dexp((55-vs)/10)+1);
       bn=0.125*dexp((45-vs)/80);
%
       si=am/(am+bm);
       sk=an/(an+bn);
       txk=12.5/(an+bn);
       tyi=12.5/(ah+bh);
       zi=ah/(ah+bh);
       st=1./(dexp(0.15*(-50-v))+1);
%
       f(1)=  (gi*si**3*yi + gt*xt) * (vi-v);
     *      + (gk*xk**4 + gp*c/(kp+c)) * (vk-v);
     *      + gl * (vl-v);
       f(2)=  (st-xt) / (txt*zeta);
       f(3)=  (sk-xk) / (txk*zeta);
       f(4)=  (zi-yi) / (tyi*zeta);
       f(5)=  rho * (kc*xt*(vca-v) - c);
%
      return 
