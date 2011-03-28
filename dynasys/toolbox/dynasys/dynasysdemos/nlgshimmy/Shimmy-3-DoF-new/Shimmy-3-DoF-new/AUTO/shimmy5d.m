function [f,o,dfdu,dfdp]=shimmy5d(par,u,ijac)
%
% function file for demo shimmy5d
%
f=[0,0];
o=[];
dfdu=[];
dfdp=[];

iz=100;
ix=600;
cma=1.0;
sigma=0.3;
alphag=10*pi/180;
eeff=par(1)*cos(par(8))+(0.362d0+par(1)*sin(par(8)))*tan(par(8));
hlg=2.5;

alpha=atan(u(5)/sigma);

fy=0.01*atan(7*tan(alpha))*cos(0.95d0*atan(7*tan(alpha)))*par(4);

if (abs(alpha)<alphag)
    mz=-par(4)*cma*(alphag/3.1415926)*sin(alpha*3.1415926/alphag);
else
    mz=0;
end

mkpsi=-par(6)*u(1);
mdpsi=-par(3)*u(2);
mklampsi=-eeff*fy;
m4=-u(2)*par(7)*cos(par(8))/par(2);
mkdelta=-par(9)*u(3);
mddelta=-par(10)*u(4);
mklamdelta=-fy*cos(u(1)*cos(par(8)))*hlg*cos(par(8))*cos(u(3));

f(1)=u(2);
f(2)=(1/iz)*(mkpsi+mdpsi+mklampsi+mz+m4...
    +par(4)*sin(par(8))*eeff*sin(u(1)*cos(par(8))));
f(3)=u(4);
f(4)=(1/ix)*(mkdelta+mddelta+mklamdelta...
    +par(4)*eeff*sin(u(1)*cos(par(8))));
f(5)=par(2)*sin(u(1)*cos(par(8)))...
    +(eeff-par(5))*u(2)*cos(u(1)*cos(par(8)))*cos(par(8))...
    -(par(2)/sigma)*u(5)*cos(u(1)*cos(par(8)))...
    +hlg*cos(u(3))*u(4);

