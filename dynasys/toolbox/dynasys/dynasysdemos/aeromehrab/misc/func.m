function [f,o,dfdu,dfdp]=func(par,u,ijac)
% function file for Mehra's Aircraft B
%
%Created by S.Gill 02/12/2010

f=[];
o=[];
dfdu=[];
dfdp=[];
        
%Turn AUTO U array into alpha, beta, p, q, and r.
alpha=u(1)*pi/180;
beta=u(2)*pi/180;
p=u(3)*pi/180;
q=u(4)*pi/180;
r=u(5)*pi/180;

%Zero lift angle of incidence is set to zero for convenience
alphazero=0*pi/180;

%Defines which control surface deflection is being varied. Change for
%different control surfaces
da=par(1)*pi/180;
%de=par(1)*pi/180;
%dr=par(1)*pi/180;

%Changed alpha and alpha zero into the parameter delta alpha
delalpha=alpha-alphazero;

%Gives Values for the other control surfaces
%da=0*pi/180;
de=2*pi/180;
dr=0*pi/180;

%Equations of Motion for Aircraft B
f(1)=(q-(p*beta)+(-3*delalpha)+(0*de))*180/pi;
f(2)=((-0.056*beta)+(p*(sin(alphazero)+delalpha))-(r*cos(alphazero))+(0*da)+(0*dr))*180/pi;
f(3)=((-110.1*beta)+(0*q)+(1.512*r)+(-21.64*p)-(0.706*q*r)+(-326.3*da)+(0*dr))*180/pi;
f(4)=((-12.34*delalpha)+(-2.205*q)+(0.960*p*r)+(-33.24*de)-(-0.391*p*beta))*180/pi;
f(5)=((3.705*beta)+(-0.259*r)+(0*p)-(0.787*p*q)+(0*da)+(0*dr))*180/pi;

end