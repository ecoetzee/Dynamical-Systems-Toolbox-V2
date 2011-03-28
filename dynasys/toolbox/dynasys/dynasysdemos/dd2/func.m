function [f,o,dfdu,dfdp]= func(par,u,ijac)
%
% equations file for dd2 demo
%
f=[];
o=[];
dfdu=[];
dfdp=[];

f(1)=par(1)*u(1)*(1-u(1)) - par(2)*u(1)*u(2);
f(2)=(1-par(3))*u(2) + par(2)*u(1)*u(2);

if(ijac==0)
    return;
end

dfdu(1,1)=par(1)*(1-2*u(1))-par(2)*u(2);
dfdu(1,2)=-par(2)*u(1);
dfdu(2,1)=par(2)*u(2);
dfdu(2,2)=1-par(3) + par(2)*u(1);

if(ijac==1)
    return;
end

dfdp(1,1)=u(1)*(1-u(1));
dfdp(2,1)=0.0;
dfdp(1,2)=-u(1)*u(2);
dfdp(2,2)= u(1)*u(2);

