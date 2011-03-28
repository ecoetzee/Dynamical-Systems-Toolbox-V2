function [f,o,dfdu,dfdp]= func(par,u,ijac)
%
% equations file for demo int
%
f=[];
o=[];
dfdu=[];
dfdp=[];

e=exp(u(1));
f(1)=u(2);
f(2)=-par(1).*e;

if(ijac==0)
    return
end

dfdu(1,1)=0.0;
dfdu(1,2)=1;
dfdu(2,1)=-par(1).*e;
dfdu(2,2)=0.0;

if(ijac==1)
    return
end

% parameter derivatives
dfdp(1,1)=0.0;
dfdp(2,1)=-e;


