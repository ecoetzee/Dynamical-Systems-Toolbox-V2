function [fb,o,dbc]= bcnd(par,u0,u1,ijac)
%
% boundary conditions for demo int
%
fb=[];
o=[];
dbc=[];

fb(1)=u0(1)-u1(1)-par(2);

if(ijac==0)
    return
end

dbc(1,1)=1.0;
dbc(1,2)=0.0;

dbc(1,3)=-1.0;
dbc(1,4)=0.0;

if(ijac==1)
    return
end

% parameter derivatives
dbc(1,5)=0.0;
dbc(1,6)=-1.0;
dbc(1,7)=0.0;

