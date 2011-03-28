function [fi,o,dint]= icnd(par,u,ijac)
%
%
%
fi=[];
o=[];
dint=[];

fi(1)=u(1)-par(3);

if(ijac==0)
    return
end

dint(1,1)=1.0;
dint(1,2)=0.0;

if(ijac==1)
    return
end

% parameter derivatives
dint(1,3)=0.0;
dint(1,4)=0.0;
dint(1,5)=-1.0;
