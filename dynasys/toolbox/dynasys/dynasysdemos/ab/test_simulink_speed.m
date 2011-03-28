par=[0,14,2];
u=[0,0];
ijac=0;

feval('ab',[],[],[],'compile');

for i=1:100000
    [f,o,dfdu,dfdp]=absimfunc(par,u,ijac);
end

feval('ab',[],[],[],'term');