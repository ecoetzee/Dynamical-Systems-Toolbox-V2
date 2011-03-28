% Vector field from the somieski-modelling of shimmy
function varargout = odefile1(t,y,flag,parameters)

switch flag

    case 'events'
        [varargout{1:3}]=events(y,parameters);

    case ''
        [varargout{1}]=f(y,parameters);

end
% The ODE's defining the dynamical system
function dydt = f(y,parameters)
a=parameters(1);
e=parameters(2);
Iz=parameters(3);
Fz=parameters(4);
c=parameters(5);
cFa=parameters(6);
cMa=parameters(7);
k=parameters(8);
kappa=parameters(9);
sigma=parameters(10);
alphag=parameters(11);
delta=parameters(12);
V=parameters(13);
ksdelta=parameters(14);
csdelta=parameters(15);

alpha=atan(y(5)/sigma);
rake=0.1571;
hlg=2.5;
eeff=e*cos(rake)+(0.362+e*sin(rake))*tan(rake);
if abs(alpha) <= delta
    Fy=cFa*alpha*Fz;
else
    Fy=cFa*delta*Fz*sign(alpha);
end
%Fy=20*0.1*tan(0.3*alpha)*Fz/(0.03+(3*tan(0.3*alpha))^2);
Fy=3*atan(7.0*tan(alpha))*cos(0.95*atan(7.0*tan(alpha)))*Fz;

if abs(alpha) <= alphag
    Mz=(Fz*cMa*10)/180 * sin(180*alpha/10);
else
    Mz=0;
end

M4=y(2)*kappa*cos(rake)/V;
MKdelta=y(3)*ksdelta;
MDdelta=y(4)*csdelta;
MKlamdel=Fy*cos(y(1)*cos(rake))*hlg*cos(rake);

% Nonlinear Set of Eqns
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dydt = [y(2);...
    k*y(2)+c*y(1)+Mz-eeff*Fy+M4;...
    y(4);...
    MKdelta+MDdelta+MKlamdel+Fz*eeff*sin(y(1)*cos(rake));...
    V*sin(y(1)*cos(rake))+(eeff-a)*y(2)*cos(rake)*cos(y(1)*cos(rake))-(V/sigma)*y(5)*cos(y(1)*cos(rake))+hlg*cos(y(3))*y(4)];
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Events monitored during integration and continuation
function [value,isterminal,direction]=events(y,parameters)

value=[y(4);y(2)];
isterminal=[0;0];
direction=[-1;-1];