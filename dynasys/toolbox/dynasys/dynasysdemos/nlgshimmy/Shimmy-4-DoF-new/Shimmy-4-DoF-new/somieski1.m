% Numerical simulation of the shimmy equations given by somieski

% Initializing the problem parameters
parameterset

% Initializing the option of the numerical integration
options=odeset('RelTol',1e-8,'AbsTol',1e-8,'events','on');

tspan=[0 1000];
inicon=[ 0.035099586031231  -0.000000000798443   0.007191429997292];
[T,Y,TE,YE,IE]=ode45('odefile1',tspan,inicon,options,parameters);
plot(Y(:,1),Y(:,2));
figure
plot(YE(:,1),YE(:,3),'.');
grid on

% Testing Fy using a beam deflection equation
if 0
% Fy Vs deflection under pure bending moment at the free end of the beam    
  x=[0.00001:0.001:1];
  c=1;
  y=((c^2*x.^2-1).*sqrt(-1./(c^2*x.^2-1))+1)/c;
  plot(x,-y,'r.')
  
  c=[0.00001:0.001:1];
  L=1;
  y=((L^2*c.^2-1).*sqrt(-1./(L^2*c.^2-1))+1)./c;
  plot(y,c,'r.')
% Fy vs deflection from the models of the paper by Osman
  c=[0.0001:0.001:10];
  y1=c.*(1-cos(0.1./c));
  y2=c.*(1-sqrt((0.1./c).^2));
  y3=(c/2).*((0.1./c).^2);
  plot(c,y1,'r.')
  hold on
  
  plot(c,y2,'b.')
  plot(c,y3,'k.')
  
  P=1;
  L=1;
  r=0.5;
  EI=1;
  x=[0:0.001:L+r];
  y=-P*(L+r).*x.^2+P.*(x.^3)/6+(P*L^2)/(2*EI)*r;
  plot(x,y,'.')
end
