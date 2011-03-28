% Numerical simulation of the shimmy equations given by somieski

% Initializing the problem parameters
parameterset

% Initializing the option of the numerical integration
options=odeset('RelTol',1e-6,'AbsTol',1e-6,'events','on');

tspan=[0 10];
parameters(13)=30;

inicon=[   0.047775109049949 -42.220102984697597   0.001810029349389  -1.414095995444490   0.020328927171386];
[T,Y,TE,YE,IE]=ode45('odefile1',tspan,inicon,options,parameters);
plot(Y(:,1),Y(:,2))

data=[];
vel=[];
for v=248:0.1:250
    vel=[];
    parameters(13)=v;
    parameters(13)
    inicon=[Y(end,:)];
    tspan=[0 1];
    options=odeset('RelTol',1e-6,'AbsTol',1e-6,'events','off');
    [T,Y]=ode45('odefile1',tspan,inicon,options,parameters);
    inicon=[Y(end,:)];
    tspan=[0 1];
    options=odeset('RelTol',1e-6,'AbsTol',1e-6,'events','on');
    [T,Y,TE,YE,IE]=ode45('odefile1',tspan,inicon,options,parameters);
    vel(1:length(YE(:,1)),1)=v;
    data=[data;vel IE YE];
end

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
    alpha=[0:0.001:1];
    Fyvec=[];
    for i=1:length(alpha)
        if abs(alpha(i)) <= 0.09
            Fy=20*alpha(i);
        else
            Fy=20*5*pi/180*sign(alpha(i));
        end
        Fyvec=[Fyvec Fy];
    end
    plot(alpha*180/pi,Fyvec)
    Fyest=20*0.1*tan(0.3*alpha)/(0.03+(3*tan(0.3*alpha))^2);
    Fyest=20*0.1*tan(alpha)./(0.1+(1.3*tan(alpha)).^2);
    Fyest=3*atan(7*tan(alpha)).*cos(0.95*atan(7*tan(alpha)));
end
figure
plot(T,Y(:,1),'r')
hold on
plot(T,Y(:,3),'b')
plot(T,Y(:,5),'k')
hold on

load timeseries_27_60
subplot(2,1,1)
plot(T(:,1),Y(:,1),'k')
axis([9.98 10 -0.6 0.6])
subplot(2,1,2)
hold on
plot(T(:,1),2.5*sin(Y(:,3)),'k')
plot(T(:,1),Y(:,5),'b')
axis([9.98 10 -0.1 0.1])
