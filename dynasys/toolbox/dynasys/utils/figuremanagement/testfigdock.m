group = setfigdocked('GroupName','F100 Inertia Coupling','GridSize',[1 2],'Maximize',1,'groupdocked',0);
h(1)=figure;
set(gcf,'Tag','BifFig','Name','Bifurcation Diagram','NumberTitle','off');
plot([1:10],[1:10],'b');
group = setfigdocked('GroupName','F100 Inertia Coupling','Figure',gcf,'Figindex',2);
h(2)=figure;
plot([1:10],[1:10],'r');
set(gcf,'Tag','AnimFig','Name','Aircraft Animation','NumberTitle','off');
group = setfigdocked('GroupName','F100 Inertia Coupling','Figure',gcf,'Figindex',1);
