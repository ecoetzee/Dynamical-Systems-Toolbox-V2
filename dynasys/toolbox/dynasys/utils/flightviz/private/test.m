fig =figure;

% axis circles
h.ax = plotAxes(fig,[0 0 0],9.5,3);
% trajectory arrow
h.trj = plotTraj(fig,[0 0 0],9);
% draw a model
model = drawModel(fig,'F16');

axis equal;
camlight('headlight');
  
% Create handles to the model
h.body = struct('hgt',model.hgt.all,'ref',model.ref.all);
% h.rudder = struct('hgt',model.hgt.body.rudder,'ref',model.ref.body.rudder);
% h.elevator= struct('hgt',model.hgt.body.elevator,'ref',model.ref.body.elevator);
% h.left_aileron= struct('hgt',model.hgt.body.left_aileron,'ref',model.ref.body.left_aileron);
% h.right_aileron= struct('hgt',model.hgt.body.right_aileron,'ref',model.ref.body.right_aileron);

% To update the model, one should use the updateModel function
updateModel(h.body,[0 0 0],[0 0 45]*pi/180);
% updateModel(h.rudder,[0 0 0],[0 0 -70]*pi/180);
% updateModel(h.elevator,[0 0 0],[0 -70 0]*pi/180);
% updateModel(h.left_aileron,[0 0 0],[0 -70 0]*pi/180);
% updateModel(h.right_aileron,[0 0 0],[0 70 0]*pi/180);

  axis equal; 
  camlight(0,0);