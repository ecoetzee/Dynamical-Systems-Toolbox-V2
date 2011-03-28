function [M,ref]=build_flightviz_model()

% BUILD_FLIGHTVIZ_MODEL to construct a model from STL files created in AC3D
% or any other package, for use with flightviz from Matlab Fex. Also uses 
% STL_Import from Matlab FEX.

M.body.port_split=build_component([],'dull',[0.75,0.75,0.75],'flat');
M.body.star_split=build_component([],'dull',[0.75,0.75,0.75],'flat');
M.body.tail=build_component('rudder_ac3d.stl','dull',[1,0,0],'flat');
M.body.star_flap1=build_component([],'dull',[0.75,0.75,0.75],'flat');
M.body.port_flap1=build_component([],'dull',[0.75,0.75,0.75],'flat');
M.body.elevp=build_component('starb_elev_ac3d.stl','dull',[1,0,0],'flat');
M.body.elevs=build_component('port_elev_ac3d.stl','dull',[1,0,0],'flat');
M.body.star_flap2=build_component('starb_aileron_ac3d.stl','dull',[1,0,0],'flat');
M.body.port_flap2=build_component('port_aileron_ac3d.stl','dull',[1,0,0],'flat');

M.F100(1)=build_component('nozzle_ac3d.stl','dull',[0.25,0.2,0.2],'flat');
M.F100(2)=build_component('fuselage_ac3d.stl','dull',[0.75,0.75,0.75],'gouraud');
M.F100(3)=build_component('rear_canopy_ac3d.stl','shiny',[0.45,0.35,0.6],'gouraud');
M.F100(4)=build_component('fwd_canopy_ac3d.stl','shiny',[0.45,0.35,0.6],'gouraud');

ref.body.tail=[-6.0665 0 -2.1474 0 0.6173 0];
ref.body.star_flap1=[0,0,0,0,0,0];
ref.body.port_flap1=[0,0,0,0,0,0];
ref.body.elevs=[-4.7316 0.3904 0.3550 0 0 0];
ref.body.elevp=[-4.7316 0.3904 0.3550 0 0 0];
ref.body.star_flap2=[-3.0710  3.7430 0.0963 0 0  0.5510];
ref.body.port_flap2=[-3.0710 -3.7430 0.0963 0 0 -0.5510];

ref.all=[0 0 0 0 0 0];

%--------------------------------------------------------------------------
function d=build_component(fname,mat,col,lit)

if ~isempty(fname)
    [p,t,tnorm]=STL_Import(fname,1);
end

d.mat=mat;
d.col=col;
d.lit=lit;
d.alp=1;

if ~isempty(fname)
    d.x=p(:,1);
    d.y=p(:,2);
    d.z=p(:,3);
    d.tri=t;
else
    % fake patch for non-existing component
    d.x=[0;0;0];
    d.y=[0;0;0];
    d.z=[0;0;0];
    d.tri=[1,2,3];
end

d.ref=[0 0 0 0 0 0];
d.parent=1;