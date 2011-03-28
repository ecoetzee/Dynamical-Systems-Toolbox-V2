function data_out=cust_plot_fn(obj,b,s,state_sel)
global a320 a320tyres
ext=obj.fName;
if ~isempty(strfind(ext,'fort'))
    error('fName must specify model string. Received fort file')
elseif strcmp(ext(1:2),'b.') || strcmp(ext(1:2),'s.')
    [pathName,fileName,ext]=fileparts(ext);
end
us_idx=strfind(ext,'_');
mod_str=ext(1:us_idx-1);
function_string='a320_eqnofmot';
fhandle=str2func(function_string);
data_string=['a320',mod_str,'_data_wo'];

active_pars=b.active_pars;
par=b.par;
p=s.par(1,:);

feval(data_string,p(2),p(3));

states_br=b.states;
slip=zeros(size(states_br,1),3);
fz=zeros(size(states_br,1),3);
fy=zeros(size(states_br,1),3);
fx=zeros(size(states_br,1),3);
vxg=zeros(size(states_br,1),3);
vyg=zeros(size(states_br,1),3);
sl=zeros(size(states_br,1),3);
bopt=zeros(size(states_br,1),3);
mza_tmp=zeros(size(states_br,1),1);



for i=1:size(states_br,1)
    for j=1:length(active_pars)
        p(active_pars(j))=par(i,j);
    end
    ic=states_br(i,1:end);
    ic=[ic(1:6) 0 0 ic(7:9) 0];
    [dx slip_tmp fz_tmp fy_tmp vxg_tmp vyg_tmp sl_tmp bopt_tmp mza_tmp fx_tmp]...
        =fhandle(mod_str,0,ic,p);
    slip(i,:)=slip_tmp;
    bopt(i,:)=bopt_tmp;
    fz(i,:)=fz_tmp;
    fy(i,:)=fy_tmp;
    fx(i,:)=fx_tmp;
    vxg(i,:)=vxg_tmp;
    vyg(i,:)=vyg_tmp;
    sl(i,:)=[sl_tmp(1) sl_tmp(2) sl_tmp(2)];
    mza(i,:)=mza_tmp;
   
end
llf=0.5*sl;
j=str2num(state_sel(end));
if j > size(slip,2)
    error(['There are not ',num2str(j),' gears in the model']);
end
switch lower(state_sel)
    case {'c_fz1','c_fz2','c_fz3','c_fz4','c_fz5'}
        pc_load=-fz./sl*100;
        data_out=pc_load(:,j);
    case {'c_fysp'}
        p1_idx=find(active_pars==1);
        if ~isempty(p1_idx)
            fy(:,1)=cosd(par(:,p1_idx)).*fy(:,1);
        end
        pc_lat=fy./llf*100;
        data_out=(fy(:,1)-abs(fx(:,1))).*10.7726./(abs(mza)+1.91*(fy(:,2)+fy(:,3)));
    case {'c_fy1','c_fy2','c_fy3','c_fy4','c_fy5'}
        pc_lat=fy./llf*100;
        data_out=pc_lat(:,j);
    case {'c_slip1','c_slip2','c_slip3','c_slip4','c_slip5'}
        data_out=-slip(:,j);
    case {'c_bopt1','c_bopt2','c_bopt3','c_bopt4','c_bopt5'}
        data_out=bopt(:,j);  
    case {'c_tr'}
        data_out=states_br(:,1)./states_br(:,6);
    case {'c_llac'}
        data_out=sqrt(states_br(:,1).^2+states_br(:,2).^2).*states_br(:,6)./9.81;    
    case {'c_gr1','c_gr2','c_gr3','c_gr4','c_gr5'}
        data_out=vxg(:,j)./states_br(:,6); 
    otherwise
        error('Invalid custom plot string supplied')
end
