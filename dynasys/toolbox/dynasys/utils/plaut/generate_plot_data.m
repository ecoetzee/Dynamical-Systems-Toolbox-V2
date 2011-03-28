function [plot_data pts]=generate_plot_data(obj,autoobj,index)

if isempty(index)
    return
end

autoobj.f7.Ibr=autoobj.f7.Ibr(index);
autoobj.f7.Mtot=autoobj.f7.Mtot(index);
autoobj.f7.Itp=autoobj.f7.Itp(index);
autoobj.f7.Lab=autoobj.f7.Lab(index);
autoobj.f7.Par=autoobj.f7.Par(index,:);
autoobj.f7.L2norm=autoobj.f7.L2norm(index);

if ~isempty(autoobj.f7.U)
    autoobj.f7.U=autoobj.f7.U(index,:);
end

if ~isempty(autoobj.f7.Out)
    autoobj.f7.Out=autoobj.f7.Out(index,:);
end

pts=[autoobj.f7.Ibr,autoobj.f7.Mtot,autoobj.f7.Itp,autoobj.f7.Lab];

if ~isempty(obj.zEqStr)
    axis_strs={obj.xEqStr,obj.yEqStr,obj.zEqStr};
else
    axis_strs={obj.xEqStr,obj.yEqStr};
end

for i=1:length(axis_strs)
    eqstr=lower(axis_strs{i});
    if ~isempty(strmatch('u(',eqstr))
        eqstr=strrep(eqstr,'u(','');
        eqstr=strrep(eqstr,')','');
        par_idx=str2num(eqstr);
        eqstr='u';
    end
    if ~isempty(strmatch('o(',eqstr))
        eqstr=strrep(eqstr,'o(','');
        eqstr=strrep(eqstr,')','');
        par_idx=str2num(eqstr);
        eqstr='o';
    end
    if ~isempty(strmatch('p(',eqstr))
        active_pars=autoobj.c.Icp;
        eqstr=strrep(eqstr,'p(','');
        eqstr=strrep(eqstr,')','');
        plot_par=str2num(eqstr);
        par_idx=find(active_pars==plot_par);
        eqstr='p';
        
        if isempty(par_idx)
            error('Invalid parameter selection');
        end
    end
    
    switch eqstr
        case 'p'
            plot_data(:,i)=autoobj.f7.Par(:,par_idx);
        case {'l2','norm'}
            plot_data(:,i)=autoobj.f7.L2norm;
        case 'o'
            plot_data(:,i)=autoobj.f7.Out(:,par_idx);
        otherwise
            plot_data(:,i)=autoobj.f7.U(:,par_idx);
    end
end

for i=1:size(plot_data,2)
    plot_data(:,i)=plot_data(:,i)*obj.axScales{i};
end

if ~isempty(obj.xCropLims) || ~isempty(obj.yCropLims) || ~isempty(obj.zCropLims)
    [plot_data pts]=crop_data(obj,plot_data,pts);
end

%%%%%%%%%%%%%%
function [data_out pts_out]=crop_data(obj,data_in,pts_in)
lims{1}=obj.xCropLims;
lims{2}=obj.yCropLims;
lims{3}=obj.zCropLims;

for i=1:3
    lims_tmp=lims{i};
    if isempty(lims_tmp)
        continue
    elseif length(lims_tmp)~=2
        error('Only an upper and a lower limit can be supplied in *CropLims');
    end
    
    st_idx=1;
    fin_idx_1=find(data_in(:,i)<lims_tmp(1),1);
    fin_idx_2=find(data_in(:,i)>lims_tmp(2),1);
    if isempty(fin_idx_1) && isempty(fin_idx_2)
        fin_idx=size(data_in,1);
    elseif ~isempty(fin_idx_1) && ~isempty(fin_idx_2)
        if fin_idx_1>fin_idx_2
            fin_idx=fin_idx_2;
        else
            fin_idx=fin_idx_1;
        end
    elseif ~isempty(fin_idx_1)
        fin_idx=fin_idx_1;
    else
        fin_idx=fin_idx_2;
    end
    data_in=data_in(st_idx:fin_idx,:);
    pts_in=pts_in(st_idx:fin_idx,:);
end

data_out=data_in;
pts_out=pts_in;