function varargout=plotbc(obj,plot_data,pts)

% Make some checks
hold on
if size(plot_data,2)==3
    do3d=1;
elseif ~size(plot_data,2)==2
    error('plot_data should have 2 or 3 columns')
else do3d=0;
end
if isempty(obj.zEqStr) && size(plot_data,2)==3
    warning('Extra data supplied in plot_data: plotting in 3d');
elseif ~isempty(obj.zEqStr) && size(plot_data,2)==2
    warning('No Z data provided: plotting in 2d')
end

% Break into segments
pts_ty=pts(:,3) ;
pts_stab=sign(pts(:,2));
pts_irs=pts(:,4);
pts_lab=pts(:,2);
pts_stab(1,1)=pts_stab(2,1);

% Find bifurcation points and use these as references
sw_idx=find(abs(pts_ty)>0);
sw_idx=[1;sw_idx;length(pts_ty)];
sw_idx=unique(sw_idx);

for i=1:length(sw_idx)-1
   seg_idx{i}=sw_idx(i):sw_idx(i+1); 
   stabflag=sign(sum(sign(pts_stab(sw_idx(i):sw_idx(i+1)))));
   if stabflag==0
       stabflag=sign(pts_stab(sw_idx(i+1)));
   end
   current_stab{i}=stabflag;
end

% Plot the lines
for i=1:length(seg_idx)
    idx_tmp=seg_idx{i};     

    if current_stab{i}==-1
        if do3d
            plot3(plot_data(idx_tmp,1),plot_data(idx_tmp,2),plot_data(idx_tmp,3),...
                'color',obj.clr_stab,'linewidth',obj.lineWidth,'linestyle',obj.ls_stab);
        else plot(plot_data(idx_tmp,1),plot_data(idx_tmp,2),...
                'color',obj.clr_stab,'linewidth',obj.lineWidth,'linestyle',obj.ls_stab);
        end
    else
        if do3d
            plot3(plot_data(idx_tmp,1),plot_data(idx_tmp,2),plot_data(idx_tmp,3),...
                'color',obj.clr_unst,'linewidth',obj.lineWidth,'linestyle',obj.ls_unst);
        else plot(plot_data(idx_tmp,1),plot_data(idx_tmp,2),...
                'color',obj.clr_unst,'linewidth',obj.lineWidth,'linestyle',obj.ls_unst);
        end
    end
end

% Plot the labels
if strcmp(obj.markPts,'on')
    irs_idx=find(pts_irs~=0);
    pts_ty=mod(pts_ty,10);
    for i=1:length(irs_idx)
        if strcmp(obj.markBifOnly,'on') && ...
            (pts_ty(irs_idx(i))==4 || pts_ty(irs_idx(i))==9 ||...
            pts_ty(irs_idx(i))==-9 || pts_ty(irs_idx(i))==1 ||...
        pts_ty(irs_idx(i))==6 )
            continue
        end
        if do3d
            plot_pt=[plot_data(irs_idx(i),1),plot_data(irs_idx(i),2),plot_data(irs_idx(i),3)];
            h=plot3(plot_pt(1),plot_pt(2),plot_pt(3),'ko');
        else
            plot_pt=[plot_data(irs_idx(i),1),plot_data(irs_idx(i),2)];
            h=plot(plot_pt(1),plot_pt(2),'ko'); 
        end
        [lab_str]=setlabels(obj,pts_ty(irs_idx(i)),h);
        
        if strcmp(obj.labelBifOnly,'on') && (pts_ty(irs_idx(i))==4 || pts_ty(irs_idx(i))==9 ||...
            pts_ty(irs_idx(i))==-9 || pts_ty(irs_idx(i))==1 ||...
        pts_ty(irs_idx(i))==6 )
            continue
        end
        if strcmp(obj.labelPts,'on')
            lab_str=['   ',num2str(pts_irs(irs_idx(i))),'  ',lab_str];
            if do3d==1
                text(plot_pt(1),plot_pt(2),plot_pt(3),lab_str);
            else
                text(plot_pt(1),plot_pt(2),lab_str);
            end
        end
    end%for
end%if

%--------------------------------------------------------------------------
% Set the labels
function [lab_str]=setlabels(obj,pt_type,h)
switch pt_type
    case {1,6}
        lab_str='BP';
    case {2,5}
        lab_str='LP';
         set(h,'marker','.','markersize',24);
         set(h,'MarkerFaceColor',obj.clr_lp);
    case 3
        lab_str='HB';
        set(h,'marker','p','markersize',13);
         set(h,'MarkerFaceColor',obj.clr_hb);
    case 4
        lab_str='';
         set(h,'marker','d','markersize',7);
         set(h,'MarkerFaceColor','k');
    case 7
        lab_str='PD';
        
    case 8
        lab_str='TR';
        
    case 9
        lab_str='ND';
    case -9
        lab_str='MX';
end
