function [Hline,HT,HC,Hline2] = GTbarmeter(H,Aval,varargin)
% Draw a vertical bar with needle
%
% [Hline,Htitle,Haxis] = GTbarmeter(min,max,Title,type,slide,{property name value pairs})
%
%   Create or update a barmeter. One can create bar meters of different
%   types.
%
%   Property name value pairs are for the axes
%   type : Can be any of the following ...
%          'left', 'right', 'center', 'top', 'bottom', 'middle'
%   slide : 0 - needle moves, 1 - needle is stationary and axis moves
%   property pairs :
%      'position',[x,y,width,height] : {0-1}
%      'color','red' : or RGB color vector, background color
%      'FontSize',16 : size of axis font
%
% Example:
%   % Create a bar meter and get handle
%   [Hline] = GTbarmeter(0,100,'left','color','blue')
%   % Move the needle 
%   GTbarmeter(Hline,50);
%   % Change the color
%   GTbarmeter(Hline,65,'color','green');
%
%   see also:
%   <a href="matlab: help plotAxes">plotAxes</a>, <a href="matlab: help plotTraj">plotTraj</a>, <a href="matlab: help drawModel">drawModel</a>, <a href="matlab: help updateModel">updateModel</a>

if (length(varargin)>0),
  Yaxisloc = 'right';
  Xaxisloc = 'bottom';
  HC = axes('position',[0.96 0.1 0.02 0.75],'layer','top','FontSize',14,'color','white','YAxisLocation','left');
  if (isnumeric(varargin{2})),
    POS = varargin{2};
    set(HC,'position',POS)
    if (POS(4)<POS(3)),
      set(HC,'tag','vertical_bar');
      Talign = 'center';
      axis([H Aval 0 1 ]);
      Xtick = get(gca,'xtick');
      Xtick = min(Xtick):diff(Xtick([1 end]))/(5*(length(Xtick)-1)):max(Xtick);
      Hline = plot([0 0],[0 1],[0 0],[0 1],[0 0],[0.5 0.5],repmat(Xtick,2,1),repmat([0.25; 0.75],1,length(Xtick)),'tag','horizontal_bar_line');
      set(HC,'XMinorTick','on','YTick',[],'YTickLabel',[],'color','none','XColor','white','YColor','white','XLimMode','manual','Xlim',[H Aval]);
    else
      set(HC,'tag','horizontal_bar');
      Talign = 'center';
      axis([0 1 H Aval]);
      Ytick = get(gca,'ytick');
      Ytick = min(Ytick):diff(Ytick([1 end]))/(5*(length(Ytick)-1)):max(Ytick);
      Hline = plot([0 1],[0 0],[0 1],[0 0],[0.5 0.5],[0 0],repmat([0.25; 0.75],1,length(Ytick)),repmat(Ytick,2,1),'tag','vertical_bar_line');
      set(HC,'YMinorTick','on','XTick',[],'XTickLabel',[],'color','none','XColor','white','YColor','white','YLimMode','manual','Ylim',[H Aval]);
    end;
  elseif (strcmpi(varargin(2),'left') | strcmpi(varargin(2),'center') | strcmpi(varargin(2),'right')),
    set(HC,'tag','horizontal_bar');
    if (strcmpi(varargin(2),'left')),
      set(HC,'position',[0.02 0.1 0.02 0.75]);
      Yaxisloc = 'right';
      Talign = 'left';
    elseif (strcmpi(varargin(2),'right')),
      set(HC,'position',[0.96 0.1 0.02 0.75]);
      Yaxisloc = 'left';
      Talign = 'right';
    elseif (strcmpi(varargin(2),'center')),
      set(HC,'position',[0.49 0.1 0.02 0.75]);
      Yaxisloc = 'left';
      Talign = 'center';
    end;
    axis([0 1 H Aval]);
    Ytick = get(gca,'ytick');
    Ytick = min(Ytick):diff(Ytick([1 end]))/(5*(length(Ytick)-1)):max(Ytick);
    Hline = plot([0 1],[0 0],[0 1],[0 0],[0.5 0.5],[0 0],repmat([0.25; 0.75],1,length(Ytick)),repmat(Ytick,2,1),'tag','vertical_bar_line');
    set(HC,'YMinorTick','on','XTick',[],'XTickLabel',[],'color','none','XColor','white','YColor','white','YLimMode','manual','Ylim',[H Aval]);
  else
    Talign = 'center';
    set(HC,'tag','vertical_bar');
    if (strcmpi(varargin(2),'top')),
      set(HC,'position',[0.12 0.96 0.76 0.03]);
      Xaxisloc = 'bottom';
    elseif (strcmpi(varargin(2),'middle')),
      set(HC,'position',[0.12 0.49 0.76 0.03]);
      Xaxisloc = 'bottom';
    elseif (strcmpi(varargin(2),'bottom')),
      set(HC,'position',[0.12 0.02 0.76 0.03]);
      Xaxisloc = 'top';
    end;
    axis([H Aval 0 1 ]);
    Xtick = get(gca,'xtick');
    Xtick = min(Xtick):diff(Xtick([1 end]))/(5*(length(Xtick)-1)):max(Xtick);
    Hline = plot([0 0],[0 1],[0 0],[0 1],[0 0],[0.5 0.5],repmat(Xtick,2,1),repmat([0.25; 0.75],1,length(Xtick)),'tag','horizontal_bar_line');
    set(HC,'XMinorTick','on','YTick',[],'YTickLabel',[],'color','none','XColor','white','YColor','white','XLimMode','manual','Xlim',[H Aval]);
  end;
  HT = title(varargin(1));
  set(Hline,'linewidth',1,'color',[1 1 1]*0.6);
  set(HT,'color',[1 1 1],'FontWeight','bold','fontsize',14,'HorizontalAlign',Talign);

  set(Hline(1),'linewidth',7,'color',[1 0 0]);
  set(Hline(2),'linewidth',2,'color',[1 1 1]);
  set(Hline(3),'linewidth',2,'color',[1 0.5 0]);
  % grid on;

  Hline2 = Hline(4:end);
  Hline = Hline(1:3);

  if (nargin>=3),
    TYPE = varargin{3};
  else
    TYPE = 0;
  end;

  ii = 3;
  while ii < length(varargin)
    ii = ii + 1;
    set(HC,varargin(ii),varargin(ii+1));
    ii = ii + 1;
  end;

  set(HC,'HandleVisibility','off','YAxisLocation',Yaxisloc,'XaxisLocation',Xaxisloc,'USerData',[TYPE axis]);
else
  if (isfinite(Aval)),
    HA = get(H(1),'Parent');
    UD = get(HA,'Userdata');
    if (UD(1)==1),
      if (strcmpi(get(H,'tag'),'vertical_bar_line')),
        Ylim = get(HA,'Ylim');
        D = diff(Ylim)/2;
        set(HA,'Ylim',[Aval-D Aval+D]);
        set(H,'YDATA',[Aval Aval]);
        set(H(3),'YDATA',[0 Aval]);
      else
        Xlim = get(HA,'Xlim');
        D = diff(Xlim)/2;
        set(HA,'Xlim',[Aval-D Aval+D]);
        set(H,'XDATA',[Aval Aval]);
        set(H(3),'XDATA',[0 Aval]);
      end;
    elseif (UD(1)==2),
      if (strcmpi(get(H,'tag'),'vertical_bar_line')),
        Ylim = get(HA,'Ylim');
        D = diff(Ylim)/2;
        set(HA,'Ylim',[max(0,Aval-D) max(0,Aval-D)+2*D]);
        set(H,'YDATA',[Aval Aval]);
        set(H(3),'YDATA',[0 Aval]);
      else
        Xlim = get(HA,'Xlim');
        D = diff(Xlim)/2;
        set(HA,'Xlim',[max(0,Aval-D) max(0,Aval-D)+2*D]);
        set(H,'XDATA',[Aval Aval]);
        set(H(3),'XDATA',[0 Aval]);
      end;
    else
      if (strcmpi(get(H,'tag'),'vertical_bar_line')),
        if (Aval>UD(5)),
          set(HA,'Ylim',[Aval-diff(UD(4:5)) Aval]);
        elseif (Aval<UD(4)),
          set(HA,'Ylim',[Aval Aval+diff(UD(4:5))]);
        else
          set(HA,'Ylim',UD(4:5));
        end;
        set(H,'YDATA',[Aval Aval]);
        set(H(3),'YDATA',[0 Aval]);
      else
        if (Aval>UD(3)),
          set(HA,'Xlim',[Aval-diff(UD(2:3)) Aval]);
        elseif (Aval<UD(2)),
          set(HA,'Xlim',[Aval Aval+diff(UD(2:3))]);
        else
          set(HA,'Xlim',UD(2:3));
        end;
        set(H,'XDATA',[Aval Aval]);
        set(H(3),'XDATA',[0 Aval]);
      end;
    end;
  end;

end;
