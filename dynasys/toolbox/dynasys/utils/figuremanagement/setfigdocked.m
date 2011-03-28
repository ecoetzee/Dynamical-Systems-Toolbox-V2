function group = setfigdocked(varargin)
%   SETFIGDOCKED allows docking figures at specified positions in group of figures whose
%   structure is defined by the parameters GridSize, Spanned Cells, ...
%   This function also allows maximizing and docking groups into MATLAB desktop
%
%   This function runs on MATLAB 7.1 sp3 or higher
%
%   group = setfigdocked('PropertyName1',value1,'PropertyName2',value2,...)
%   PropertyName: 
%       - GroupName     name of group need to be generated
%       - GridSize      scalar or vector quantity, defines number of rows
%                       and columns of cell in group
%       - SpanCell      vector or matrix quantity, size n x 4, 
%                       [row col occupiedrows occupiedcols]
%                       build an cell at the position (row, col) in group
%                       cell (GridSize) which occupies "occupiedrows"
%                       rows and "occupiedcols" columns
%       - Figure        handle of figure
%       - Figindex      index position of figure in group cell
%       - Maximize      0/1
%       - GroupDocked   0/1
%
%   Examples:
%      Example 1: 
%           %creates empty group 'Group of Images'with 2 rows and 3 columns
%           group = setfigdocked('GroupName','Group of Images','GridSize',[2 3]);
%           im1 = imread('cameraman.tif');
%           imshow(im1)
%           group = setfigdocked('GroupName','Group of Images','Figure',gcf);
%           figure; imhist(im1)
%           group = setfigdocked('GroupName','Group of Images','Figure',gcf,'Figindex',4);
% 
%           im2 = imread('rice.png');
%           figure; imshow(im2)
%           group = setfigdocked('GroupName','Group of Images','Figure',gcf,'Figindex',2);
%           figure; imhist(im2)
%           group = setfigdocked('GroupName','Group of Images','Figure',gcf,'Figindex',5);
% 
%           im3 = imread('eight.tif');
%           figure; imshow(im3)
%           group = setfigdocked('GroupName','Group of Images','Figure',gcf,'Figindex',3);
%           figure; imhist(im3)
%           group = setfigdocked('GroupName','Group of Images','Figure',gcf,'Figindex',6);
%
%
%      Example 2: 
%          group = setfigdocked('GroupName','Image and Edges','GridSize',3,'SpanCell',[1 2 2 2]);
%          im1 = imread('cameraman.tif');
%          figure;imshow(im1);set(gcf,'Name','Cameraman','NumberTitle','off')
%          group = setfigdocked('GroupName','Image and Edges','Figure',gcf,'Figindex',2);
%  
%          figure; edge(im1,'prewitt');set(gcf,'Name','Prewitt method','NumberTitle','off')
%          group = setfigdocked('GroupName','Image and Edges','Figure',gcf,'Figindex',1);
%  
%          figure; edge(im1,'roberts');set(gcf,'Name','Roberts method','NumberTitle','off')
%          group = setfigdocked('GroupName','Image and Edges','Figure',gcf,'Figindex',3);
% 
%          figure; edge(im1,'roberts');set(gcf,'Name','Roberts method','NumberTitle','off')
%          group = setfigdocked('GroupName','Image and Edges','Figure',gcf,'Figindex',4);
% 
%          figure; edge(im1,'roberts');set(gcf,'Name','Roberts method','NumberTitle','off')
%          group = setfigdocked('GroupName','Image and Edges','Figure',gcf,'Figindex',5);
% 
%          figure; edge(im1,'canny');set(gcf,'Name','Canny Method','NumberTitle','off')
%          group = setfigdocked('GroupName','Image and Edges','Figure',gcf,'Figindex',6);
% 
%          group = setfigdocked('GroupName','Image and Edges','Maximize',1,'GroupDocked',0);

%
% The author does not accept any responsibility or liability for loss 
% or damage occasioned to any person or property through using function,instructions, 
% methods or ideas contained herein, or acting or refraining from acting 
% as a result of such use. The author expressly disclaim all implied warranties, 
% including merchantability or fitness for any particular purpose. There will be 
% no duty on the author to correct any errors or defects in the function. 
% This function and the documentation are the property of the author and should only 
% be used for scientific and educational purposes. All software is provided free and 
% it is not supported. The author is, however, happy to receive comments,
% criticism and suggestions to phan@brain.riken.jp

% Programmed and Copyright by Phan Anh Huy 
% phan@brain.riken.jp
% $Date: 24/12/2007

%%
import java.awt.*;
import java.awt.event.*;
import java.io.IOException;

%% get parameters
[regargs, proppairs]=parseparams(varargin);

invars = proppairs(1:2:end);
listvar = {'figure','groupname', 'gridsize' ,'spancell','figindex','maximize' 'groupdocked';
            'fig' ,'groupname','gridsize','posarr','figind','maximizeflag','grpdockflag'};

for k = 1: numel(invars)
    varind = find(~cellfun(@isempty,strfind(listvar(1,:),lower(invars{k}))));
    if ~isempty(varind)
        eval(sprintf('%s = proppairs{2*%d};',listvar{2,varind},k));
    end
end

%%
desktop = com.mathworks.mde.desk.MLDesktop.getInstance;

%%
if (exist('groupname','var')~=1) | isempty(groupname)
    groupname = 'MyGroup';
end
group = desktop.addGroup(groupname);    %
desktop.showGroup(groupname,1)

if (exist('grpdockflag','var')==1)
    desktop.setGroupDocked(groupname,grpdockflag)
end

% xoa group

%% tao moi

%%
desktop.showGroup(groupname,1)
pause(.3)
jpanel = group.getInternalFrame.getComponent(0).getComponent(0);
% jpanel.requestFocus(1);     % Focus on Dock
%%
%%
if (exist('gridsize','var')==1) && ~isempty(gridsize)
    if numel(gridsize) ==1
        gridsize(2) = gridsize(1);
    end
    toolbar = jpanel.getComponent(0);
    btnsplittopbottom = toolbar.getComponent(0).getComponent(0).getComponent(0).getComponent(3);
    btnsplittopbottom.setMnemonic('A');

    %% Split NORTH-SOUTH

    robot = Robot;
    for k=1:10
        jpanel.requestFocus(1);     % Focus on Dock
        robot.setAutoDelay(50);
        robot.keyPress(KeyEvent.VK_ALT);
        robot.setAutoDelay(50);
        robot.keyPress(KeyEvent.VK_A);
        robot.setAutoDelay(50);
        robot.keyRelease(KeyEvent.VK_A);
        % robot.setAutoDelay(50);
        robot.keyRelease(KeyEvent.VK_ALT);
        % robot.setAutoDelay(100);
        %% split jpanel into
        try 
            tildpane = jpanel.getComponent(1).getComponent(1);%.getComponent(0)
        catch
            tildpane = jpanel.getComponent(1).getComponent(0).getComponent(1);     %ver 7.1
        end
        if strcmp(char(tildpane.getClass.getCanonicalName),'com.mathworks.mwswing.desk.DTTiledPane')
            break
        end
    end

    tildpane.setGridSize(java.awt.Dimension(gridsize(2),gridsize(1)));

    % jpanel.updateUI;

    %% spanning cells
    if (exist('posarr','var')==1) && ~isempty(posarr)
        for pos = posarr'
            pos(1:2) = pos(1:2)-1;
            if pos(3)>1
                for k = 1:pos(4)
                    tildpane.setRowSpan(pos(1),pos(2)+k-1,pos(3))
                end
            end
            %% span cot
            tildpane.setColumnSpan(pos(1),pos(2),pos(4))
            jpanel.updateUI;
        end
    end

    %%
    tildpane.setCloseButtonsEnabled(0)      % khong cho tat cau truc luoi
end
%%
if (exist('maximizeflag','var')==1) && maximizeflag==1
    try
        desktop.setGroupMaximized(groupname,1)  % maximize group
    catch
        group.getInternalFrame.getTopLevelAncestor.setMaximized(1);
    end
end
%% add figure
if (exist('figind','var')==1) && ~isempty(figind)
    try
        tildpane = jpanel.getComponent(1).getComponent(1);%.getComponent(0)
        if ~strcmp(char(tildpane.getClass.getCanonicalName),'com.mathworks.mwswing.desk.DTTiledPane')
            tildpane = jpanel.getComponent(1).getComponent(1).getComponent(0);
        end
    catch           %ver 7.1
        tildpane = jpanel.getComponent(1).getComponent(0).getComponent(1);
        if ~strcmp(char(tildpane.getClass.getCanonicalName),'com.mathworks.mwswing.desk.DTTiledPane')
            tildpane = jpanel.getComponent(1).getComponent(0).getComponent(1).getComponent(0);
        end
    end
    tildpane.setSelectedTile(figind-1)
end
% fig = figure;
if (exist('fig','var')==1) && ~isempty(fig)
    set(get(fig,'javaframe'), 'GroupName',groupname);
    set(fig,'WindowStyle','docked');
end

%%
jpanel.updateUI;
