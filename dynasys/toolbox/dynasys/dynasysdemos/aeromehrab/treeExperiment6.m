function treeExperiment6
% function based on treeExperiment6 by John Anderson
% see
% 
% http://www.mathworks.com/matlabcentral/newsreader/view_thread/104957#269485
%
% The mousePressedCallback part is inspired by Yair Altman's
% 
% https://foxitsoftware.com/foxit_manage/ticketsupport/developer/download.php?file_name=20
% 08-02-06-2119.09.pdf
%
% derived from Brad Phelan's tree demo
% create a tree model based on UITreeNodes and insert into uitree.
% add and remove nodes from the treeModel and update the display
import javax.swing.*
import javax.swing.tree.*;

% figure window
f = figure('WindowStyle', 'docked', 'Units', 'normalized' );
f = gcf;

b1 = uicontrol( 'string','add Node', ...
   'units' , 'normalized', ...
   'position', [0 0.5 0.5 0.5], ...
   'callback', @b1_cb ...
   );

b2 = uicontrol( 'string','remove Node', ...
   'units' , 'normalized', ...
   'position', [0.5 0.5 0.5 0.5], ...
   'callback', @b2_cb ...
   );

%[I,map] = imread([matlab_work_path, '/Projectmanager/checkedIcon.gif']);
[I,map] = checkedIcon;
javaImage_checked = im2java(I,map);

%[I,map] = imread ([matlab_work_path, '/Projectmanager/unCheckedIcon.gif']);
[I,map] = uncheckedIcon;
javaImage_unchecked = im2java(I,map);

% javaImage_checked and javaImage_unchecked are assumed to be of the same
% width
iconWidth = javaImage_unchecked.getWidth;

% create top node
rootNode = UITreeNode('root', 'File List', ...
    [], 0);
% [matlab_work_path, '/Projectmanager/fileListIcon.gif'],0);

% create two children with checkboxes
cNode = UITreeNode('unselected', 'File A', [], 0); 
% as icon is embedded here we set the icon via java, otherwise one could 
% use the uitreenode syntax uitreenode(value, string, icon, isLeaf) with
% icon being a qualified pathname to an image to be used.
cNode.setIcon(javaImage_unchecked);
rootNode.add(cNode);

cNode = UITreeNode('unselected', 'File B', [], 0);
cNode.setIcon(javaImage_unchecked);
rootNode.add(cNode);

% set treeModel
treeModel = DefaultTreeModel( rootNode );

% create the tree
tree = uitree;
tree.setModel( treeModel );
% we often rely on the underlying java tree
jtree = tree.getTree;
% some layout
set(tree, 'Units', 'normalized',...
   'position', [0 0 1 0.5]);
set( tree, 'NodeSelectedCallback', @selected_cb );

% make root the initially selected node
tree.setSelectedNode( rootNode );

% MousePressedCallback is not supported by the uitree, but by jtree
set(jtree, 'MousePressedCallback', @mousePressedCallback);

  % Set the mouse-press callback
  function mousePressedCallback(hTree, eventData) %, additionalVar)
  % if eventData.isMetaDown 
  % right-click is like a Meta-button
  % if eventData.getClickCount==2 
  % how to detect double clicks

  % Get the clicked node
    clickX = eventData.getX;
    clickY = eventData.getY;
    treePath = jtree.getPathForLocation(clickX, clickY);
    % check if a node was clicked
    if ~isempty(treePath)
      % check if the checkbox was clicked
      if clickX <= (jtree.getPathBounds(treePath).x+iconWidth)
        node = treePath.getLastPathComponent;
        nodeValue = node.getValue;
        % as the value field is the selected/unselected flag,
        % we can also use it to only act on nodes with these values
        switch nodeValue
          case 'selected'
            node.setValue('unselected');
            node.setIcon(javaImage_unchecked);
            jtree.treeDidChange();
          case 'unselected'
            node.setValue('selected');
            node.setIcon(javaImage_checked);
            jtree.treeDidChange();
        end
      end
    end
  end % function mousePressedCallback
  

   function selected_cb( tree, ev )
       nodes = tree.getSelectedNodes;
       node = nodes(1);
       path = node2path(node);
       %path 
   end

   function path = node2path(node)
       path = node.getPath;
       for i=1:length(path);
           p{i} = char(path(i).getName);
       end
       if length(p) > 1
           path = fullfile(p{:});
       else
           path = p{1};
       end
   end

   % add node
   function b1_cb( h, env )
       nodes = tree.getSelectedNodes;
       node = nodes(1);
       parent = node;
       childNode = UITreeNode('dummy', 'Child Node', [], 0);
       treeModel.insertNodeInto(childNode,parent,parent.getChildCount());
       % expand to show added child
       tree.setSelectedNode( childNode );
       % insure additional nodes are added to parent
       tree.setSelectedNode( parent );
   end

   % remove node
   function b2_cb( h, env )
       nodes = tree.getSelectedNodes;
       node = nodes(1);
       if ~node.isRoot
           nP = node.getPreviousSibling;
           nN = node.getNextSibling;
           if ~isempty( nN )
               tree.setSelectedNode( nN );
           elseif ~isempty( nP )
               tree.setSelectedNode( nP );
           else
               tree.setSelectedNode( node.getParent );
           end
           %node
           treeModel.removeNodeFromParent( node );
       end
    end
end % of main function treeExperiment6

  function [I,map] = checkedIcon()
    I = uint8(...
        [1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0;
         2,2,2,2,2,2,2,2,2,2,2,2,2,0,0,1;
         2,2,2,2,2,2,2,2,2,2,2,2,0,2,3,1;
         2,2,1,1,1,1,1,1,1,1,1,0,2,2,3,1;
         2,2,1,1,1,1,1,1,1,1,0,1,2,2,3,1;
         2,2,1,1,1,1,1,1,1,0,1,1,2,2,3,1;
         2,2,1,1,1,1,1,1,0,0,1,1,2,2,3,1;
         2,2,1,0,0,1,1,0,0,1,1,1,2,2,3,1;
         2,2,1,1,0,0,0,0,1,1,1,1,2,2,3,1;
         2,2,1,1,0,0,0,0,1,1,1,1,2,2,3,1;
         2,2,1,1,1,0,0,1,1,1,1,1,2,2,3,1;
         2,2,1,1,1,0,1,1,1,1,1,1,2,2,3,1;
         2,2,1,1,1,1,1,1,1,1,1,1,2,2,3,1;
         2,2,2,2,2,2,2,2,2,2,2,2,2,2,3,1;
         2,2,2,2,2,2,2,2,2,2,2,2,2,2,3,1;
         1,3,3,3,3,3,3,3,3,3,3,3,3,3,3,1]);
     map = [0.023529,0.4902,0;
            1,1,1;
            0,0,0;
            0.50196,0.50196,0.50196;
            0.50196,0.50196,0.50196;
            0,0,0;
            0,0,0;
            0,0,0]; 
  end
  
  function [I,map] = uncheckedIcon()
     I = uint8(...
       [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1;
        2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1;
        2,2,2,2,2,2,2,2,2,2,2,2,2,2,3,1;
        2,2,1,1,1,1,1,1,1,1,1,1,2,2,3,1;
        2,2,1,1,1,1,1,1,1,1,1,1,2,2,3,1;
        2,2,1,1,1,1,1,1,1,1,1,1,2,2,3,1;
        2,2,1,1,1,1,1,1,1,1,1,1,2,2,3,1;
        2,2,1,1,1,1,1,1,1,1,1,1,2,2,3,1;
        2,2,1,1,1,1,1,1,1,1,1,1,2,2,3,1;
        2,2,1,1,1,1,1,1,1,1,1,1,2,2,3,1;
        2,2,1,1,1,1,1,1,1,1,1,1,2,2,3,1;
        2,2,1,1,1,1,1,1,1,1,1,1,2,2,3,1;
        2,2,1,1,1,1,1,1,1,1,1,1,2,2,3,1;
        2,2,2,2,2,2,2,2,2,2,2,2,2,2,3,1;
        2,2,2,2,2,2,2,2,2,2,2,2,2,2,3,1;
        1,3,3,3,3,3,3,3,3,3,3,3,3,3,3,1]);
     map = ...
      [0.023529,0.4902,0;
       1,1,1;
       0,0,0;
       0.50196,0.50196,0.50196;
       0.50196,0.50196,0.50196;
       0,0,0;
       0,0,0;
       0,0,0];
  end

