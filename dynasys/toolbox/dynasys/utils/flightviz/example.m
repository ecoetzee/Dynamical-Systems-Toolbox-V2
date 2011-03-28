% ---------------------------------------------------------------------------
%
% This is an example script file to show how to use the flightviz function
% to create an animation.
%
% You will need to supply some flight data, see the load data section below.
% 
% If you specify an output file then an avi file will be created at the 
% frame rate that is given.
% If you do not specify an output file then the animation will be displayed
% in real time.
% Use the timescale variable to speedup or slowdown the animation,
%
% ---------------------------------------------------------------------------


  % animation file, if this is empty then no file will be created
  outfile = 'test.avi';
  %outfile = '';
  codec = 'Indeo3';  % choose a codec that you can use on your setup
                   % try: 'MP42', 'Indeo3', 'Indeo5', 'Cinepak', 'MSVC', 'RLE' or 'None'
  % if there is no output file specified then the animation will be run realtime
  FPS = 25;       % Frames per second
  Tzero = 0;      % start time
  timescale = 0.25;  % animation speedup over realtime

  % a frame buffer will speed up animation file creation
  FrameBuff = 1;  % Frame buffer in seconds


%% load data
%% Replace this section of code with code to load your data
  dat.time = ((0:99)/10)';
  dat.euler= [(pi/6)*cos(dat.time*2) (pi/6)*sin(dat.time*3) (pi/6)*sin(dat.time*4)];
  dat.vel  = zeros([100 3]);
  dat.alt  = zeros([100 1]);
  dat.mach = (0:99)/100;
  dat.acc  = (0:99)/50;
  dat.ele  = (pi/6)*sin(dat.time);
  dat.ail  = (pi/6)*sin(dat.time);
  dat.rud  = (pi/6)*sin(dat.time);
  dat.flap = (pi/6)*sin(dat.time);

%% Animate
  modelName='Phantom';
  
  % Create figure
  FIG = flightviz(modelName);
  % fps holder
  HFPS = uicontrol('Style','text','Parent',FIG,'Units','normalized','HorizontalAlignment','center', ...
          'Tag','Status','BackgroundColor',[0.7 0.8 0.9],'Position',[ 0.001 0.001 0.1 0.05],'TooltipString', ...
          'Frames per second','string','fps','fontsize',10);

%   % Create video file 
%   if (~isempty(outfile)),
%     try; MOVF = close(MOV), catch; disp(lasterr); end;
%     MOV = avifile(outfile,'FPS',FPS,'COMPRESSION',codec);
%     OUT_frames(1:ceil(FrameBuff*FPS)) = getframe(FIG);
%     buf_frame = 0;
%   end;

  TIME = [Tzero:timescale/(FPS):max(dat.time)]';

  % interpolate data, watch out for wrapping data such as roll angle
  % interpolating here should be faster than interpolating before each frame
  euler = interp1(dat.time,dat.euler,TIME);
  vel   = interp1(dat.time,dat.vel  ,TIME);
  alt   = interp1(dat.time,dat.alt  ,TIME);
  mach  = interp1(dat.time,dat.mach ,TIME);
  acc   = interp1(dat.time,dat.acc  ,TIME);
  ele   = interp1(dat.time,dat.ele  ,TIME);
  ail   = interp1(dat.time,dat.ail  ,TIME);
  rud   = interp1(dat.time,dat.rud  ,TIME);
  flap  = interp1(dat.time,dat.flap ,TIME);

  % Animate
  drawnow;
  tic;
  toc_old = toc;
  frame = 1;
  fram0 = frame;
  fps_old = 0;
  while (frame<=length(TIME)),

    % update figure
    fig = flightviz(modelName,euler(frame,:)*pi/180,vel(frame,:),alt(frame),mach(frame),acc(frame),ele(frame),ail(frame),rud(frame),flap(frame));     

     % Calculate frames per second
     if (isempty(outfile)) 
       % Realtime mode, FPS is dynamic
       fps = (fps_old + 1/(toc-toc_old))/2; fps_old = fps; toc_old = toc;
       set(HFPS,'string',sprintf('%3.1ffps',fps));
       frame = ceil(abs(FPS)*toc);
       T = max(TIME)-TIME(min(frame,length(TIME)));
       set(FIG,'name',sprintf('Realtime %imin %04.1fs to end',floor(T/60),round(mod(T,60)/timescale)));
     else
       % save to file at fixed frame rate, calculate actual rendering FPS
       fps = (fps_old + 1/(toc-toc_old))/2; fps_old = fps; toc_old = toc;
       set(HFPS,'string',sprintf('%3.1ffps',FPS));
       frame = frame+1;
       T = (length(TIME)-frame)/(frame/toc);
       %T = TIME(frame);
       set(FIG,'name',sprintf('Forced frame rate of %gfps, rendering %0.1f fps (%0.1f%%), ETA %imin %04.1fs',FPS,fps,100*fps/FPS,floor(T/60),round(mod(T,60))));
     end;

%      % drawnow;
%      % Save video to buffer
%      if (~isempty(outfile)),
%        if (buf_frame==0),  % initialise buffer
%          buf_frame = 1;
%          OUT_frames(buf_frame) = getframe(FIG);
%        else  % add frame to buffer
%          buf_frame = buf_frame + 1;
%          OUT_frames(buf_frame) = getframe(FIG);
%        end;
%        if (buf_frame>=ceil(FrameBuff*abs(FPS))),  % save buffer to disk
%          TXT = sprintf('Saving %g frames to disk, "%s"',buf_frame,outfile);
%          set(FIG,'name',TXT);
%          disp(TXT);
%          MOV = addframe(MOV,OUT_frames);
%          buf_frame = 0;
%          disp(sprintf('       %g of %g frames done %g to go',frame,length(TIME),length(TIME)-frame));
%        end;
%      end;
  end;
  % final fps rendered
  fps = length(TIME)/toc;       
  disp(sprintf('FPS = %g',fps));

%   % Flush frame buffer and Close video file
%   if (~isempty(outfile)),
%     disp(sprintf('Flushing %g frames to disk.',buf_frame));
%     MOV = addframe(MOV,OUT_frames(1:buf_frame));
%     MOVF = close(MOV);
%    disp(sprintf('Saved video to file <a href="%s">%s</a>.',outfile,outfile));
%   end;

disp('Done.');

