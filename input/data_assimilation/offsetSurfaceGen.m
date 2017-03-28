%--------------------------------------------------------------------------
% offsetSurfaceGen.m
%
% Computes water level offset surface for data assimilation with ADCIRC.
%--------------------------------------------------------------------------
% Copyright(C) 2016--2017 Taylor Asher
% Copyright(C) 2017 Jason Fleming
%
% This program is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
% (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public License
% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
close all
%
% jgf: Check to see if command line options were specified; if so, 
% assume this script is being run in batch mode on an HPC system.
% If not, assume the script is being run in interactive mode on a
% Windows or Mac system.
if exist('commandLineOptions','var')==0
   pathSep='/'; % linux-style path separator
else
   pathSep='\'; %'% windows-style path separator
   clearvars -except grd %jgf: this uninitializes variables specified on the command line
   clc
end
mypath='.';
%
%-----------------------------------------------------------------------
%        I N I T I A L I Z E   P A R A M E T E R S
%-----------------------------------------------------------------------
% dodownload=1 : download gage data and compute difference from reference water level 
% dodownload=0 : load differences from a file
if exist('dodownload','var')==0
   dodownload=1;   
end 
% water level comparison: 
%   refwlmode=0 : reference water level set to 0.0 + constrefwl 
%   refwlmode=1 : load in file with adcirc average at gage locations
if exist('refwlmode','var')==0
   refwlmode=1;    
end
% only show the plots if dographs=1
if exist('dographs','var')==0
   if exist('commandLineOptions','var')==0
      dographs=1;  % show plots by default in interactive execution
   else
      dographs=0;  % suppress plots by default in HPC execution   
   end
end
% x-y grid resolution for interpolation
if exist('approxres','var')==0
   approxres=0.04; 
end
% offshorepointmode : whether to close off with exterior points 
% =0 : no
% =1 : circles of points
% =2 : offshore points
% =3 : same as 2 plus attempt to draw a box of zeroes
if exist('offshorepointmode','var')==0
   offshorepointmode=3; 
end
% offblendmode % whether to blend interpolant surface in a specific area
% =0 : no
% =1 : yes, linear
if exist('offblendmode','var')==0
   offblendmode=1; 
end
% interptomesh : whether to produce offset surface values at mesh nodes
% =1 : yes
if exist('interptomesh','var')==0
   interptomesh=1; 
end
% stopafterdownload : whether to stop the program after downloading data 
% and save the data out to a file rather than proceeding with the calculation
% =1 : yes
if exist('stopafterdownload','var')==0
   stopafterdownload=0; 
end
% writeoutfil : whether to write oi output file
% =0 : no
% =1 : yes in format suitable for reading into ADCIRC as offset surface
% =-63 : yes as a sparse .63 file
% =63 : yes as a "full" .63 file
if exist('writeoutfil','var')==0
   writeoutfil=1;  
end
if any(writeoutfil==[1,-63,63])
   timeinterval=-99999.0;  % time interval between datasets in fort.63 file 
end
% constrefwl : constant reference water level value, only used if refwlmode
% is set to 0 
if exist('constrefwl','var')==0
   constrefwl=0;   
end
%-----------------------------------------------------------------------
%        I N I T I A L I Z E   F I L E   N A M E S
%-----------------------------------------------------------------------
% jgf: TODO: need to document the data are in these mat files
%
% file containing outer line for blending
if exist('filfaroff','var')==0
   filfaroff=[mypath,[pathSep,'FarOffV1.mat']]; 
end
% file containing other line for defining bounds of non-zeroed areas
if exist('filinl','var')==0
   filinl=[mypath,[pathSep,'InlandV1.mat']]; 
end
% reference (adcirc) water level file
if exist('filrefwl','var')==0
   filrefwl=[mypath,[pathSep,'adcircAvg.dat']];  
end
% file with measured wl data
if exist('filwldata','var')==0
   filwldata=[mypath,[pathSep,'MeasWLsForTwoM2CycleMatthewOct5_23colon15Z_OIRun1.mat']];
end
% mesh file
if exist('filmesh','var')==0
   filmesh=[mypath,[pathSep,'fort.14']]; 
end
% text file containing stations of interest, in standard ADCIRC station metadata format
if exist('filstations','var')==0
   useDefaultStationList=true
   disp('INFO: Stations file name was not specified. Using a default stations list.')
end
% for bounding offshore/inland values
if exist('filoffshfixpnts','var')==0
   filoffshfixpnts=[mypath,[pathSep,'CloseOffV1.mat']]; 
end
% oi output file name
if exist('filout','var')==0
   filout=[mypath,[pathSep,'oi_surface.dat']];
end
%-----------------------------------------------------------------------
%      I N I T I A L I Z E   G A G E   P A R A M E T E R S
%-----------------------------------------------------------------------
% If the time range of the averaged ADCIRC data is expressed as 
% a cold start date/time and number of seconds since cold start to 
% start and end the comparison period, then these timing parameters
% may also be supplied here to define the period of measured data
% to be downloaded. 
if exist('timesecStart','var')==0
   timesecStart=0;  % seconds from adcirc cold start to start of averaging period
end
if exist('timesecEnd','var')==0
   timesecEnd=86400; % seconds from adcirc cold start to end of averaging period
end
if exist('csyear','var')==0
   csyear=2000; % year of cold start
end
if exist('csmonth','var')==0
   csmonth=1; % month of cold start
end
if exist('csday','var')==0
   csday=1; % day of cold start
end
if exist('cshour','var')==0
   cshour=0; % hour of cold start
end
if exist('csmin','var')==0
   csmin=0; % minute of cold start
end
if exist('cssec','var')==0
   cssec=0 ; % second of cold start
end
if exist('datatype','var')==0
   datatype='VerifiedSixMinute'; %'PreliminarySixMinute', 'VerifiedHourlyHeight', etc.
end
if exist('units','var')==0
   units='Meters';
end
if exist('vertdatum','var')==0
   vertdatum='MSL';
end
if exist('timefmt','var')==0
   timefmt='yyyy-mm-ddTHH:MM:SSZ';
end
%-----------------------------------------------------------------------
%           I N I T I A L I Z E   S U R F A C E   
%         G E N E R A T I O N   P A R A M E T E R S
%-----------------------------------------------------------------------
%
% For RegularizeData3D
if exist('rdsmoothness=','var')==0
   rd.smoothness=0.001; %weight between smoothness and fitting data (lower equals less smooth)
else
   rd.smoothness=rdsmoothness;
end
if exist('rdinterp','var')==0
   rd.interp='bilinear';   %method to interpolate data (triangle, bicubic, bilinear, nearest) 
else
   rd.interp=rdinterp;
end
if exist('rdsolver','var')==0
   rd.solver='\';          %'%solver method (normal, \, symlq, lsqr)
else
   rd.solver=rdsolver;
end
% rd.npntgrid=200;      %number of points (in each direction) for underlying gridding of data
if exist('approxres','var')==0
   rd.approxres=approxres; %approximate resolution (in each direction) for underlying gridding of data
end
%
% For optimalInterp
if exist('oilx','var')==0
   oi.Lx=1.0;   %radius of influence in x (help says Gaussian function, so this is prob a stdev or var)
else
   oi.Lx=oiLx;
end
oi.Ly=oi.Lx;    %same, in y
if exist('oiobsnoise','var')==0
   oi.obsNoise=0.001;      %signal to noise ratio
else
   oi.obsNoise=oiobsnoise;      %signal to noise ratio
end
%
% For mesh
meshdefaultz=outsidedefaultz;           %default value for mesh
if exist('meshinterpmethod','var')==0
   meshinterpmethod='oi';                  %which method to use ('oi' or 'rd')
end
%-----------------------------------------------------------------------
%             B E G I N   E X E C U T I O N
%-----------------------------------------------------------------------
%
% For bounding offshore/inland values
if offshorepointmode==1
    circledist=4;  % how far out the circle should be (same units as input data)
end
outsideblendz=0;                % value to blend to at the faroff boundary
outsidedefaultz=outsideblendz;  % default elevation value for areas that aren't to be interpolated
%
% Error checking (this prob needs to be filled out more)
if offblendmode==1&&~any(offshorepointmode==[2,3])
    error('offblendmode=1 and offshorepointmode=2 or =3')
end
%
% load stations from file in standard ADCIRC station metadata format, e.g.:
%   lon deg E   lat deg N  ! stationID ! agency   ! description ! datum 
% -81.80867700 24.55500000 ! 8724580 ! NOAA NOS ! Key West ! MSL
if useDefaultStationsList==false
   if exist('filstations','file')==0
      error(['offsetSurfaceGen.m: The stations file' filstations 'was not found.'])
   end
   stationFileID=fopen(filstations)
   if stationFileID==-1
      error(['offsetSurfaceGen.m: Failed to open station file ' filstations '.'])
   end
   % read the station data 
   [stationLon,stationLat,bang1,stationid,bang2,agency,bang3,description,bang4,datum] = textscan(stationFileID,'%f %f %s %s %s %s %s %s %s %s') % lo la !  id !  ag !  ds !  da 
   status = fclose(stationFileID)
   if status==-1
      error(['offsetSurfaceGen.m: Failed to close station file ' filstations '.'])
   end
   nstat=numel(stationid); % compute number of stations
else
   stationnum=[8724580;8723970;8723214;8722670;8721604;8720218;8720030;8670870;8665530;8661070;8658120;8658163;8656483;8654467;8651370;8638863;8638610;8637689;8635750;8577330;8575512;8574680;8573364;8632200;8570283];   
   stationid=mat2cell(num2str(stationnum),ones(numel(stationnum),1),7);
   nstat=numel(stationnum); % compute number of stations
end
% compute start and end dates
start=datenum([csyear,csmonth,csday,cshour,csmin,cssec])+timesecStart/86400;
stop=datenum([csyear,csmonth,csday,cshour,csmin,cssec])+timesecEnd/86400;
%
% load mesh only if it hasn't been done previously
if interptomesh==1
   if ~exist('grd','var')==1||~isfield(grd,'filmesh')||~strcmp(grd.filmesh,filmesh)
      grd=readfort14(filmesh);
%         grd.fil=filmesh;
   end
end
%
%% Load/download measured data
if dodownload==1
   disp('INFO: offsetSurfaceGen.m: Downloading measured gage data.')
   nodata=zeros(nstat,1);
   for cnt=1:nstat
      tmp=GetNosWaterLevelViaSOSv4_8('station',stationid{cnt},'start',start,'stop',stop,'datatype',datatype,'units',units,'vertdatum',vertdatum);
      if ~isempty(tmp)
         dat(cnt)=tmp;
      else
         nodata(cnt)=1;
      end
   end
   if stopafterdownload==1
      return % so you can save the data, rather than re-downloading every time
   end
elseif dodownload==0
   disp('INFO: offsetSurfaceGen.m: Loading measured gage data from file.')
   load(filwldata)
   nstat=numel(dat);
end
%
% Set or load reference water level data
if refwlmode==0
   disp('INFO: offsetSurfaceGen.m: Setting constant reference water level.')
   refwl=zeros(nstat,1)+constrefwl;
elseif refwlmode==1
   % read a single column of unadorned numbers in ascii format and use as-is
   disp('INFO: offsetSurfaceGen.m: Loading single column of reference (adcirc) water level data from file.')
   refwl=load(filrefwl,'-ascii');
elseif refwlmode=2
   % read the file as written by stationProcessor.f90
   disp('INFO: offsetSurfaceGen.m: Loading reference (adcirc) water level data from file as written by stationProcessor.f90.')
   %# rundes: cy:MATTHEW47 ASGS runid:nowcast agrid:not_set
   %# stationID ! operationType ! timestart(s) ! timeend(s) ! (result ! numObservations (c=1,num_components))
   if exist('filstations','file')==0
      error(['offsetSurfaceGen.m: The stations file' filstations 'was not found.'])
   end
   stationFileID=fopen(filstations)
   if stationFileID==-1
      error(['offsetSurfaceGen.m: Failed to open station file ' filstations '.'])
   end
   % read the station data 
   [stationLon,stationLat,bang1,stationid,bang2,agency,bang3,description,bang4,datum] = textscan(stationFileID,'%f %f %s %s %s %s %s %s %s %s') % lo la !  id !  ag !  ds !  da 
   status = fclose(stationFileID)
   if status==-1
      error(['offsetSurfaceGen.m: Failed to close station file ' filstations '.'])
   end
   nstat=numel(stationid); % compute number of stations
else
   error('offsetSurfaceGen.m: The refwlmode parameter must be set to 0, 1, or 2.')
end
%
% Load in offshore points if that's the chosen mode of operation
if offblendmode==1
    load(filfaroff)         %has variable faroffpnts   that's a *x2 array of x-y data
    load(filinl)            %has variable inlpnts      that's a *x2 array of x-y data
end
if any(offshorepointmode==[2,3,4])
    load(filoffshfixpnts)   %has variable offshfixpnts that's a *x2 array of x-y data
end
%
% Ansys
disp('INFO: offsetSurfaceGen.m: Computing offset surface.')
%
% Define input points
xpin=[dat(:).lon];
ypin=[dat(:).lat];
zpin=zeros(1,numel(dat));
for cnt=1:nstat
   zpin(cnt)=mean([dat(cnt).wl])-refwl(cnt);
end
%
% Add offshore points to dataset, if desired
if offshorepointmode==0                 %no offshore points
   xp=xpin;
   yp=ypin;
   zp=zpin;
elseif offshorepointmode==1             %circles of points
   cir0=[(max(xpin)+min(xpin))/2,(max(ypin)+min(ypin))/2];
   cirr=sqrt(max((cir0(1)-xpin).^2+(cir0(2)-ypin).^2))+circledist;
%    xp=[xp,cirr.*cos(0:0.1:2*pi)+cir0(1)];
%    yp=[yp,cirr.*sin(0:0.1:2*pi)+cir0(2)];
%    xp=[xp,cirr.*cos(0:0.1:2*pi)+cir0(1),1.2.*cirr.*cos(0:0.1:2*pi)+cir0(1)];
%    yp=[yp,cirr.*sin(0:0.1:2*pi)+cir0(2),1.2.*cirr.*sin(0:0.1:2*pi)+cir0(2)];
   xpbnd=[cirr.*cos(0:0.1:2*pi)+cir0(1),1.2.*cirr.*cos(0:0.1:2*pi)+cir0(1),1.4.*cirr.*cos(0:0.1:2*pi)+cir0(1),1.6.*cirr.*cos(0:0.1:2*pi)+cir0(1)];
   ypbnd=[cirr.*sin(0:0.1:2*pi)+cir0(2),1.2.*cirr.*sin(0:0.1:2*pi)+cir0(2),1.4.*cirr.*sin(0:0.1:2*pi)+cir0(2),1.6.*cirr.*sin(0:0.1:2*pi)+cir0(2)];
%    xpbnd=[cirr.*cos(0:0.1:2*pi)+cir0(1),1.1.*cirr.*cos(0:0.1:2*pi)+cir0(1),1.2.*cirr.*cos(0:0.1:2*pi)+cir0(1),1.3.*cirr.*cos(0:0.1:2*pi)+cir0(1)];
%    ypbnd=[cirr.*sin(0:0.1:2*pi)+cir0(2),1.1.*cirr.*sin(0:0.1:2*pi)+cir0(2),1.2.*cirr.*sin(0:0.1:2*pi)+cir0(2),1.3.*cirr.*sin(0:0.1:2*pi)+cir0(2)];
%    xpbnd=[cirr.*cos(0:0.1:2*pi)+cir0(1),1.5.*cirr.*cos(0:0.1:2*pi)+cir0(1)];
%    ypbnd=[cirr.*sin(0:0.1:2*pi)+cir0(2),1.5.*cirr.*sin(0:0.1:2*pi)+cir0(2)];
   zpbnd=zeros(1,numel(xpbnd));
   xp=[xpin,xpbnd];
   yp=[ypin,ypbnd];
   zp=[zpin,zpbnd];
elseif any(offshorepointmode==[2,3])    %user-specified offshore points, loaded from filoffshfixpoints
    xp=[xpin,offshfixpnts(:,1).'];
    yp=[ypin,offshfixpnts(:,2).'];
    zp=[zpin,offshfixpnts(:,3).'];
end
% 
% Define x-y coordinate bounds and arrays based on data bounds and resolution
xvlo=min(xp)-1;%min(xpin)-2;
xvhi=max(xp)+1;%max(xpin)+2;
yvlo=min(yp)-1;%min(ypin)-2;
yvhi=max(yp)+1;%max(ypin)+2;
% xv=linspace(xvlo,xvhi,rd.npntgrid);
% yv=linspace(yvlo,yvhi,rd.npntgrid);
xv=linspace(xvlo,xvhi,ceil((xvhi-xvlo)/approxres)+1);
yv=linspace(yvlo,yvhi,ceil((yvhi-yvlo)/approxres)+1);
nxv=numel(xv);
nyv=numel(yv);
%
%
if offshorepointmode==3
    xp=[xp, linspace(xvlo,xvhi,10), repmat(xv(end),1,12),   linspace(xvhi,xvlo,10), repmat(xv(1),1,12)];
    yp=[yp, repmat(yv(1),1,10),     linspace(yvlo,yvhi,12), repmat(yv(end),1,10),   linspace(yvhi,yvlo,12)];
    zp=[zp,zeros(1,numel(xp)-numel(zp))];
end

[rd.zg,xg,yg]=RegularizeData3D(xp,yp,zp,xv,yv,'interp',rd.interp,'smoothness',rd.smoothness,...
                               'solver',rd.solver,'extend','warning','tilesize',inf);
[oi.zg,erroi]=optimalInterp(xp.',yp.',zp.',xg(:),yg(:),oi.Lx,oi.Ly,oi.obsNoise);oi.zg=reshape(oi.zg,size(xg));
%
%
%% Blend interpolants to zero (or other specified value)
%In the area between the inner and outer bounding lines, for each vertex,
%compute the shortest distance between the point and each line and use that
%as the weighting function to have its value go from whatever it was on the
%first line to zero as you reach the second line.  p_poly_dist.m,
%downloaded from:
%https://www.mathworks.com/matlabcentral/fileexchange/12744-distance-from-points-to-polyline-or-polygon
%will get you the distance calculations.  inpolygon (built-in) figures out
%which ones are inside.  Steps:

%1. For each vertex, see if it's within the polygon formed by the two
%   lines.  If so, proceed, if not, skip.
%2. Calculate shortest distance between vertex and each line
%3. Calculate the value of the surface at that nearest point on the inner
%   line.  
%4. Calculate the value at the current lattice point as the weighted mean
%   of the value at the nearest inner line point (and its distance) and the
%   value of zero assumed for the outer line point (and its distance).  
%5. Define the value at all vertices outside of the polygon created by the
%   outer line and the inland line as zero.  
%You now have an updated lattice of values ready for bi-whatever
%interpolation that gives a smooth answer that goes to zero.  
if offblendmode==1
    disp('INFO: offsetSurfaceGen.m: Blending interpolants to zero or other specified value.')   
    %Make big polygon that surrounds all non-default points
    if all(faroffpnts(1,:)==inlpnts(end,:)&faroffpnts(end,:)==inlpnts(1,:))
        nondefpoly=[faroffpnts;inlpnts(2:end-1,:)];
    elseif all(faroffpnts(1,:)==inlpnts(1,:)&faroffpnts(end,:)==inlpnts(end,:))
        nondefpoly=[faroffpnts;inlpnts(end-1:-1:2,:)];
    else
        error('The faroff and inl points don''t share common endpoints')
    end
    %Make smaller polygon for area where blending of interpolated values is done
    if all(faroffpnts(1,:)==offshfixpnts(end,1:2)&faroffpnts(end,:)==offshfixpnts(1,1:2))
        blendpoly=[faroffpnts;offshfixpnts(2:end-1,1:2)];
    elseif all(faroffpnts(1,:)==offshfixpnts(1,1:2)&faroffpnts(end,:)==offshfixpnts(end,1:2))
        blendpoly=[faroffpnts;offshfixpnts(end-1:-1:2,1:2)];
    else
        error('The faroff and offshfix points don''t share common endpoints')
    end
    
    %find points within polygons
    defpnts=~inpolygon(xg,yg,nondefpoly(:,1),nondefpoly(:,2));  %points where default value should be applied
    blendpnts=inpolygon(xg,yg,blendpoly(:,1),blendpoly(:,2));   %points where blending should be applied
    %set points outside non-default area to default value
    rd.zg(defpnts)=outsidedefaultz;
    oi.zg(defpnts)=outsidedefaultz;
    %blend points in the blending area using a simple distance-weighted average
    for cnt=1:numel(xg)
        if blendpnts(cnt)
            distin=p_poly_dist(xg(cnt),yg(cnt),offshfixpnts(:,1),offshfixpnts(:,2),0);  %distance to inner line
            distout=p_poly_dist(xg(cnt),yg(cnt),faroffpnts(:,1),faroffpnts(:,2),0);     %distance to outer line
            oi.zg(cnt)=(oi.zg(cnt).*distout+outsideblendz.*distin)./(distin+distout);
            rd.zg(cnt)=(rd.zg(cnt).*distout+outsideblendz.*distin)./(distin+distout);
        end
    end
end
%
%% Interpolate to mesh
if interptomesh==1
    disp('INFO: offsetSurfaceGen.m: Interpolating offset surface to mesh.')
    if strcmp(meshinterpmethod,'rd')
        zmesh=interp2(xg,yg,rd.zg,grd.x,grd.y,'linear',meshdefaultz);
    elseif strcmp(meshinterpmethod,'oi')
        zmesh=interp2(xg,yg,oi.zg,grd.x,grd.y,'linear',meshdefaultz);
    end
    %   
    % write out file
    if any(writeoutfil==[1,-63,63])
        disp('INFO: offsetSurfaceGen.m: Saving offset surface to file.')
        disp('Saving out')
        fidout=fopen(filout,'w');
        if writeoutfil==1
            fprintf(fidout,'header\n');
            fprintf(fidout,'%f    !time interval\n',timeinterval);
            fprintf(fidout,'%f    !default value\n',meshdefaultz);
            fprintf(fidout,'%i  %f\n',[nondefaultvals,zmesh(nondefaultvals)].');
        elseif writeoutfil==-63
            nondefaultvals=find(zmesh~=meshdefaultz);
            fprintf(fidout,'header\n');
            fprintf(fidout,'1 %i 1 %f 1\n',grd.np,timeinterval);
            fprintf(fidout,'1 1 %i %f\n',numel(nondefaultvals),meshdefaultz);
            fprintf(fidout,'%i  %f\n',[nondefaultvals,zmesh(nondefaultvals)].');
        elseif writeoutfil==63
            meshdefaultz=-99999;
            nondefaultvals=find(zmesh~=meshdefaultz);
            fprintf(fidout,'header\n');
            fprintf(fidout,'1 %i -99999.0 1 1\n',grd.np);
            fprintf(fidout,'%f -99999\n',stopSeconds);
            fprintf(fidout,'%i  %f\n',[nondefaultvals,zmesh(nondefaultvals)].');
        else
            error('writeoutfil must be set to either 1, 63, or -63 if interptomesh is set to 1')
        end
        fclose(fidout);
    end
end
%
%% Plots
if dographs==1
    thecoast=shaperead(['C:\Users\T\Documents\Data\GIS\DetailedAtlGulfCounty']);
%     load('C:\Users\taylor_asher\Documents\Research\MeanWaterLevelOffsetting\dtl_cnty.mat')
    
    disp('Plotting')
    dockit=@()set(gcf,'WindowStyle','docked');
    latorg=(min(ypin)+max(ypin))/2*pi/180;
    lonorg=(min(xpin)+max(xpin))/2*pi/180;
    deltalatlon=0.0001*pi/180;
    al=cos(latorg)*cos(latorg)*sin(deltalatlon/2).^2;
    ap=sin(deltalatlon/2).^2;
    latlonaspectratio=atan2(sqrt(al),sqrt(1-al))./atan2(sqrt(ap),sqrt(1-ap));   %lat/lon aspect ratio for plot dimensions
    
    
    figure
    dockit()
    set(gcf,'WindowStyle','docked')
    hax1=axes;
    hpc1=pcolor(xg,yg,rd.zg);
    set(hpc1,'edgecolor','none')
    shading interp
    colormap(parula(256))
    colorbar
    hold on
    hsc1=scatter(xp,yp,40,zp,'filled','markeredgecolor','k');
%     htx1=text(xpin,ypin,num2str(zpin.',3),'VerticalAlignment','baseline');
    htx1=text(xpin,ypin,num2str((interp2(xg,yg,rd.zg,xpin,ypin,'linear')-zpin).','%-5.2f'),'VerticalAlignment','middle','HorizontalAlignment','right');
    daspect([1,latlonaspectratio,1])
    if offblendmode==1
        plot(faroffpnts(:,1),faroffpnts(:,2),'k')
        plot(inlpnts(:,1),inlpnts(:,2),'color',[0.3,0.3,0.3])
    end
%     for cnt=1:length(thecoast.ncst)
%        line(thecoast.ncst{cnt}(:,1),thecoast.ncst{cnt}(:,2),'Color','w')
    for cnt=1:length(thecoast)
        line(thecoast(cnt).X,thecoast(cnt).Y,'Color','w')
    end
    uistack(hsc1,'top'),uistack(htx1,'top')
    title(['RegularizeData3D, smooth=',num2str(rd.smoothness),' interp=',rd.interp,' approxres=',num2str(rd.approxres)])
    
    figure
    dockit()
    hax2=axes;
%     imagesc(xv,yv,zg)
%     set(gca,'YDir','normal')
%     surf(xg,yg,zg,'edgecolor','none')
    hpc2=pcolor(xg,yg,oi.zg);
    set(hpc2,'edgecolor','none')
    shading interp
    colormap(parula(256))
    colorbar
    hold on
    hsc2=scatter(xp,yp,40,zp,'filled','markeredgecolor','k');
%     htx2=text(xpin,ypin,num2str(zpin.',3),'VerticalAlignment','baseline');
    htx2=text(xpin,ypin,num2str((interp2(xg,yg,oi.zg,xpin,ypin,'linear')-zpin).','%-5.2f'),'VerticalAlignment','middle','HorizontalAlignment','right');
    daspect([1,latlonaspectratio,1])
    if offblendmode==1
        plot(faroffpnts(:,1),faroffpnts(:,2),'k')
        plot(inlpnts(:,1),inlpnts(:,2),'color',[0.3,0.3,0.3])
    end
%     for cnt=1:length(thecoast.ncst)
%        line(thecoast.ncst{cnt}(:,1),thecoast.ncst{cnt}(:,2),'Color','w')
    for cnt=1:length(thecoast)
        line(thecoast(cnt).X,thecoast(cnt).Y,'Color','w')
    end
    uistack(hsc2,'top'),uistack(htx2,'top')
    title(['Optimal Interpolation, Lx=',num2str(oi.Lx),' obsNoise=',num2str(oi.obsNoise)])
    
%     optional axis setup...
%     disp('hit a key to set color and axis bounds')
%     pause
%     caxis(hax1,[0.0,0.5])
%     caxis(hax2,[0.0,0.5])
%     % axis(hax1,[-90,-65,22,42])
%     % axis(hax2,[-90,-65,22,42])
end
