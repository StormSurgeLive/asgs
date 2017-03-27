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
%clearvars -except grd %jgf: this erases variables specified on the command line
%clc
mypath='.';
%
% jgf: List of command line options required
% parameters
%   dodownload
%   refwlmode
%   dographs
%   approxres
%   offblendmode
%   interptomesh
%   stopafterdownload
% files
%   filfaroff
%   filinl
%   filwldata
%   filrefwl
%   filmesh
%   filout
%   filoffshfixpnts
%-----------------------------------------------------------------------
%        I N I T I A L I Z E   P A R A M E T E R S
%-----------------------------------------------------------------------
% initialize parameters to default values
dodownload=1;   % =1 to download gage data, =0 to load differences from a file
refwlmode=1;    % water level comparison: 0=0.0+const., 1=load in file with adcirc avg at gages
dographs=0;     % whether to plot stuff
approxres=0.04; % x-y grid resolution for interpolation
offshorepointmode=3; % whether to close off with exterior points 0=no, 1=circles of points, 2=offshore points, 3=2 plus attempt to draw a box of zeroes
offblendmode=1; % whether to blend interpolant surface in a specific area (0=no, 1=yes linear)
interptomesh=1; % whether to calculate values on mesh (1=yes)
stopafterdownload=0; %whether to stop the program after downloading data (in order to save the data out to a file)
writeoutfil=63;  % whether to write oi output file (1=yes in Jason's format, -63=yes as a sparse .63 file, 63=yes as a full .63 file)
constrefwl=0;   % constant reference water level value
%-----------------------------------------------------------------------
%        I N I T I A L I Z E   F I L E   N A M E S
%-----------------------------------------------------------------------
filfaroff='FarOffV1.mat';  % file containing outer line for blending
filinl='InlandV1.mat';     % file containing other line for defining bounds of non-zeroed areas
filrefwl=[mypath,'/adcircAvg.dat'];  % ref (adcirc) wl file
% file with measured wl data
% jgf: question: "Taylor the comment and file name imply that these are measured
% water levels but I think you mentioned in an email that this is 
% actually water level differences. Can you clarify?"
filwldata=[mypath,'/MeasWLsForTwoM2CycleMatthewOct5_23colon15Z_OIRun1.mat'];    
filmesh='./fort.14';          % mesh file
filoffshfixpnts=[mypath,'/CloseOffV1.mat'];% for bounding offshore/inland values
filout=[mypath,'/oi_surface.dat'];   % oi output file name
%-----------------------------------------------------------------------
%      I N I T I A L I Z E   G A G E   P A R A M E T E R S
%-----------------------------------------------------------------------
% initialize parameters related to gage data
% TODO: set start and stop dates/times on command line
% startSeconds=2902188; stopSeconds=2991600; % matthew adv20 2xM2
%startSeconds=2923788; stopSeconds=3013200; % matthew adv21 2xM2
%startSeconds=2945388; stopSeconds=3034800; % matthew adv22 2xM2
%startSeconds=2966988; stopSeconds=3056400; % matthew adv23 2xM2
% 2x m2 tidal cycles
%filrefwl=[mypath,['/col5.filtered.' num2str(adv) '.adcircAvg.dat']];  % ref (adcirc) wl file
% 14x m2 tidal cycles
%filrefwl=[mypath,['/col5.filtered.' num2str(adv) '.14x.adcircAvg.dat']];  % ref (adcirc) wl file
% 2x m2 tidal cycles without cape fear river
%filrefwl=[mypath,['/col5.filtered.' num2str(adv) '.nocfr.adcircAvg.dat']];  % ref (adcirc) wl file
% 2x m2 tidal cycles without springmaid pier
filrefwl=[mypath,['/col5.filtered.' num2str(adv) '.nosmp.adcircAvg.dat']];  % ref (adcirc) wl file
%
start=datenum([2016,08,29,12,00,00])+startSeconds/86400;
stop=datenum([2016,08,29,12,00,00])+stopSeconds/86400;
% TODO: load stations from file
%stationnum=[8724580;8723970;8723214;8722670;8721604;8720218;8720030;8670870;8665530;8661070;8658120;8658163;8656483;8654467;8651370;8638863;8638610;8637689;8635750;8577330;8575512;8574680;8573364;8632200;8570283];
% without the cape fear river
%stationnum=[8724580;8723970;8723214;8722670;8721604;8720218;8720030;8670870;8665530;8661070;8658163;8656483;8654467;8651370;8638863;8638610;8637689;8635750;8577330;8575512;8574680;8573364;8632200;8570283];
% without springmaid pier
stationnum=[8724580;8723970;8723214;8722670;8721604;8720218;8720030;8670870;8665530;8658120;8658163;8656483;8654467;8651370;8638863;8638610;8637689;8635750;8577330;8575512;8574680;8573364;8632200;8570283];
stationid=mat2cell(num2str(stationnum),ones(numel(stationnum),1),7);
%     stationname={'Springmaid';'Wilmington';'Wrightsville';'Beaufort';'Hatteras';'Oregon' ;'Duck'   ;'ChesaBridge';'Sewells'};
datatype='VerifiedSixMinute';           %'PreliminarySixMinute', 'VerifiedHourlyHeight', etc.
units='Meters';
vertdatum='MSL';
timefmt='yyyy-mm-ddTHH:MM:SSZ';
nstat=numel(stationnum);
%
% For bounding offshore/inland values
if offshorepointmode==1
    circledist=4;  % how far out the circle should be (same units as input data)
end
%
% initialize blending of interpolant surface in a specific area if specified
filfaroff='FarOffV1.mat';       % outer line for blending
filinl='InlandV1.mat';          % other line for defining bounds of non-zeroed areas
outsideblendz=0;                % value to blend to at the faroff boundary
outsidedefaultz=outsideblendz;  % default elevation value for areas that aren't to be interpolated
%
% For RegularizeData3D
rd.smoothness=0.001;    %weight between smoothness and fitting data (lower equals less smooth)
rd.interp='bilinear';   %method to interpolate data (triangle, bicubic, bilinear, nearest)
rd.solver='\';          %'%solver method (normal, \, symlq, lsqr)
% rd.npntgrid=200;      %number of points (in each direction) for underlying gridding of data
rd.approxres=approxres; %approximate resolution (in each direction) for underlying gridding of data
%
% For optimalInterp
oi.Lx=1.0;              %radius of influence in x (help says Gaussian function, so this is prob a stdev or var)
oi.Ly=oi.Lx;            %same, in y
oi.obsNoise=0.001;      %signal to noise ratio
%
% For mesh
meshdefaultz=outsidedefaultz;           %default value for mesh
meshinterpmethod='oi';                  %which method to use ('oi' or 'rd')
if any(writeoutfil==[1,-63,63])
   timeinterval=0;
end
%
%% Error checking (this prob needs to be filled out more)
if offblendmode==1&&~any(offshorepointmode==[2,3])
    error('offblendmode=1 and offshorepointmode=2 or =3')
end
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
disp('Loading data')
if dodownload==1
    nodata=zeros(nstat,1);
    for cnt=1:nstat
%        tmp=GetNosWaterLevelViaSOSv4_8('station',stationid{cnt},'start',start,'stop',stop,'datatype',datatype,'units',units,'vertdatum',vertdatum);
        tmp=GetNosWaterLevelViaSOSv4_6('station',stationid{cnt},'start',start,'stop',stop,'datatype',datatype,'units',units,'vertdatum',vertdatum);
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
    load(filwldata)
    nstat=numel(dat);
end
%
% Set or load reference water level data
if refwlmode==0
    refwl=zeros(nstat,1)+constrefwl;
elseif refwlmode==1
    refwl=load(filrefwl,'-ascii');
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
disp('Analyzing')
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
    disp('Blending')
    
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
    disp('Interpolating to mesh')
    if strcmp(meshinterpmethod,'rd')
        zmesh=interp2(xg,yg,rd.zg,grd.x,grd.y,'linear',meshdefaultz);
    elseif strcmp(meshinterpmethod,'oi')
        zmesh=interp2(xg,yg,oi.zg,grd.x,grd.y,'linear',meshdefaultz);
    end
    %   
    % write out file
    if any(writeoutfil==[1,-63,63])
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
            error('writeoutfil must be set to either 1 or -63 if interptomesh is set to 1')
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
