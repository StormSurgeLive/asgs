function [D,outtimefmt,turl]=GetNosWaterLevelViaSOSv4_5(varargin)
%Get water levels from NOS SOS service
%Call as: [D,outtimefmt,turl]=GetNosWaterLevelViaSOS(p1,v1,...,...);
%where p1,v1 are name,value pairs.  
%
%Outputs
%   D is a struct with all data
%   outtimefmt is the datestring format of output dates
%   turl is the last URL queried for data, helpful for debugging
%
%v1:    Brian Blanton
%       Renaissance Computing Institute
%       UNC-Chapel Hill
%       Brian_Blanton@Renci.Org
%v2-v4: Taylor Asher
%Last updated Jan. 2 2017
%
%
%Features
%   Handles arbitary date ranges
%   Can input a single gage, all available gages, or gages in a lat-lon box
%   Allows output of files with user-specified or auto-generated file names
%   User-specified output units and datums
%
%Wishlist
%   Modularize stuff into subfunctions
%   Add ability to download datum info
%   Add ability to download harmonic constituent info
%
%
%Options:
%    http://opendap.co-ops.nos.noaa.gov/ioos-dif-sos/ClientGetter?p=1
%Active water level stations via SOS, this is NOT a list of all stations
%with data, but these may be the only ones you can get data for from SOS):
%    http://opendap.co-ops.nos.noaa.gov/ioos-dif-sos/ClientGetter?p=6
%To test urls:
%    http://opendap.co-ops.nos.noaa.gov/ioos-dif-sos-test/
%
%
%Other options for getting NOAA data:
%http://opendap.co-ops.nos.noaa.gov/axis/
%http://opendap.co-ops.nos.noaa.gov/ioos-dif-sos/
%http://tidesandcurrents.noaa.gov/api/
%% Help
if nargin==0
    disp('Help:  ')
    disp('GetNosWaterLevelViaSos station STATN_NAME [OPTIONS]')
    disp(' station STATN_NAME - STATN_NAME can either be a string containing the 7-digit station ')
    disp('                      ID of a NOAA station, or it can be one of the keywords All or ')
    disp('                      WaterLevelActive.  All means all available gages will be acquired, ')
    disp('                      WaterLevelActive means available active gages will be acquired.  ')
    disp('                      If a lat-lon box is given via the bbox option, either All or ')
    disp('                      WaterLevelActive must be specified.')
    disp('')
    disp('')
    disp('All options are optional, and are specified as name-value pairs.  ')
    disp('Options:  ')
    disp(' start TSTART - TSTART can be a datevector, a datenumber, or a datestring in ')
    disp('                yyyy-mm-ddTHH:MM:SSZ format, where T and Z are constant ')
    disp('                characters).  Default is 30 days prior to now.  Input time is in GMT.  ')
    disp(' stop TSTOP - TSTOP can be a datevector, a datenumber, or a datestring in ')
    disp('              yyyy-mm-ddTHH:MM:SSZ format (T and Z are constant characters).  ')
    disp('              Default is now.  TSTOP is in GMT.  ')
    disp(' datatype TYPE - Type of data to be downloaded.  TYPE can be any of:  ')
    disp('                 PreliminarySixMinute (default), VerifiedSixMinute, VerifiedHourlyHeight,')
    disp('                 VerifiedHighLow, VerifiedDailyMean, SixMinuteTidePredictions, ')
    disp('                 HourlyTidePredictions, HighLowTidePredictions.  ')
    disp(' units UNIT - Output data units, UNIT can be Meters (default) or Feet.  ')
    disp(' vertdatum DATUM - Output data vertical datum.  Options may vary by NOS gage, options ')
    disp('                   include:  MSL (default), IGLD, MHHW, MHW, MLLW, MLW, MTL, NAVD88, ')
    disp('                   STND (station datum).  ')
    disp(' timezone TZ - Output data time zone.  TZ can be GMT (default) or LST.  Note ')
    disp('               that this DOES NOT affect the time zone of the input start and ')
    disp('               stop times.  ')
    disp(' bbox BOX - Bounding lat-lon box to acquire data for.  BOX is a 1x4 numeric array ')
    disp('            of the form [min_lon,min_lat,max_lon,max_lat].  If bbox is specified, ')
    disp('            station All or station WaterLevelActive must be specified.  ')
    disp(' writeout WRITE - Option to write out data file(s).  Options for WRITE are:  ')
    disp('                    tab - Tab-delimited ASCII file')
    disp('                    mat - Matlab .mat file')
    disp('                    no - No file output (default)')
    disp(' fileout FOUT - Name of output file.  Only available if writeout is enabled.  ')
    disp('                FOUT can be any valid file name and path.  If FOUT is ? then the ')
    disp('                user will be prompted to define the output file(s) via a GUI.  ')
    disp('                If a file name is specified, but there are multiple stations, ')
    disp('                then the file names are appended with _1, _2, etc.  If FOUT is a ')
    disp('                directory, then automatic file naming will be used but files are ')
    disp('                placed in the specified directory.  If this option is not ')
    disp('                specified but writeout is enabled, automatic file naming is used ')
    disp('                and output files are placed in Matlab''s current directory.  ')
    disp('')
    disp('Ex: [datastruct,outtimefmt]=...')
    disp('    GetNosWaterLevelViaSOSv4_4(''station'',''All'',...')
    disp('                               ''Start'',''1974-08-01T00:00:00Z'',...')
    disp('                               ''Stop'',''1976-07-01T00:00:00Z'',...')
    disp('                               ''DataType'',''VerifiedHourlyHeight'',...')
    disp('                               ''bbox'',[-78,33,-75,37],...')
    disp('                               ''writeout'',''tab'')')
    disp('')
    disp('')
    disp('Active water level stations via SOS, this is NOT a list of all NOS stations with data, ')
    disp('but these may be the only ones you can get data for from SOS):  ')
    disp('http://opendap.co-ops.nos.noaa.gov/ioos-dif-sos/ClientGetter?p=6')
    disp('')
    disp('To test URLs: http://opendap.co-ops.nos.noaa.gov/ioos-dif-sos-test/')
    disp('')
    disp('Output struct fields:  ')
    disp('  station - 7-digit numeric ID of station')
    disp('  fullstation - lengthy station name output by NOS')
    disp('  lon - longitude')
    disp('  lat - latitude')
    disp('  datum - vertical datum')
    disp('  timezone - time zone')
    disp('  units - data units (meters, feet)')
    disp('  datatype - type of dataset')
    disp('  start - date string of date/time of first data values encountered')
    disp('  stop - date string of date/time of last data values encountered')
    disp('  requeststart - date string of date/time of first requested data')
    disp('  requeststop - date string of date/time of last requested data')
    disp('  navail - number of data values found')
    disp('  nmiss - number of data values missing, including ones before/after first/last measurement')
    disp('  ngap - number of data values missing between first and last measurement')
    disp('  t - times of data values as datenumbers')
    disp('  tstr - times of data values as datestrings with format outtimefmt')
    disp('  wl - data values (i.e. water levels)')
    return
end



%% Base
argsin=varargin;                    %save a copy of the input arguments (for debug purposes)
timefmt='yyyy-mm-ddTHH:MM:SSZ';     %date string format for data to/from NOS
outtimefmt='yyyy-mm-dd HH:MM:SS';   %date string format for data written to file



%% Test cases
% clearvars -except timefmt outtimefmt; home;
% varargin=[];
% % Station='8651370';  % Duck NC
% % Station='8665530';  % Charleston SC
% % Station='8659182';  % Oak Island, NC (historical station active Apr. '96-97
% Station='All';  % all
% % Station='WaterLevelActive';  %
% % Start=datestr(now-30,timefmt); %Default is 30 day window from "now"
% % Stop=datestr(now,timefmt);
% % Start=datestr([2016,04,14,00,00,00],timefmt);
% % Stop=datestr([2016,05,15,00,00,00],timefmt);
% % Start=datestr([1996,06,01,00,00,00],timefmt);
% % Stop=datestr([1996,07,01,00,00,00],timefmt);
% % Start=datestr([1992,06,01,00,00,00],timefmt);
% % Stop=datestr([1993,07,01,00,00,00],timefmt);
% Start=datestr([1974,08,01,00,00,00],timefmt);       %good for checking variable gage availability for 35-37 deg. latitudes
% Stop=datestr([1978,07,01,00,00,00],timefmt);
% % Start=datestr([1922,06,01,00,00,00],timefmt);
% % Stop=datestr([1922,07,01,00,00,00],timefmt);
% % DataType='SixMinuteTidePredictions';
% % DataType='VerifiedSixMinute';
% DataType='VerifiedHourlyHeight';
% % DataType='VerifiedDailyMean';
% Units='Meters';
% VertDatum='MSL';
% % bbox=[-76.5,44,-74.6,45];
% bbox=[-78,33,-75,37];
% writeout='tab';
%%%%Big data download test:  
% Station='all'
% VertDatum='MHW'
% writeout='tab'
% DataType='verifiedhourlyheight'
% Start='2010-01-01T00:31:15Z'
% Stop='2012-01-06T14:00:00Z'
%%%%Bounding box test that caused some interesting issues (try removing the datatype specification, too)
% Station='All',Start='2016-09-26T00:00:00Z',Stop='2016-10-10T12:00:00Z',bbox=[-85,24,-74,40],DataType='VerifiedHourlyHeight'


%% Input Parsing and Defaults
%Strip off propertyname/value pairs in varargin
k=1;
while k<length(varargin),
    switch lower(varargin{k}),
        case 'station',
            Station=varargin{k+1};
            varargin([k k+1])=[];
        case 'start',
            Start=varargin{k+1};
            varargin([k k+1])=[];
            if isnumeric(Start)==1              %assume datenum time, put into timefmt
                Start=datestr(Start,timefmt);
            end
        case 'stop',
            Stop=varargin{k+1};
            varargin([k k+1])=[];
            if isnumeric(Stop)==1               %assume datenum time, put into timefmt
                Stop=datestr(Stop,timefmt);
            end
        case 'datatype',
            DataType=varargin{k+1};
            varargin([k k+1])=[];
        case 'units',
            Units=varargin{k+1};
            varargin([k k+1])=[];
        case 'vertdatum',
            VertDatum=varargin{k+1};
            varargin([k k+1])=[];
        case 'timezone',
            Timezone=varargin{k+1};
            varargin([k k+1])=[];
        case 'bbox',
            bbox=varargin{k+1};
            varargin([k k+1])=[];
        case 'writeout'
            writeout=varargin{k+1};
            varargin([k k+1])=[];
        case 'fileout'
            fileout=varargin{k+1};
            varargin([k k+1])=[];
        otherwise
            error(['Input not recognized: ',varargin{k}])
    end
end


%Setting defaults
if exist('Stop','var')==0&&exist('Start','var')==0
    Stop=datestr(datetime('now','TimeZone','GMT','Format','uuuu-MM-dd''T''HH:mm:ss''Z'),timefmt);
    Start=datestr(datenum(Stop,timefmt)-30,timefmt);  %30 days prior to stop time
end
if exist('DataType','var')==0
    DataType='PreliminarySixMinute';
end
if exist('Units','var')==0
    Units='Meters';
end
if exist('VertDatum','var')==0
    VertDatum='MSL';
end
if exist('Timezone','var')==0
    Timezone='GMT';
end
if exist('writeout','var')==0
    writeout='no';
end



%% Error checks
%Errors
%Station
if exist('Station','var')==0
    error('Station variable not specified')
elseif isnan(str2double(Station))&&~strcmpi(Station,'All')&&...
        ~strcmpi(Station,'WaterLevelActive')     %tests if Station isn't numeric since all NOAA station IDs are a number
    error(['Unrecognized Station: ',Station])
end
%Start/Stop
if exist('Start','var')==0&&exist('Stop','var')==1
    error('Stop time specified, but no start time.')
elseif exist('Stop','var')==0&&exist('Start','var')==1
    error('Start time specified, but no stop time.')
else
    try
        datevec(Start,timefmt)
    catch err
        warning(['Start time input seems to be improperly formatted, format should be: ',timefmt])
        warning(['Start value input is:  ',Start])
        warning('Error returned by Matlab:  ')
        rethrow(err)
    end
    try
        datevec(Stop,timefmt)
    catch err
        warning(['Stop time input seems to be improperly formatted, format should be: ',timefmt])
        warning(['Stop value input is:  ',Stop])
        warning('Error returned by Matlab:  ')
        rethrow(err)
    end
end
%DataType
if ~strcmpi(DataType,'VerifiedSixMinute')&&~strcmpi(DataType,'PreliminarySixMinute')&&...
        ~strcmpi(DataType,'SixMinuteTidePredictions')&&~strcmpi(DataType,'VerifiedHourlyHeight')&&...
        ~strcmpi(DataType,'VerifiedHighLow')&&~strcmpi(DataType,'HourlyTidePredictions')&&...
        ~strcmpi(DataType,'HighLowTidePredictions')&&~strcmpi(DataType,'VerifiedDailyMean')
    error(['Unrecognized DataType: ',DataType])
end
%Units
if ~strcmpi(Units,'Meters')&&~strcmpi(Units,'Feet')
    error(['Unrecognized Units: ',Units])
end
%VertDatum
if ~strcmpi(VertDatum,'IGLD')&&~strcmpi(VertDatum,'MHHW')&&~strcmpi(VertDatum,'MHW')&&...
        ~strcmpi(VertDatum,'MSL')&&~strcmpi(VertDatum,'MTL')&&~strcmpi(VertDatum,'MLW')&&...
        ~strcmpi(VertDatum,'MLLW')&&~strcmpi(VertDatum,'NAVD88')&&~strcmpi(VertDatum,'STND')
    error(['Unrecognized VertDatum: ',VertDatum])
end
%timezone
if ~strcmpi(Timezone,'GMT')&&~strcmpi(Timezone,'LST')
    error(['Unrecognized Timezone: ',Timezone])
end
%bbox
if exist('bbox','var')==1&&~(strcmpi(Station,'All')||strcmpi(Station,'WaterLevelActive'))
    error('If bbox is specified, then input Station must be specified as either "All" or "WaterLevelActive".')
end
if exist('bbox','var')==1&&(bbox(1)>bbox(3)||bbox(2)>bbox(4)||any(abs(bbox)>360))
    warning('Coordinates for bounding box seem wrong, should be longitudes and latitudes, formatted as [min_lon,min_lat,max_lon,max_lat]')
end
%writeout
if ~strcmpi(writeout,'tab')&&~strcmpi(writeout,'mat')&&~strcmpi(writeout,'no')
    error(['Unrecognized writeout: ',writeout])
end


%Warnings
if ~strcmpi(Timezone,'GMT')                 %options not yet programmed in
    warning(['No timezone but GMT has been tested, user specified: ',Timezone])
end



%% Derived inputs
%Set up parameters for reading in data in chunks
if strcmpi(DataType,'VerifiedSixMinute')||strcmpi(DataType,'PreliminarySixMinute')||strcmpi(DataType,'SixMinuteTidePredictions')
    timeinterval=1/24/10;            %data time interval in days
    if strcmpi(Station,'All')||strcmpi(Station,'WaterLevelActive')
        maxtimerange=1;              %max # of days that can be requested at once
    else
        maxtimerange=30;
    end
elseif strcmpi(DataType,'VerifiedHourlyHeight')||strcmpi(DataType,'VerifiedHighLow')||...
        strcmpi(DataType,'HourlyTidePredictions')||strcmpi(DataType,'HighLowTidePredictions')
    timeinterval=1/24;
    if strcmpi(Station,'All')||strcmpi(Station,'WaterLevelActive')
        maxtimerange=30;
    else
        maxtimerange=365;
    end
elseif strcmpi(DataType,'VerifiedDailyMean')
    timeinterval=1;
    if strcmpi(Station,'All')||strcmpi(Station,'WaterLevelActive')
        maxtimerange=365;
    else
        maxtimerange=3650;
    end
else
    error(['Have not yet coded for this DataType: ',DataType])
end

%Flag variables
if exist('bbox','var')==1
    dobbox=1;                   %flag indicating a bounding box is to be used
else
    dobbox=0;
end
if exist('writeout','var')==1&&~strcmpi(writeout,'no')
    dowriteout=1;               %flag indicating outputs are to be written to disk
else
    dowriteout=0;
end
if exist('fileout','var')==0
    autofileout=1;              %auto-generated file name(s)
    fopath='';                  %output directory
elseif isdir(fileout)==1
    autofileout=2;              %auto-generated file name(s) at specified directory
    fopath=fileout;             %output directory
elseif strcmp(fileout,'?')==1
    autofileout=0;              %ask for each file name via gui
else
    autofileout=-1;             %use input file name
    [fopath,foname,foext]=fileparts(fileout);
end

% %Determine if data needs to be downloaded in chunks
% if datenum(Stop,timefmt)-datenum(Start,timefmt)>maxtimerange
%     nchunk=ceil((datenum(Stop,timefmt)-datenum(Start,timefmt))/maxtimerange);   %# of chunks of data
% else
%     nchunk=1;
% end




%% Setup
fprintf('%s ==>> %s\n',Start,Stop)

%Setting up base URL and parameters.  This works by creating a URL with a
%bunch of special characters (e.g. "%s") that, later in the data download
%loop, are replaced with actual information.  That way most of the URL can
%be setup here.  
url=['http://opendap.co-ops.nos.noaa.gov/ioos-dif-sos/SOS?',...
    'service=SOS&',...                                                  % required
    'request=GetObservation&',...                                       % required
    'version=1.0.0&',...                                                % required
    'observedProperty=%s&',...                                          % required
    'offering=urn:ioos:%s:NOAA.NOS.CO-OPS:%s&',...                      % required
    'responseFormat=text%%2Fcsv&',...                                   % required
    'eventTime=%s/%s&',...
    'result=VerticalDatum%%3D%%3Durn:ioos:def:datum:%s&',...
    'dataType=%s&',...
    'unit=%s',...
    'timeZone=%s'...
    ];
if dobbox==1    %add bounding box info
    url=[url,'&featureOfInterest=BBOX:%s,%s,%s,%s'];
end

%Parsing data type options
if strcmpi(DataType,'SixMinuteTidePredictions')||strcmpi(DataType,'HighLowTidePredictions')||...
        strcmpi(DataType,'HourlyTidePredictions')
    ObsProp='sea_surface_height_amplitude_due_to_equilibrium_ocean_tide';
elseif strcmpi(DataType,'VerifiedHighLow')||strcmpi(DataType,'VerifiedHourlyHeight')||...
        strcmpi(DataType,'VerifiedSixMinute')||strcmpi(DataType,'PreliminarySixMinute')||...
        strcmpi(DataType,'VerifiedDailyMean')
    ObsProp='water_surface_height_above_reference_datum';
else
    error('how did you get here?')
end

%Parsing vertical datum options
if strcmpi(VertDatum,'NAVD88')
    datumurlstr='epsg::5103';
else
    datumurlstr=['noaa::',VertDatum];
end

%Parsing "offering" options
if strcmpi(Station,'All')||strcmpi(Station,'WaterLevelActive')
    Stationset='network';
else
    Stationset='station';
end

%Initialize fields to ensure the order of variables in the struct aren't changed
D.station='placeholder';        %station ID (7-digit number)
D.fullstation='placeholder';    %full-length station ID as reported by the SOS
D.lon='placeholder';            %station longitude
D.lat='placeholder';            %station latitude
D.datum='placeholder';          %vertical datum
D.timezone='placeholder';       %time zone
D.units='placeholder';          %units of data
D.datatype='placeholder';       %type of data
D.start='placeholder';          %date string of date/time of first data values encountered
D.stop='placeholder';           %date string of date/time of last data values encountered
D.requeststart='placeholder';   %date string of date/time of first requested data
D.requeststop='placeholder';    %date string of date/time of last requested data
D.navail='placeholder';         %number of data values found
D.nmiss='placeholder';          %number of data values missing, including ones before/after first/last measurement
D.ngap='placeholder';           %number of data values missing between first and last measurement
D.t='placeholder';              %times of data values as datenumbers
D.tstr='placeholder';           %times of data values as datestrings
D.wl='placeholder';             %data values (i.e. water levels)
% D.url='placeholder';            %cannot count on this variable if multiple requests are made



%% Data download loop
%Should  change this to work with date vectors to avoid roundoff error, 
%which can creep into datenumbers.  I also have found that Matlab (2015a 
%at least) has some sort of bug in datenum that causes it to take your time
%zone into account.  As a result, the UTC times keep getting shifted back 
%by 5 hours for me.  To avoid this, I found doing datenum(datevec(...)) 
%worked, so that has been implemented here.
curstop=datenum(datevec(Start,timefmt))-timeinterval;
cnt=0;
J=[];
while curstop<datenum(Stop,timefmt)
    cnt=cnt+1;
    curstart=curstop+timeinterval;                                      %start time for current chunk of data to be downloaded
    curstop=min(datenum(datevec(Stop,timefmt)),curstart+maxtimerange);  %stop time for current chunk of data to be downloaded
    disp('Downloading data for time range:  ')
    disp(['curstart= ',num2str(datevec(curstart))])         %as a rudimentary check on roundoff error
    disp(['curstop = ',num2str(datevec(curstop))])          %as a rudimentary check on roundoff error
    curstartstr=datestr(curstart,timefmt);
    curstopstr=datestr(curstop,timefmt);
    
    if dobbox==1    %add bounding box info
        turl=sprintf(url,ObsProp,Stationset,Station,curstartstr,curstopstr,datumurlstr,DataType,Units,Timezone,...
            num2str(bbox(1)),num2str(bbox(2)),num2str(bbox(3)),num2str(bbox(4)));
    else            %or don't
        turl=sprintf(url,ObsProp,Stationset,Station,curstartstr,curstopstr,datumurlstr,DataType,Units,Timezone);
    end
    
    %Download data
    [f]=websave(['tempdataGetNosWaterLevelViaSOS',num2str(cnt),'.csv'],turl);
%     [f,status]=urlwrite(turl,['tempdataGetNosWaterLevelViaSOS',num2str(cnt),'.csv']);
%     [f,status]=urlwrite(turl,'tempdataGetNosWaterLevelViaSOS.csv');
%     if status~=1
%         error('Failed to urlwrite.')
%     end
    
    %Load downloaded data
    warning('off','MATLAB:table:ModifiedVarnames');     %temporarily turn off annoying Matlab warning about how the variable names don't quite match those in the file
    T=readtable(f,'ReadVariableNames',true,'Delimiter',',');
    warning('on','MATLAB:table:ModifiedVarnames');
    if isempty(T)||length(T.Properties.VariableNames)==1
        fprintf(['Data request resulted in empty return. \nMay not be data for station/start/stop/etc combination. '...
            '\nSee contents of table T and %s for clues.\n'],f)
%         error('lets stop and look')   %debug line
    else
        delete(f)   %delete downloaded data file after reading
        
        J=[J;T];
    end
end
if isempty(J)==1
    warning('No data appears to have been encountered')
    D=[];
    return
end


     
%% Process downloaded data
allfulstatns=unique(J.station_id);                  %unique list of all stations, note that this sorts the values
nstatn=numel(allfulstatns);                         %number of unique stations
datumcheckfail=0;                                   %flag to make sure datum check failure is reported no more than once
D(nstatn).station=[];                               %preallocation
for cnt=1:nstatn
    curind=find(strcmp(J.station_id,allfulstatns{cnt}));    %index of all rows in J for the current station
    
    
    %Verifying/setting output datum
    datumcheck=J.datum_id{curind(1)}(find(J.datum_id{curind(1)}==':',1,'last')+1:end);
    if strcmp(datumcheck,'5103')
        datum='NAVD88';         %setting the value manually for NAVD88 since NOS codes it weirdly
    else
        datum=datumcheck;
    end
    if datumcheckfail==0&&~strcmpi(datum,VertDatum)
        datumcheckfail=1;
        warning('Input and output vert datums do not match, assuming output value')
        warning(['Input: ',VertDatum])
        warning(['Output: ',datum])
    end
    
    
    %Assign values and change datestring time formats from timefmt to
    %outtimefmt
    D(cnt).station=allfulstatns{cnt}(find(allfulstatns{cnt}==':',1,'last')+1:end);  %ID is the value after the last colon
    D(cnt).fullstation=allfulstatns{cnt};
    D(cnt).lon=J.longitude_degree_(curind(1));
    D(cnt).lat=J.latitude_degree_(curind(1));
    D(cnt).datum=datum;
    D(cnt).timezone=Timezone;
    D(cnt).units=Units;
    D(cnt).datatype=DataType;
    D(cnt).start=datestr(datevec(J.date_time{curind(1)},timefmt),outtimefmt);
    D(cnt).stop=datestr(datevec(J.date_time{curind(end)},timefmt),outtimefmt);
    D(cnt).requeststart=datestr(datevec(Start,timefmt),outtimefmt);
    D(cnt).requeststop=datestr(datevec(Stop,timefmt),outtimefmt);
    if strcmpi(DataType,'VerifiedSixMinute')||strcmpi(DataType,'PreliminarySixMinute')||...
       strcmpi(DataType,'VerifiedHourlyHeight')||strcmpi(DataType,'VerifiedHighLow')||...
       strcmpi(DataType,'VerifiedDailyMean')
        D(cnt).navail=numel(J.water_surface_height_above_reference_datum_m_(curind));
    elseif strcmpi(DataType,'SixMinuteTidePredictions')||strcmpi(DataType,'HourlyTidePredictions')||...
           strcmpi(DataType,'HighLowTidePredictions')
        D(cnt).navail=numel(J.sea_surface_height_amplitude_due_to_equilibrium_ocean_tide_m_(curind));
    else
        error('how did you get here?')
    end
    D(cnt).nmiss=round((datenum(Stop,timefmt)-datenum(Start,timefmt))/timeinterval)+1-D(cnt).navail;
    D(cnt).ngap=round((datenum(D(cnt).stop,outtimefmt)-datenum(D(cnt).start,outtimefmt))/timeinterval)+1-D(cnt).navail;
    D(cnt).t=datenum(datevec(J.date_time(curind),timefmt));
    D(cnt).tstr=datestr(datevec(J.date_time(curind),timefmt),outtimefmt);
    if strcmpi(DataType,'VerifiedSixMinute')||strcmpi(DataType,'PreliminarySixMinute')||...
       strcmpi(DataType,'VerifiedHourlyHeight')||strcmpi(DataType,'VerifiedHighLow')||...
       strcmpi(DataType,'VerifiedDailyMean')
        D(cnt).wl=J.water_surface_height_above_reference_datum_m_(curind);
    elseif strcmpi(DataType,'SixMinuteTidePredictions')||strcmpi(DataType,'HourlyTidePredictions')||...
           strcmpi(DataType,'HighLowTidePredictions')
        D(cnt).wl=J.sea_surface_height_amplitude_due_to_equilibrium_ocean_tide_m_(curind);
    else
        error('how did you get here?')
    end
end



%% Write output
if dowriteout==1
    for cnt=1:nstatn
        %Determine file name
        if autofileout==1||autofileout==2       %auto-define output file name
            curfileout=[D(cnt).station,'_',...
                        datestr(datevec(D(cnt).start,outtimefmt),'yyyy-mm-dd'),'_',...
                        datestr(datevec(D(cnt).stop,outtimefmt),'yyyy-mm-dd'),'_',...
                        D(cnt).datatype,'_',D(cnt).units,'_',D(cnt).datum];
            if strcmpi(writeout,'tab')
                curfileout=fullfile(fopath,[curfileout,'.txt']);
            elseif strcmpi(writeout,'mat')
                curfileout=fullfile(fopath,[curfileout,'.mat']);
            end
        elseif autofileout==0   %user gui file name specification
            curfileout=uiputfile('*',['Specify output file for station ',D(cnt).station]);
            if curfileout==0
                warning(['Output file not specified, skipping file output for station ',D(cnt).station])
                continue
            end
        elseif autofileout==-1
            if nstatn>1
%                 [fopath,foname,foext]=fileparts(fileout);
                curfileout=fullfile(fopath,[foname,'_',num2str(cnt),foext]);
            else
                curfileout=fileout;
            end
        else
            error('How did you get here?')
        end
        
        
        %Write out data
        disp(['Writing output file:  ',curfileout])
        if strcmpi(writeout,'tab')
            [fid,err]=fopen(curfileout,'w');
            if fid==-1
                warning(['Failed to open output file ',curfileout,', skipping file output for station ',D(cnt).station])
                warning(['Error from Matlab: ',err])
                continue
            end
            
            %Use try-catch loop to make sure file curfileout gets closed in
            %the event of an error
            try
                %1st line:  metadata header
                fprintf(fid,...
                    ['Station\t%s\tLongitude\t%11.6f\tLatitude\t%11.6f\t'...
                    'VertDatum\t%s\tTimezone\t%s\tUnits\t%s\tDataType\t%s\t'...
                    'StartDateTime\t%s\tStopDateTime\t%s\t'...
                    'RequestedStartDateTime\t%s\tRequestedStopDateTime\t%s\t'...
                    'NumberOfAvailableValues\t%i\tNumberOfMissingValues\t%i\t'...
                    'NumberOfMissingValuesBetweenStartAndStop\t%i'...
                    '\r\n'],...
                    D(cnt).station,D(cnt).lon,D(cnt).lat,...
                    D(cnt).datum,D(cnt).timezone,D(cnt).units,D(cnt).datatype,...
                    D(cnt).start,D(cnt).stop,...
                    D(cnt).requeststart,D(cnt).requeststop,...
                    D(cnt).navail,D(cnt).nmiss,...
                    D(cnt).ngap...
                    );
                
                %2nd line: column header for rest of file
                fprintf(fid,'date_time\tdata\r\n');
                
                %All additional lines:  time and data
                for cnt2=1:numel(D(cnt).wl)
                    fprintf(fid,'%s\t%12.5f\r\n',D(cnt).tstr(cnt2,:),D(cnt).wl(cnt2));
                end
                fclose(fid);
            catch err
                fclose(fid);
                rethrow(err)
            end
        elseif strcmpi(writeout,'mat')
            save(curfileout,'D','-mat');
        else
            error('How did you get here?')
        end
        
    end
end




