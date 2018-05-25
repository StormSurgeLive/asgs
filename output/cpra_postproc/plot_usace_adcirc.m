clear all;
close all;
clc;

colors = [0.9290 0.6940 0.1250; 175/255 54/255 60/255];

% -------------------------------------------------------------------------

% All time are in UTC. The USACE values are adjusted from CDT to UTC
% by adding 5 hours.

% -------------------------------------------------------------------------

% Read cpraHydro.info for specific run-time information
fid = fopen('cpraHydro.info','r');
info = textscan(fid,'%s\n');
fclose(fid);
storm = info{1}{1}; adcGrid = info{1}{2}; forecastValid = info{1}{3};

% -------------------------------------------------------------------------

% These lists are used in order to look-up the appropriate name based on
% the USACE Station ID found when parsing the fort.15/fort.61.
% USACE Station ID
stations={'85625','76065','76030','76265','82762','82770','82742',...
    '85760','76010','82715','01440','01440'};
usaceStationsName={'West End',...
    'Seabrook CC - Flood Side',...
    'IHNC Surge Barrier East - Flood Side',...
    'GIWW at West Closure Complex - Flood Side',...
    'Hero Canal Bulkhead Control Structure - Flood Side/West',...
    'Oakville Levee Pump Station - Flood Side/South',...
    'Bayou Segnette Sector Gate - Flood Side',...
    'Caernarvon Canal Sector Gate - South/Flood Side',...
    'Bayou Dupre Sector Gate - East/Flood Side',...
    'Bayou Verret / Western Tie-In Sector Gate - Flood Side',...
    'Mississippi River at Empire',...
    'Mississippi River at Empire'};
% The stationID in parenthesis must match what is in the fort.51
cpraStationNames = {'17th St. Outfall Canal (17StCanal)',...
    'Seabrook Complex (IHNC01)',...
    'IHNC  Surge Barrier (IHNC02)',...
    'West Closure Complex (WBV90)',...
    'Hero Canal Stop-Log Gage (WBV09b)',...
    'Oakville Sluice Gage (WBV09a)',...
    'Bayou Segnette Closure (WBV162)',...
    'Caernarvon Canal Sector Gate (LPV149)',...
    'Bayou Dupre Sector Gate (LPV144)',...
    'Western Tie-In (WBV7274)',...
    'Empire Floodgate (NOV13)',...
    'Empire Lock (NOV14)'};

% Read in table containg the Station ID and index of the cpra stations as
% contained in the fort.15/61
% The first column in staID and the second is the index
fid = fopen('station_index.txt');
stationIndex = textscan(fid,'%s%s','CollectOutput',1);
stationIndex = stationIndex{:};
fid = fclose(fid);

% index = ~cellfun(@isempty,strfind(cpraStationNames,'17StCanal'));

% -------------------------------------------------------------------------

% Need to get this in through some input information...
% dtAdvisory = datenum(2012,8,27,9,0,0);
dtAdvisory = datenum(forecastValid,'yyyymmddHHMMSS');

% Loop through the number of ensemble simulations
numEns = 1;
ensFileNames = {'fort.61.imeds','fort.61.veerRight50.imeds'};
for i = 1:numEns
    % Read in ADCIRC time-series water levels
    adcData(i) = readIMEDS(char(ensFileNames(i)));
end

sdate = round(adcData(1).STATION{1}.DATE(1));
edate = round(adcData(1).STATION{1}.DATE(length(adcData(1).STATION{1}.DATE)));
            
ax = gca;
fig = gcf;

for f = 1:length(stations)
    
    % Find the cpraStation name for stations in stationIndex
    f51Loc = find(~cellfun(@isempty,strfind(cpraStationNames,stationIndex(f))));
       
    % Get USACE water level data for station f51Loc
    % If the rivergages function fails, then create a dummy value to plot.
    try
        %wl=rivergages2(stations{1,f},datestr(sdate-3),datestr(sdate),'HG');
        wl=rivergages2(stations{1,f51Loc},datestr(sdate-3),datestr(sdate),'HG');
        
        % Plot USACE Observations
        if isempty(wl) == 0
            wl(:,1) = wl(:,1) + 5/24; % Adjust time from CDT to UTC
        else
            wl(1,1) = sdate; wl(1,2) = -20; % Create a fake point for legend purposes
        end
        % Plot USACE water levels as scatter points
%         scatter(wl(:,1),wl(:,2),50,'MarkerEdgeColor','black','Linewidth',1);hold on;
    catch ME
        % USACE is down... :(
        % Create some dummy values so the legned can still be plotted
        wl(1,1) = sdate;
        wl(1,2) = -20;
    end
%         wl(1,1) = sdate;
%         wl(1,2) = -20;
    % Plot USACE water levels as scatter points
    scatter(wl(:,1),wl(:,2),50,'MarkerEdgeColor','black','Linewidth',1);hold on;
     
% -------------------------------------------------------------------------
    
    % Get ADCIRC data from station f and loop through each available ensemble
    for i = 1:numEns
        adcData(i).STATION{f51Loc}.DATA(adcData(i).STATION{f51Loc}.DATA < -999) = NaN;
        % Find min/max water surface elevation from ADCIRC result
        res = ~any(~isnan(adcData(i).STATION{f51Loc}.DATA(:)));
        if res == false
    %         maxWL = ceil(max(adcData.STATION{f}.DATA / 0.3048)) + 3;
    %         minWL = floor(min(adcData.STATION{f}.DATA / 0.3048)) - 2;

            % I want to find this before plotting so all charts have the same
            % min and max y-axis.
            maxWL = 18;
            minWL = -4;
        else
            % Create some dummy values so the legned can still be plotted
            adcData(i).STATION{f51Loc}.DATE(1) = sdate;
            adcData(i).STATION{f51Loc}.DATE(2) = sdate+0.01;
            adcData(i).STATION{f51Loc}.DATA(1) = -20;
            adcData(i).STATION{f51Loc}.DATA(2) = -20;

            % Force max and min water level bounds
            maxWL = 18;
            minWL = -4;
        end
    
        % Plot ADCIRC Simulation
        plot(adcData(i).STATION{f51Loc}.DATE,...
            adcData(i).STATION{f51Loc}.DATA / 0.3048,...
            'Linewidth',3,'color',colors(i,:))

    end
    
% -------------------------------------------------------------------------
    
    % Plot advisory date/time and vertical line
    plot([dtAdvisory dtAdvisory],[-10 maxWL-4.5],...
        '--', 'color', 'black','Linewidth',0.75);
    lf = text(dtAdvisory,maxWL-4.3,strcat(datestr(dtAdvisory,'HH:MM'),' UTC'));
    set(lf,'Rotation',90);

% -------------------------------------------------------------------------

% Setup Axis Properties
    % X-Axis Properties
    xlim([sdate-3,edate+2]);
    ax.XAxis.TickValues = [sdate-3:1:edate+2];
    ax.XAxis.MinorTickValues = [sdate-3:0.25:edate+2];
    datetick('x','mmm-dd','keeplimits','keepticks')
    ax.XMinorTick = 'on';
    ax.XTickLabelRotation = 90;
    xlabel('Date (UTC)');
     
    % Y-Axis Properties
    ylim([minWL,maxWL])
    ax.YAxis.TickValues = [minWL:2:maxWL];
    ax.YMinorTick = 'on';
    ax.YAxis.MinorTickValues = [minWL:1:maxWL];
    ylabel('Water Level (ft, NAVD88)');

    grid on;
    %ax.YMinorGrid = 'on';
    
% -------------------------------------------------------------------------
    
%     title1 = 'Storm:Isaac  -  grid:LA_v17a-WithUpperAtch_chk';
    title1 = strcat('Storm:',storm,' - grid:',adcGrid);
    title2 = strcat(cpraStationNames(f51Loc),'  -  USACE Gage ID ',stations(f51Loc));
    
    text(0,1.07,title1,'Units','normalized','Interpreter','None');
    text(0,1.03,title2,'Units','normalized','Interpreter','None');
    
    % Add Legend
%     legend('USACE Observations','nhcConsensus','veerRight50',...
%         'Location','northwest');
    legend('USACE Observations','nhcConsensus','Location','northwest');
    
    % Override some defaults
    set(gca,'LineWidth',1,'TickLength',[0.015 0.015]);
    set(gca,'FontSize',12,'FontWeight','bold');
    
    hold off;
    
    % Plot Figure
    fig.PaperUnits = 'inches';
    fig.PaperPosition = [0 0 12.5 5];
    % Image filename is 'WSE_cpraStationID_USACEStationID'
    fname = char(strcat('WSE_',stationIndex(f),'_USACE',stations(f51Loc)));
    print(fname,'-dpng','-r200');
end
