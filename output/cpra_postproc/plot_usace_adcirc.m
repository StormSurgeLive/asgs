%% 
clear all;
close all;
clc;

% All time are in UTC. The USACE values are adjusted from CDT to UTC
% by adding 5 hours.

% Hydrograph ensemble colors
colors = [0.9290 0.6940 0.1250; 175/255 54/255 60/255];

%% 
% -------------------------------------------------------------------------
% Read cpraHydro.info for specific run-time information
fid = fopen('cpraHydro.info','r');
info = textscan(fid,'%s\n');
fclose(fid);
storm = info{1}{1}; adcGrid = info{1}{2}; forecastValid = info{1}{3};

%% 
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

%% 
% -------------------------------------------------------------------------

% Get the current date/time of the advisory
dtAdvisory = datenum(forecastValid,'yyyymmddHHMMSS');

%% 
% -------------------------------------------------------------------------

% Loop through the number of ensemble simulations
numEns = 1;
ensFileNames = {'fort.61.nc','fort.61.veerRight50.61.nc'};
for i = 1:numEns
    % Read in ADCIRC time-series water levels
    adcData(i) = read61nc(char(ensFileNames(i)));
end
% NEED TO ADD CHECK TO MAKE SURE THE NUMBER OF STATIONS ARE THE SAME FOR
% EACH ENSEMBLE SIMULATION

% Get the start and end date of the ADCIRC simulation and round off
% Used for plotting purposes and grabbing USACE gage data
sdate = round(adcData(1).STATION{1}.DATE(1));
edate = round(adcData(1).STATION{1}.DATE(length(adcData(1).STATION{1}.DATE)));
            
%% 
ax = gca;
fig = gcf;

%% 
for f = 1:adcData(1).NumStations
  
    % This will provide you with the index in the cpraStationNames vector
    % assigned earlier in the script
    cpraStationIndex = find(~cellfun(@isempty,strfind(cpraStationNames,adcData.STATION{f}.NAME)));
    
    % Check if cpraStationIndex was found -> If not, then cycle to next iteration.
    if isempty(cpraStationIndex) == 1
        continue;
    end
       
    % Get USACE water level data for station cpraStationIndex
    % If the rivergages function fails, then create a dummy value to plot.
    try
        display(stations{1,cpraStationIndex})
        wl=rivergages2(stations{1,cpraStationIndex},datestr(sdate-3),datestr(sdate),'HG');
        
        % Plot USACE Observations
        if isempty(wl) == 0 % Data was obtained
            oDataExist = 1;
            wl(:,1) = wl(:,1) + 5/24; % Adjust time from CDT to UTC
            wl(wl < -99) = NaN; % Remove data points that are less than -99
 
            % Find the min and max observed water levels
            maxOWL = ceil(max(wl(:,2)));
            minOWL = floor(min(wl(:,2)));
        else
            wl(1,1) = sdate; wl(1,2) = -20; % Create a fake point for legend purposes
        end
    catch ME
        % USACE is down... :(
        % Create some dummy values so the legned can still be plotted
        wl(1,1) = sdate;
        wl(1,2) = -20;
        maxOWL = 1;
        minOWL = 0;
        oDataExist = 0;
    end
    
    % Plot USACE water levels as scatter points
    scatter(wl(:,1),wl(:,2),50,'MarkerEdgeColor','black','Linewidth',1);hold on;
     
    %% 
% -------------------------------------------------------------------------
    
    % Get ADCIRC data from station f and loop through each available ensemble
    for i = 1:numEns
        adcData(i).STATION{f}.DATA(adcData(i).STATION{f}.DATA < -999) = NaN;
        % Find min/max water surface elevation from ADCIRC result
        res = ~any(~isnan(adcData(i).STATION{f}.DATA(:)));
        if res == false
            mDataExist = 1;
            % Find min/max water surface elevation from ADCIRC result
            maxMWL = ceil(max(adcData.STATION{f}.DATA / 0.3048));
            minMWL = floor(min(adcData.STATION{f}.DATA / 0.3048));
        else
            mDataExist = 0;
            % Create some dummy values so the legned can still be plotted
            adcData(i).STATION{f}.DATE(1) = sdate;
            adcData(i).STATION{f}.DATE(2) = sdate+0.01;
            adcData(i).STATION{f}.DATA(1) = -20;
            adcData(i).STATION{f}.DATA(2) = -20;

            % Force max and min water level bounds
            maxMWL = 1;
            minMWL = 0;
        end

        % Plot ADCIRC Simulation
        plot(adcData(i).STATION{f}.DATE,...
            adcData(i).STATION{f}.DATA / 0.3048,...
            'Linewidth',3,'color',colors(i,:))
    end
    
    minWL = min(minOWL,minMWL) - 1;
    maxWL = max(maxOWL,maxMWL) + 3;

    
    %% 
% -------------------------------------------------------------------------
    
    % Plot advisory date/time and vertical line
    plot([dtAdvisory dtAdvisory],[minWL maxWL - 1.25],...
        '--', 'color', 'black','Linewidth',0.75);
    lf = text(dtAdvisory,maxWL - 1.25,strcat(datestr(dtAdvisory,'HH:MM'),' UTC'));
    set(lf,'Rotation',90);
    
    % Plot horizonal line at 0
    plot([0 0],[sdate-3 edate+1],...
        '-', 'color', 'black','Linewidth',1.0);
    
    %% 
% -------------------------------------------------------------------------

% Setup Axis Properties
    % X-Axis Properties
    xlim([sdate-3,edate+1]);
    ax.XAxis.TickValues = [sdate-3:1:edate+1];
    ax.XAxis.MinorTickValues = [sdate-3:0.25:edate+1];
    datetick('x','mmm-dd','keeplimits','keepticks')
    ax.XMinorTick = 'on';
    ax.XTickLabelRotation = 90;
    xlabel('Date (UTC)');
     
    % Y-Axis Properties
    ylim([minWL,maxWL])
    if (maxWL - minWL) < 10
        ax.YAxis.TickValues = [minWL:1:maxWL];
        ax.YMinorTick = 'on';
        ax.YAxis.MinorTickValues = [minWL:0.5:maxWL];
    else
        ax.YAxis.TickValues = [minWL:2:maxWL];
        ax.YMinorTick = 'on';
        ax.YAxis.MinorTickValues = [minWL:1:maxWL];
    end
    ylabel('Water Level (ft, NAVD88)');

    grid on;
    %ax.YMinorGrid = 'on';
    
    %% 
% -------------------------------------------------------------------------
    
    title1 = strcat('Storm:',storm,' - grid:',adcGrid);
    title2 = strcat(cpraStationNames(cpraStationIndex),'  -  USACE Gage ID ',stations(cpraStationIndex));
    
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
    
    fname = char(strcat('WSE_',adcData.STATION{f}.NAME,'_USACE',stations(cpraStationIndex)));
    print(fname,'-dpng','-r200');
end