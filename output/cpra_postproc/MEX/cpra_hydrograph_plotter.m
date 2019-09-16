%####################################################
% AUTHORS: Matthew V Bilskie, PhD
%          Louisiana State University
%
%          Jason Fleming, PhD
%          Seahorse Coastal Consulting
% COPYRIGHT 2018-2019
%
%####################################################
% 
% All time are in CDT. The model values are adjusted from UTC to CDT
% by subtracting 5 hours.
%
clear all;
close all;
%
%%
%
msg = sprintf('cpra_hydrograph_plotter.m: BEGIN cpra_hydrograph_plotter.m %s CDT',...
    datestr(datetime('now','TimeZone','America/Chicago'),'yyyymmdd HH:MM'));
disp(msg);
%
%%
% User-defined inputs
%
% offset = 0.0; % in feet
% offset = 1.25; % in feet
% offsetSet = zeros(1,22);
% offsetSet(:) = offset;
% Offset vector - same order as "stations" string vector
% offsetSet = [1.25,1.25,1.25,0.75,1.25,1.25,1.25,...
%     1.25,1.25,1.25,1.25,1.25,1.25,1.50,1.00,0.35];
% {'85625','76065','76030','76265','82762','82770','82742',...
% '85760','76010','82715','01440','01440','85670','85575','85700','82875'};

% productionMode = true; % for ASGS
productionMode = false; % Manual mode

if (productionMode)
%     ASGS Production mode.
    numEns = 1;
    ensFileNames = {'fort.61.nc'};
    propFile = {'run.properties'};
    plotPrevious = false;
else
%     Manual Mode
%     The nhcConsensus for the last advisory, if used, advsiory should be
%     FIRST in the array
%     The nhcConsensus for the current advsiory should be LAST in the array
    numEns = 4;
    ensFileNames = {'Adv11.nhcConsensus.fort.61.nc','Adv12.veerLeft100.fort.61.nc','Adv12.veerRight100.fort.61.nc','Adv12.nhcConsensus.fort.61.nc'};
    propFile = {'Adv11.nhcConsensus.run.properties','Adv12.veerLeft100.run.properties','Adv12.veerRight100.run.properties','Adv12.nhcConsensus.run.properties'};
    plotPrevious = true;
end
%
%%

% Hydrograph ensemble colors
colors = [0.9290 0.6940 0.1250; 175/255 54/255 60/255; 0/255 128/255 0/255;...
    143/255 0/255 255/255; 0/255 0/255 255/255; 47/255 79/255 79/255];

% These lists are used in order to look-up the appropriate name based on
% the USACE Station ID found when parsing the fort.15/fort.61.
%
% Station Agency and ID
fstations={'USACE 85625','USACE 76065','USACE 76030','USACE 76265',...
    'USACE 82762','USACE 82770','USACE 82742','USACE 85760',...
    'USACE 76010','USACE 82715','USACE 01440','USACE 01440',...
    'USACE 85670','USACE 85575','USACE 85700','USACE 82875',...
    'USACE 76220','USACE 76230','USACE 76025','USGS 073802516',...
    'USACE 76593','USGS 08017118'};
fstationsSplit = regexp(fstations, ' ', 'split');
fstationsSplit = vertcat(fstationsSplit{:});
% Station ID
stations={'85625','76065','76030','76265','82762','82770','82742',...
    '85760','76010','82715','01440','01440','85670','85575','85700',...
    '82875','76220','76230','76025','073802516','76593','08017118'};
gageStationsName={'West End',...
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
    'Mississippi River at Empire',...
    'Lake Pontchartrain at Lakefront Airport',...
    'Lake Pontchartrain at Mandeville',...
    'Rigolets near Lake Pontchartrain',...
    'Barataria Waterway at Lafitte',...
    'Harvey Canal Sector Gates North / Protected Side nr Lapalco Blvd',...
    'Harvey Canal at Boomtown Casino',...
    'Bayou Bienvenue Floodgate - East/Flood Side',...
    'Barataria Pass at Grand Isle',...
    'Freshwater Canal at Freshwater Bayou Lock',...
    'Calcasieu River at Cameron'};
% The stationID in parenthesis must match what is in the fort.15
cpraStationNames = {'17th St. Outfall Canal (17StCanal)',...
    'Seabrook Complex (IHNC01)',...
    'IHNC  Surge Barrier (IHNC02)',...
    'West Closure Complex (WBV90)',...
    'Hero Canal Stop-Log Gage (WBV09b)',...
    'Oakville Sluice Gate (WBV09a)',...
    'Bayou Segnette Closure (WBV162)',...
    'Caernarvon Canal Sector Gate (LPV149)',...
    'Bayou Dupre Sector Gate (LPV144)',...
    'Western Tie-In (WBV7274)',...
    'Empire Floodgate (NOV13)',...
    'Empire Lock (NOV14)',...
    'Lakefront Airport (LakefrontAirport)',...
    'Mandeville (Mandeville)',...
    'Rigolets (Rigolets)',...
    'Lafitte (Lafitte)',...
    'Harvey Canal Sector Gate (HarveyCanalNorth)',...
    'Harvey Canal at Boomtown (HarveyCanalBoom)',...
    'Bayou Bienvenue Floodgate (BayouBienv)',...
    'Barataria Pass at Grand Isle (BaraPass)',...
    'Freshwater Canal (FreshCanal)',...
    'Calcasieu River (CalcRiver)'};
%
%% 

% The static offset (ft)
staticOffsetFile = 'Static_Offset.xlsx';
if exist(staticOffsetFile)
    fid = fopen(staticOffsetFile,'r');
    msg = sprintf('cpra_hydrograph_plotter.m: File %s was found.', staticOffsetFile);
    disp(msg);
else
    msg = sprintf('cpra_hydrograph_plotter.m FATAL ERROR: %s was NOT found.', staticOffsetFile);
    disp(msg);
    %quit;
end
msg = sprintf('cpra_hydrograph_plotter.m: Reading static offsets information from %s.', staticOffsetFile);
disp(msg);
[~, ~, raw] = xlsread(staticOffsetFile);
for idx = 1:numel(raw)
   if isnumeric(raw{idx})
      raw{idx} = num2str(raw{idx});
   end
end
offsetMap = containers.Map(raw(:,2),str2num(char(raw(:,3)))*0.3048);
msg = sprintf('cpra_hydrograph_plotter.m: Success reading static offset information from %s.', staticOffsetFile);
disp(msg);

% Create a map container of station-dependent offsets
%offsetSet = offsetSet * 0.3048;
%offsetMap = containers.Map(stations,offsetSet);

% msg = sprintf('cpra_hydrograph_plotter.m: Using an offset of %f ft.',offset);
% disp(msg);
% offset = offset*0.3048;

%
%% 

if (plotPrevious)
    useEnsemble = numEns;
else
    useEnsemble = 1;
end

%%
% The USACE Gate Closure Trigger (ft, NAVD88)
gateFile = 'Gate_Closure_Trigger.xlsx';
if exist(gateFile)
    fid = fopen(gateFile,'r');
    msg = sprintf('cpra_hydrograph_plotter.m: File %s was found.', gateFile);
    disp(msg);
else
    msg = sprintf('cpra_hydrograph_plotter.m FATAL ERROR: %s was NOT found.', gateFile);
    disp(msg);
    %quit;
end
msg = sprintf('cpra_hydrograph_plotter.m: Reading gate closure information from %s.', gateFile);
disp(msg);
[num,txt,raw] = xlsread(gateFile);
trigger = num;
msg = sprintf('cpra_hydrograph_plotter.m: Success reading gate closure information from %s.', gateFile);
disp(msg);
%
%%
% The Datum Conversion (ft -> Gage Datum to NAVD88)
datumConvFile = 'Datum_Conversion.xlsx';
if exist(datumConvFile)
    fid = fopen(datumConvFile,'r');
    msg = sprintf('cpra_hydrograph_plotter.m: File %s was found.', datumConvFile);
    disp(msg);
else
    msg = sprintf('cpra_hydrograph_plotter.m FATAL ERROR: %s was NOT found.', datumConvFile);
    disp(msg);
    %quit;
end
msg = sprintf('cpra_hydrograph_plotter.m: Reading datum conversion information from %s.', datumConvFile);
disp(msg);
[~, ~, raw] = xlsread(datumConvFile);
for idx = 1:numel(raw)
   if isnumeric(raw{idx})
      raw{idx} = num2str(raw{idx});
   end
end
datumConvMap = containers.Map(raw(:,2),str2num(char(raw(:,3))));
msg = sprintf('cpra_hydrograph_plotter.m: Success reading datum conversion information from %s.', datumConvFile);
disp(msg);
%
%% 
% -------------------------------------------------------------------------

% Loop through the number of ensemble simulations
msg = sprintf('cpra_hydrograph_plotter.m: Begin reading ADCIRC station data.');
disp(msg);
for i = 1:numEns
    % Read in ADCIRC time-series water levels
    msg = sprintf('cpra_hydrograph_plotter.m: Reading %s', char(ensFileNames(i)));
    disp(msg);
    adcData(i) = read61nc(char(ensFileNames(i)));
    % Change from UTC to CDT by subtracting 5 hours
    % Adjust hydrograph based on offset
    for j = 1:adcData(i).NumStations
        adcData(i).STATION{j}.DATE = adcData(i).STATION{j}.DATE - (5/24);
        adcData(i).STATION{j}.DATA = adcData(i).STATION{j}.DATA;
%         adcData(i).STATION{j}.DATA = adcData(i).STATION{j}.DATA + offset;
    end
    msg = sprintf('cpra_hydrograph_plotter.m: Success reading %s', char(ensFileNames(i)));;
    disp(msg);
end
% NEED TO ADD CHECK TO MAKE SURE THE NUMBER OF STAdfTIONS ARE THE SAME FOR
% EACH ENSEMBLE SIMULATION

% Get the start and end date of the ADCIRC simulation and round off
% Used for plotting purposes and grabbing USACE gage data
sdate = round(adcData(useEnsemble).STATION{1}.DATE(1));
edate = round(adcData(useEnsemble).STATION{1}.DATE(length(adcData(useEnsemble).STATION{1}.DATE)));

%datetime(adcData(1).STATION{1}.DATE(1),'ConvertFrom','datenum')
            
%% 
ax = gca;
fig = gcf;

%% 
for f = 1:adcData(1).NumStations
  
    % This will provide you with the index in the cpraStationNames vector
    % assigned earlier in the script
    cpraStationIndex = find(~cellfun(@isempty,strfind(cpraStationNames,adcData(1).STATION{f}.NAME)));
    
    gageAgency = char(fstationsSplit(cpraStationIndex,1));
    gageID = char(fstationsSplit(cpraStationIndex,2));
    
    % Check if cpraStationIndex was found -> If not, then cycle 
    % to next iteration.
    if isempty(cpraStationIndex) == 1
        msg = sprintf('cpra_hydrograph_plotter.m: NON-FATAL WARNING: cpra_hydrograph_plotter.m: Could not find %s',adcData(1).STATION{f}.NAME);
        %disp(msg);
        continue;
    end
    
    if strcmp(gageAgency,'USGS')
        % Get USGS water level data for station cpraStationIndex
        % If the USGS function fails, then create a dummy value to plot.
        try
            msg = sprintf('cpra_hydrograph_plotter.m: Finding gage data for USGS %s', gageID);
            disp(msg);
            % wl = USGS_Service(char(site(i)),format,outputDataType,sDate,eDate,plotter);
            wl = USGS_Service(gageID,'waterml,2.0','00065',datestr(sdate-3),datestr(sdate),false);
            if isempty(wl) == 0 % Data was obtained
                msg = sprintf('cpra_hydrograph_plotter.m: Gage data for USGS %s was found!', gageID);
                disp(msg);
                oDataExist = 1;
                            
                % Adjust datum, if necessary
                wl(:,2) = wl(:,2) + datumConvMap(gageID);
                msg = sprintf('cpra_hydrograph_plotter.m: Adjusting datum to NAVD by %0.5f', datumConvMap(gageID));
                disp(msg);

                % Find the min and max observed water levels
                maxOWL = ceil(max(wl(:,2)));
                minOWL = floor(min(wl(:,2)));
            else
                msg = sprintf('cpra_hydrograph_plotter.m: NON-FATAL WARNING: Gage data for USGS %s was NOT found!', gageID);
                disp(msg);
                wl(1,1) = sdate; wl(1,2) = -20; % Create a fake point for legend purposes
                oDataExist = 0;
            end
        catch ME
            % USGS is down... :(
            msg = sprintf('cpra_hydrograph_plotter.m: NON-FATAL WARNING: Gage data for USGS %s was NOT found!', gageID);
            disp(msg);
            % Create some dummy values so the legned can still be plotted
            wl(1,1) = sdate;
            wl(1,2) = -20;
            maxOWL = 1;
            minOWL = 0;
            oDataExist = 0;
        end
    elseif strcmp(gageAgency,'USACE')
        % Get USACE water level data for station cpraStationIndex
        % If the rivergages function fails, then create a dummy value to plot.
        try
            msg = sprintf('cpra_hydrograph_plotter.m: Finding gage data for USACE %s', gageID);
            disp(msg);
            wl=rivergages2(gageID,datestr(sdate-3),datestr(sdate),'HG');
            
            % Plot USACE Observations
            if isempty(wl) == 0 % Data was obtained
                msg = sprintf('cpra_hydrograph_plotter.m: Gage data for USACE %s was found!', gageID);
                disp(msg);
                oDataExist = 1;
                
                % Adjust datum, if necessary
                wl(:,2) = wl(:,2) + datumConvMap(gageID);
                msg = sprintf('cpra_hydrograph_plotter.m: Adjusting datum to NAVD by %0.5f', datumConvMap(gageID));
                disp(msg);
                
    %             wl(:,1) = wl(:,1) + 5/24; % Adjust time from CDT to UTC
                wl(wl < -99) = NaN; % Remove data points that are less than -99

                % Find the min and max observed water levels
                maxOWL = ceil(max(wl(:,2)));
                minOWL = floor(min(wl(:,2)));
            else
                msg = sprintf('cpra_hydrograph_plotter.m: NON-FATAL WARNING: Gage data for USACE %s was NOT found!', gageID);
                disp(msg);
                wl(1,1) = sdate; wl(1,2) = -20; % Create a fake point for legend purposes
                oDataExist = 0;
            end
        catch ME
            % USACE is down... :(
            msg = sprintf('cpra_hydrograph_plotter.m: NON-FATAL WARNING: Gage data for USACE %s was NOT found!', gageID);
            disp(msg);
            % Create some dummy values so the legned can still be plotted
            wl(1,1) = sdate;
            wl(1,2) = -20;
            maxOWL = 1;
            minOWL = 0;
            oDataExist = 0;
        end
    else
        msg = sprintf('cpra_hydrograph_plotter.m: Did not recognize %s %s',...
            gageAgency, gageID);
            disp(msg);
    end
        
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       
    
    % Plot USACE water levels as scatter points
    scatter(wl(:,1),wl(:,2),50,'MarkerEdgeColor','black','Linewidth',1);hold on;
    legendCell{1} = 'Gage Observations';
        
    %% 
% -------------------------------------------------------------------------
    
    % Get ADCIRC data from station f and loop through each available ensemble
    for i = 1:numEns

        % Load run.properties file
        % Utilize Matlab Map object data structure - similar to Python dict.
        % http://www.mathworks.com/help/matlab/ref/containers.map.html;jsessionid=e51d09845dede42c23d71ce2851a
        if exist(propFile{i})
            fid = fopen(propFile{i},'r');
            msg = sprintf('cpra_hydrograph_plotter.m: File %s was found.', propFile{i});
            disp(msg);
        else
            msg = sprintf('cpra_hydrograph_plotter.m FATAL ERROR: %s was NOT found.', propFile{i});
            disp(msg);
            %quit;
        end
        data = textscan(fid,'%s %s %s %s','delimiter',{':'});
        fclose(fid);
        keySet = strtrim(data{1});
        valueSet = data{2};
        M = containers.Map(keySet,valueSet);
        clear keySet valueSet data

        adcGrid = M('adcirc.gridname');
        advisory = M('advisory'); forecastValidStart = M('forecastValidStart');
        enstorm = M('asgs.enstorm');
        
        if ~isKey(M,'stormname')
%             storm = 'NAM';
            storm = enstorm;
        else
            storm = M('stormname');
        end
        
        msg = sprintf('cpra_hydrograph_plotter.m: Success reading %s', propFile{i});
        disp(msg);

        if strcmp(storm,enstorm)
            legendCell{i+1} = enstorm;      
        else  
            legendCell{i+1} = char(strcat(enstorm,{' (Advisory '},advisory,'{)}'));      
        end
     
        % Get the current date/time of the advisory
        % Subtract 5 hours to convert from UTC to CDT.
        dtAdvisory = datenum(forecastValidStart,'yyyymmddHHMMSS') - (5/24);

		% Add in the offset, if any
        adcData(i).STATION{f}.DATA = adcData(i).STATION{f}.DATA + ...
            offsetMap(gageID);		
     
        adcData(i).STATION{f}.DATA(adcData(i).STATION{f}.DATA < -999) = NaN;
        % Find min/max water surface elevation from ADCIRC result
        res = ~any(~isnan(adcData(i).STATION{f}.DATA(:)));
        if res == false
            msg = sprintf('cpra_hydrograph_plotter.m: ADCIRC data for %s - Storm %s - Advisory %s was found!',...
                adcData(i).STATION{f}.NAME, storm, advisory);
            disp(msg);
            mDataExist = 1;
            % Find min/max water surface elevation from ADCIRC result
            maxMWL = ceil(max(adcData(i).STATION{f}.DATA / 0.3048));
            minMWL = floor(min(adcData(i).STATION{f}.DATA / 0.3048));
            % Only plot trigger for consensus track for current advisory
            if (i == useEnsemble)
                max4Trigger = max(adcData(i).STATION{f}.DATA / 0.3048);
            end
        else
            msg = sprintf('cpra_hydrograph_plotter.m: ADCIRC data for %s - Storm %s - Advisory %s was NOT found!',...
                adcData(i).STATION{f}.NAME, storm, advisory);
            disp(msg);
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
    
    % Include some text stating that no observed gage data is available.
    if oDataExist == 0
        text(sdate-2.85,minWL + 1.5,'Gage observations are not available.','Interpreter','None');
    end
    % Include some text stating that no model data is available.
    % Removed on 09-18-2018 based on review from CPRA
%     if mDataExist == 0
%         text(sdate+2.5,minWL + 1.5,'Model result indicate location is dry.','Interpreter','None');
%     end

    %% 
% -----------------------------------------------------------------------
    % Find out if forecasted water level is above the trigger
    triggerL = false;
    % Only plot trigger for consensus track for current advisory
    % This is why the index of adcData is useEnsemble
    if (trigger(cpraStationIndex) > 0) && (max4Trigger >= trigger(cpraStationIndex))
        triggerL = true;
        % Find the index at which this occurs. Use the first time it
        % occurs.
        idx = find(adcData(useEnsemble).STATION{f}.DATA/.3048 > trigger(cpraStationIndex));
        if isempty(idx) == 0
            trigDate = adcData(useEnsemble).STATION{f}.DATE(idx(1));
            % Plot gate closure trigger
            plot([trigDate-0.5 trigDate+0.5],[trigger(cpraStationIndex) trigger(cpraStationIndex)],...
                '-', 'color', 'green','Linewidth',2.0);
            plot([trigDate trigDate],[minWL maxWL],...
                '--', 'color', 'black','Linewidth',0.75);
            tt = text(trigDate-0.1,maxWL,strcat(datestr(trigDate,'HH:MM'),' CDT'));
            tt(1).HorizontalAlignment = 'right';
            set(tt,'Rotation',90);
        end
    end
    
    %% 
% -------------------------------------------------------------------------
    % Plot advisory date/time and vertical line
    plot([dtAdvisory dtAdvisory],[minWL maxWL],...
        '--', 'color', 'black','Linewidth',0.75);
    at = text(dtAdvisory-0.1,maxWL,...
        strcat(datestr(dtAdvisory,'HH:MM'),' CDT'));
    at(1).HorizontalAlignment = 'right';
    set(at,'Rotation',90);
    
    % Plot horizonal line at the top
    plot([sdate-3 edate+1],[maxWL maxWL],...
        '-', 'color', 'black','Linewidth',1.0);
    
    % Plot vertical line to close off the plot
    plot([edate+1 edate+1],[minWL maxWL],...
        '-', 'color', 'black','Linewidth',1.0);
     
    %% 
% -------------------------------------------------------------------------

% Setup Axis Properties
    % X-Axis Properties
    xlim([sdate-3,edate+1]);
    ax.XAxis.TickValues = [sdate-3:1:edate+1];
    ax.XAxis.MinorTickValues = [sdate-3:0.25:edate+1];
    datetick('x','mmm-dd HH:MM','keeplimits','keepticks')
    ax.XMinorTick = 'on';
    ax.XTickLabelRotation = 90;
    ax.FontSize = 10;
    xlabel('Date (CDT)');
     
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
    
    title1 = strcat({'Storm: '},storm,{' - Advisory: '},advisory,{' Issued on '},...
        datestr(dtAdvisory,'mm-dd HH:MM'),{' CDT'},{' - grid: '},adcGrid);
    
    % Arrange plot titles accordingly for NAM/daily runs vs STORM runs
    if (strcmp(storm,enstorm))
        title1 = strcat(M('WindModel'),{' Cycle: '},M('currentcycle'),{' Issued on '},...
            datestr(dtAdvisory,'mm-dd HH:MM'),{' CDT'},{' - grid: '},adcGrid);
    else
        title1 = strcat({'Storm: '},storm,{' - Advisory: '},advisory,{' Issued on '},...
            datestr(dtAdvisory,'mm-dd HH:MM'),{' CDT'},{' - grid: '},adcGrid);
    end
    
	if (strcmp(cpraStationNames(cpraStationIndex),'Western Tie-In (WBV7274)') == 1)
		title2 = strcat('Western Tie-In (WBV-72/74)  -  USACE Gage ID:',stations(cpraStationIndex),...
              {' - Datum Converstion to NAVD88: '},num2str(datumConvMap(gageID),'%0.2f'),{' ft'});
%             {' - '},'Datum Converstion to NAVD88 ',num2str(datumConvMap(gageID)));
	elseif (strcmp(cpraStationNames(cpraStationIndex),'Bayou Segnette Closure (WBV162)') == 1)
		title2 = strcat('Bayou Segnette Closure (WBV-16.2) -  USACE Gage ID:',stations(cpraStationIndex),...
              {' - Datum Converstion to NAVD88: '},num2str(datumConvMap(gageID),'%0.2f'),{' ft'});
%             {' - '},'Datum Converstion to NAVD88 ',num2str(datumConvMap(gageID)));
	else
		title2 = strcat(cpraStationNames(cpraStationIndex),{'  -  '},gageAgency,{' Gage ID: '},stations(cpraStationIndex),...
            {' - Datum Converstion to NAVD88: '},num2str(datumConvMap(gageID),'%0.2f'),{' ft'});
	end
    
    text(0,1.07,title1,'Units','normalized','Interpreter','None');
    text(0,1.03,title2,'Units','normalized','Interpreter','None');
    
    % Add Legend
    triggerText = strcat('Water Level Trigger (',num2str(trigger(cpraStationIndex),'%4.1f'),' ft)');
    if (triggerL)
        legendCell{i+2} = triggerText;
        %legend('Gage Observations',enstorm,triggerText,'Location','northwest');
%     else
%         legend('Gage Observations',enstorm,'Location','northwest');
    end
%     legend(legendCell,'Location','northwest');
    legend(legendCell,'Location','northeast');
       
    % Override some defaults
    set(gca,'LineWidth',1,'TickLength',[0.015 0.015]);
    set(gca,'FontSize',12,'FontWeight','bold');
    
    hold off;
    
    % Plot Figure
    fig.PaperUnits = 'inches';
    fig.PaperPosition = [0 0 12.5 6.0];
    
    fname = char(strcat('WSE_',adcData(i).STATION{f}.NAME,'_',gageAgency,'_',gageID));
    print(fname,'-dpng','-r200');
    
    clear legendCell;
    
    msg = sprintf('cpra_hydrograph_plotter.m: Station %s jpeg success.', stations{1,cpraStationIndex});
    disp(msg);
    
    clear wl;
end

msg = sprintf('cpra_hydrograph_plotter.m: END cpra_hydrograph_plotter.m %s CDT',...
    datestr(datetime('now','TimeZone','America/Chicago'),'yyyymmdd HH:MM'));
disp(msg);

clear all; close all;
