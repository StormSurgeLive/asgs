clear all
close all

% All time are in UTC. The USACE values are adjusted from CDT to UTC
% by adding 5 hours.

% Read cpraHydro.info for specific run-time information
fileID = fopen('cpraHydro.info','r');
info = textscan(fileID,'%s\n');
fclose(fileID);
storm = info{1}{1}; adcGrid = info{1}{2}; forecastValid = info{1}{3};

% Need to get this in through some input information...
%dtAdvisory = datenum(2012,8,27,9,0,0);
dtAdvisory = datenum(forecastValid,'yyyymmddHHMMSS');

colors = [0.9290 0.6940 0.1250; 175/255 54/255 60/255];

% Loop through the number of ensemble simulations
numEns = 2;
ensFileNames = {'fort.61.imeds','fort.61.veerRight50.imeds'};
for i = 1:numEns
    % Read in ADCIRC time-series water levels
    adcData(i) = readIMEDS(char(ensFileNames(i)));
end

% Must be in the order for the fort.61 file
adcNames = {'17th St. Outfall Canal',...
    'Seabrook Complex (IHNC-01)',...
    'IHNC  Surge Barrier (IHNC-02)',...
    'West Closure Complex (WBV-90)',...
    'Hero Canal Stop-Log Gage (WBV-09b)',...
    'Oakville Sluic Gage (WBV-09a)',...
    'Bayou Segnette Closure (WBV-16.2)',...
    'Caernarvon Canal Sector Gate (LPV-149)',...
    'Bayou Dupre Sector Gate (LPV-149)',...
    'Western Tie-In (WBV-72/74)',...
    'Empire Floodgate (NOV-13)',...
    'Empire Lock (NOV-14)'};

% USACE stations
stations={'85625','76065','76030','76265','82762','82770','82742',...
    '85760','76010','82715','01440','01440'};
stationsName={'West End',...
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

sdate = round(adcData(1).STATION{1}.DATE(1));
edate = round(adcData(1).STATION{1}.DATE(length(adcData(1).STATION{1}.DATE)));
            
ax = gca;
fig = gcf;

for f = 1:length(stations)
       
    % Get USACE water level data for station f
    wl=rivergages2(stations{1,f},datestr(sdate-3),datestr(sdate),'HG');
    
    % Plot USACE Observations
    if isempty(wl) == 0
        wl(:,1) = wl(:,1) + 5/24; % Adjust time from CDT to UTC
    else
        wl(1,1) = sdate; wl(1,2) = -20; % Create a fake point for legend purposes
    end
    % Plot USACE water levels as scatter points
    scatter(wl(:,1),wl(:,2),50,'MarkerEdgeColor','black','Linewidth',1);hold on;
    
% -------------------------------------------------------------------------
    
    % Get ADCIRC data from station f and loop through each available ensemble
    for i = 1:numEns
        adcData(i).STATION{f}.DATA(adcData(i).STATION{f}.DATA < -999) = NaN;
        % Find min/max water surface elevation from ADCIRC result
        res = ~any(~isnan(adcData(i).STATION{f}.DATA(:)));
        if res == false
    %         maxWL = ceil(max(adcData.STATION{f}.DATA / 0.3048)) + 3;
    %         minWL = floor(min(adcData.STATION{f}.DATA / 0.3048)) - 2;

            % I want to find this before plotting so all charts have the same
            % min and max y-axis.
            maxWL = 18;
            minWL = -4;
        else
            % Create some dummy values so the legned can still be plotted
            adcData(i).STATION{f}.DATE(1) = sdate;
            adcData(i).STATION{f}.DATE(1) = sdate+0.01;
            adcData(i).STATION{f}.DATA(1) = -20;
            adcData(i).STATION{f}.DATA(2) = -20;

            % Force max and min water level bounds
            maxWL = 18;
            minWL = -4;
        end
    
        % Plot ADCIRC Simulation
        plot(adcData(i).STATION{f}.DATE,adcData(i).STATION{f}.DATA / 0.3048,...
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
    ylabel('Water Level (ft)');

    grid on;
    %ax.YMinorGrid = 'on';
    
% -------------------------------------------------------------------------
    
%     title1 = 'Storm:Isaac  -  grid:LA_v17a-WithUpperAtch_chk';
    title1 = strcat('Storm:',storm,' - grid:',adcGrid);
    title2 = strcat(adcNames(f),'  -  USACE Gage ID ',stations(f));
    
    text(0,1.07,title1,'Units','normalized','Interpreter','None');
    text(0,1.03,title2,'Units','normalized','Interpreter','None');
    
    % Add Legend
    legend('USACE Observations','nhcConsensus','veerRight50',...
        'Location','northwest');
    
    % Override some defaults
    set(gca,'LineWidth',1,'TickLength',[0.015 0.015]);
    set(gca,'FontSize',12,'FontWeight','bold');
    
    hold off;
    
    % Plot Figure
    fig.PaperUnits = 'inches';
    fig.PaperPosition = [0 0 12.5 5];
    fname = char(strcat(num2str(f),'_WSE_',stations(f)));
    print(fname,'-dpng','-r200');
end



