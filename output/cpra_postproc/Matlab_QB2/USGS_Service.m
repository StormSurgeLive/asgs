%####################################################
% AUTHORS: Matthew V Bilskie, PhD
%          Louisiana State University
%
% COPYRIGHT 2018
%
% http://waterservices.usgs.gov/nwis/site/?<arguments>
%
% https://nwis.waterservices.usgs.gov/nwis/iv/?format=waterml,2.0&sites=02481270&startDT=2017-10-06&endDT=2017-10-17&parameterCd=00065&siteStatus=all
%
%####################################################

%function [wl dateTime]=USGS_Service(site,format,outputDataType,sDate,eDate,plotter)
function [TW]=USGS_Service(site,format,outputDataType,sDate,eDate,plotter)

% site = '02481270';
% format = 'waterml,2.0';
% outputDataType = '00065';
% sDate = '2017-10-01';
% eDate = '2017-10-17';
% plotter = true;

prefix = 'https://nwis.waterservices.usgs.gov/nwis/iv/?';

sDate = datestr(sDate,'yyyy-mm-dd');
eDate = datestr(eDate,'yyyy-mm-dd');

url = [prefix,'sites=',site,...
    '&startDT=',sDate,'&endDT=',eDate,...
    '&parameterCd=',outputDataType,...
    '&siteStatus=all'];

% urlwrite(url, 'temp.xml');
websave('temp.xml',url);

A=xml2struct('temp.xml');

try
    data = A.ns1_colon_timeSeriesResponse.ns1_colon_timeSeries.ns1_colon_values.ns1_colon_value;
catch ME
    msg = sprintf('USGS_Service.m: Gage data for USGS %s was NOT found!', site);
    disp(msg);
    TW = [];
    return
end

% fid = fopen('temp.imeds','w');
% fid = fopen(strcat('USGS_',site,'.txt'),'w');

numDataPoints = length(data);
wl = zeros(numDataPoints,1);
dateTime = zeros(numDataPoints,1);
for i=1:numDataPoints
    wl(i) = str2num(data{i}.Text);
    line = strtok(data{i}.Attributes.dateTime,'.');
    dateTime(i) = datenum(line,'yyyy-mm-ddTHH:MM:SS');
    dateTime(i) = dateTime(i) + (4/24); % Convert from EST to UTC
    %fprintf(fid,'%s\t%8.3f\n',datestr(dateTime(i),'yyyymmdd HH:MM'),wl(i));
end

wl(wl < -100) = NaN;

dateTimeStr = datestr(dateTime,'yyyymmdd HH:MM');
%fclose(fid);
delete temp.xml;

TW = [];
TW = [dateTime wl];

if plotter
    clf;
    ax = gca;
    plot(dateTime,wl,'color','b','linewidth',0.75);
    
    sDate = dateTime(1); eDate = dateTime(length(dateTime));
    
    ylim([floor(min(wl)) ceil(max(wl))]);
    ylabel('Water Surface Elevation (ft)');
    
    xlim([datenum(sDate)-0.25 datenum(eDate)+0.25]);
    xlabels = [datenum(sDate)-1:1:datenum(eDate)+1];
    set(gca,'XTick',xlabels);
    datetick('x','mm-dd-yy HH:MM','keeplimits','keepticks');
    ax.XTickLabelRotation = 90;
    
    title(strcat('USGS',site));
    
    set(gca,'fontsize',6);
    set(gcf,'PaperUnits','inches','PaperPosition',[0 0 6 3], 'color', 'white');
    fname = strcat('USGS_',site);
    print(fname,'-djpeg','-r300')
end
end
