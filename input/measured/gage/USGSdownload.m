
% returns USGS time-series data for specific station for specific parameter
% Max Agnew Maxwell.E.Agnew@usace.army.mil
% Might have bugs, be sure to convert gage height to datum
% Writes our txt then imports, maybe there is better way?
% example
% USGSdownload('07381600','2016-05-15','2016-06-15','00065')
% USGSdownload('07381600',datestr(now-15),datestr(now+1),'00060') %15days back



% station={'07381600'};
% begindate=datestr(now-15);
% enddate=datestr(now+1);
% parameter='00065';

%# Parameter Descriptions: http://nwis.waterdata.usgs.gov/usa/nwis/pmcodes?casrn_search=&format=html_table&pm_group=All+--+include+all+parameter+groups&pm_search=&radio_pm_search=param_group&show=casrn&show=parameter_group_nm&show=parameter_nm&show=parameter_units&show=srsname&srsname_search=
%#    DD parameter   Description
%#    01   00065     Gage height, feet
%#    23   00055     Stream velocity, feet per second
%#    24   00060     Discharge, cubic feet per second


function [TW]=USGSdownload(station,begindate,enddate,parameter)

bd=[datestr(begindate,'yyyy-mm-dd')];
ed=[datestr(enddate,'yyyy-mm-dd')];

disp(['downloading USGS station: ',char(station)])
urlwrite(['http://nwis.waterdata.usgs.gov/usa/nwis/uv/?cb_',parameter,'=on&format=rdb&site_no=',char(station),'&period=&begin_date=',bd,'&end_date=',ed],[char(station),'_',char(parameter),'.txt']);


%for complete file with all paramters
%urlwrite(['http://nwis.waterdata.usgs.gov/usa/nwis/uv/?format=rdb&site_no=',char(station),'&period=&begin_date=',bd,'&end_date=',ed],[char(station),'.txt']);


fileconductID = fopen([char(station),'_',char(parameter),'.txt']);
waterData = textscan(fileconductID,'%s %s %s %s %s %s %s %s %s ' , 'Delimiter', '\t', 'CommentStyle', '#');

for f = 1:length(waterData)
    
    xi=char(waterData{1,f}(1,1));
    
if isempty(xi)==0
    
    if strmatch(xi,'datetime')==1  
      for g = 3:length(waterData{1,f}(:,1))
         TW(g-2,1)=datenum(waterData{1,f}(g,1));
      end
    end

    if strmatch(xi((end-(length(parameter)-1)):end),parameter)==1  
     for g = 3:length(waterData{1,f}(:,1))
        TW(g-2,2)=str2num(waterData{1,f}{g,1});
     end
    end
    
end
end

fclose all;