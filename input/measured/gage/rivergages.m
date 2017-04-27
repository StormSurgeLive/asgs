% returns rivergages data for specific station
% Max Agnew Maxwell.E.Agnew@usace.army.mil
% Might have bugs, be sure to check variable name is appropriate
% Writes our xml to temporary file then deletes, maybe there is better way?
% example
% rivergages('01280','2016-05-15 3:00','2016-06-15 3:00','HG')
% rivergages('01280',datestr(now-15),datestr(now+1),'HG') %15days back

function [TW]=rivergages(station,begindate,enddate,variable)
                            
% station='01280'
% begindate='2016-05-15 3:00'
% begindate='2016-05-15 3:00'
% enddate='2016-06-15 3:00' %or datestr(now)
% variable='HG'


bd=[datestr(begindate,'yyyy-mm-dd'),'T',datestr(begindate,'hh:MM')];
ed=[datestr(enddate,'yyyy-mm-dd'),'T',datestr(enddate,'hh:MM')];

urlwrite(['http://rivergages.mvr.usace.army.mil/watercontrol/webservices/rest/webserviceWaterML.cfm?meth=getValues&location=',station,'&variable=',variable,'&beginDate=',bd,'&endDate=',ed,'&authToken=RiverGages'],'Gtest.xml');
urlwrite(['http://rivergages.mvr.usace.army.mil/watercontrol/webservices/rest/webserviceWaterML.cfm?meth=getSiteInfo&site=',station,'&authToken=RiverGages'],'Gtestsiteinfo.xml');

A=xml2struct('Gtest.xml');
B=xml2struct('Gtestsiteinfo.xml');

disp('Reading rivergages data:');
disp(B.sitesResponse.site.siteInfo.siteName.Text);

delete('Gtest.xml')
delete('Gtestsiteinfo.xml')
for f =1:length(A.timeSeriesResponse.timeSeries.values.value)
    if f ==1
    TW=[datenum( [A.timeSeriesResponse.timeSeries.values.value{1,f}.Attributes.dateTime(1:10),' ',A.timeSeriesResponse.timeSeries.values.value{1,f}.Attributes.dateTime(12:19)])      str2num(A.timeSeriesResponse.timeSeries.values.value{1,f}.Text)];
    else
        TW=[TW;[datenum( [A.timeSeriesResponse.timeSeries.values.value{1,f}.Attributes.dateTime(1:10),' ',A.timeSeriesResponse.timeSeries.values.value{1,f}.Attributes.dateTime(12:19)])      str2num(A.timeSeriesResponse.timeSeries.values.value{1,f}.Text)]];
    end
end

