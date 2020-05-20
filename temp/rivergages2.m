% returns rivergages data for specific station
% Max Agnew Maxwell.E.Agnew@usace.army.mil
% Might have bugs, be sure to check variable name is appropriate
% Writes our xml to temporary file then deletes, maybe there is better way?

% Modified by M.V. Bilskie - 05-21-2018

% example
% rivergages('01280','2016-05-15 3:00','2016-06-15 3:00','HG')
% rivergages('01280',datestr(now-15),datestr(now+1),'HG') %15days back
%http://rivergages.mvr.usace.army.mil/WaterControl/shefdata2.cfm?sid=01120&d=60&dt=S
% station=ll{f,3}
% begindate=datestr(R2{1,1}.time(1,1)+daybef+1)
% enddate=datestr(R2{1,1}.time(end,1)+7)
function [TW]=rivergages2(station,begindate,enddate,variable)
                            
% station='01120'
% begindate='2018-01-01 0:00'
% enddate='2018-04-10 0:00' %or datestr(now)
% variable='HG'

%DD=linspace(round(datenum(begindate)),round(datenum(enddate)),round((datenum(enddate)-datenum(begindate))/25));
% DD=linspace(round(datenum(begindate)),round(datenum(enddate)),round((datenum(enddate)-datenum(begindate))));

% This works
DD=linspace(datenum(begindate),datenum(enddate),round((datenum(enddate)-datenum(begindate))));

TW = [];
for g = 2:length(DD)
    
%     sDate = datestr(begindate,'yyyy-mm-ddThh:MM')
%     eDate = datestr(enddate,'yyyy-mm-ddThh:MM')

    bd=[datestr(DD(g-1),'yyyy-mm-dd'),'T',datestr(DD(g-1),'hh:MM')];
    ed=[datestr(DD(g),'yyyy-mm-dd'),'T',datestr(DD(g),'hh:MM')];
    
    % MVB
    % Replaced urlwrite with webread to obtain the xml file.
    % This was done because the site was moved from http to https.
    options=weboptions;
    options.CertificateFilename=(''); options.MediaType=('text/xml');
    url = ['https://rivergages.mvr.usace.army.mil/watercontrol/webservices/rest/webserviceWaterML.cfc?meth=getValues&site=',station,'&location=',station,'&variable=',variable,'&beginDate=',bd,'&endDate=',ed,'&authToken=RiverGages&method=RGWML'];
    data = webread(url, options);
    fid = fopen('temp.xml','wt'); fprintf(fid,data); fclose(fid);
    A = xml2struct('temp.xml');
    
    %urlwrite(url,'Gtest.xml','Timeout',20);
%     urlwrite(['http://rivergages.mvr.usace.army.mil/watercontrol/webservices/rest/webserviceWaterML.cfc?meth=getValues&site=',station,'&location=',station,'&variable=',variable,'&beginDate=',bd,'&endDate=',ed,'&authToken=RiverGages&method=RGWML'],'Gtest.xml','Timeout',20);
    %urlwrite(['http://rivergages.mvr.usace.army.mil/watercontrol/webservices/rest/webserviceWaterML.cfm?meth=getSiteInfo&site=',station,'&authToken=RiverGages'],'Gtestsiteinfo.xml');

    %A=xml2struct('Gtest.xml');
    %B=xml2struct('Gtestsiteinfo.xml');

    %disp('Reading rivergages data:');
    %disp(B.sitesResponse.site.siteInfo.siteName.Text);

    delete('temp.xml')
    %delete('Gtestsiteinfo.xml')
    if isfield(A.timeSeriesResponse.timeSeries.values,'value')==1
        for f =1:length(A.timeSeriesResponse.timeSeries.values.value)
            if g ==2 && f==1
                TW=[datenum( [A.timeSeriesResponse.timeSeries.values.value{1,f}.Attributes.dateTime(1:10),' ',A.timeSeriesResponse.timeSeries.values.value{1,f}.Attributes.dateTime(12:19)])      str2num(A.timeSeriesResponse.timeSeries.values.value{1,f}.Text)];
            else
                TW=[TW;[datenum( [A.timeSeriesResponse.timeSeries.values.value{1,f}.Attributes.dateTime(1:10),' ',A.timeSeriesResponse.timeSeries.values.value{1,f}.Attributes.dateTime(12:19)])      str2num(A.timeSeriesResponse.timeSeries.values.value{1,f}.Text)]];
           end
        end
        %else
            %disp('no data available')
        end
end
end
