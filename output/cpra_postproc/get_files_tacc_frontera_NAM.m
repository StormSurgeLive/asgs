% Get data from TACC Frontera for NAM forecasts

% Example URL:
% http://adcircvis.tacc.utexas.edu:8080/thredds/fileServer/asgs/2020/nam/2020061206/LA_v20a-WithUpperAtch_chk/frontera.tacc.utexas.edu/LAv20a_nam_jgf/namforecast/run.properties

%==========================================================================================
mesh='LA_v20a-WithUpperAtch_chk';  % mesh name
storm='nam';  % storm name (string)
year='2020';    % Storm year (string)
adv='2020060706';         % NAM Cycle
asgs_instance='LAv20a_nam_jgf';
en={'namforecast'};
%==========================================================================================

prefix = 'http://adcircvis.tacc.utexas.edu:8080/thredds/fileServer/asgs/';
url = [prefix year '/' storm '/' adv '/' mesh '/frontera.tacc.utexas.edu/' asgs_instance '/'];

%download and rename the files
for i=1:length(en)
    % fort.61
    msg = sprintf('get_files_tacc_frontera_NAM.m: Attempting to download fort.61.nc');
    disp(msg);
    try
        url61 = [url en{i} '/fort.61.nc'];
        fname = [upper(storm) '_' adv '.fort.61.nc'];
        websave(fname,url61);
        msg = sprintf(['get_files_tacc_frontera_NAM.m: SUCCESS downloading fort.61.nc: ', url61]);
        disp(msg);
    catch ME
        msg = sprintf(['get_files_tacc_frontera_NAM.m: **FAILED** to download fort.61.nc: ',url61]);
        disp(msg);
    end
    
    % run.properties
    msg = sprintf('get_files_tacc_frontera_NAM.m: Attempting to download run.properties');
    disp(msg);
    try
        urlrp = [url en{i} '/run.properties'];
        fname = [upper(storm) '_' adv '.run.properties'];
        websave(fname,urlrp);
        msg = sprintf(['get_files_tacc_frontera_NAM.m: SUCCESS downloading run.properties: ', urlrp]);
        disp(msg);
    catch ME
        msg = sprintf(['get_files_tacc_frontera_NAM.m: **FAILED** to download run.properties: ',urlrp]);
        disp(msg);
    end
    
    % maxele.63
    msg = sprintf('get_files_tacc_frontera_NAM.m: Attempting to download maxele.63.nc');
    disp(msg);
    try
        urlmax = [url en{i} '/maxele.63.nc'];
        fname = [upper(storm) '_' adv '.maxele.63.nc'];
        websave(fname,urlmax);
        msg = sprintf(['get_files_tacc_frontera_NAM.m: SUCCESS downloading maxele.63.nc: ', urlmax]);
        disp(msg);
    catch ME
        msg = sprintf(['get_files_tacc_frontera_NAM.m: **FAILED** to download maxele.63.nc: ',urlmax]);
        disp(msg);
    end
    
    % fort.15
    msg = sprintf('get_files_tacc_frontera_NAM.m: Attempting to download fort.15');
    disp(msg);
    try
        url15 = [url en{i} '/fort.15'];
        websave('fort.15',url15);
        msg = sprintf(['get_files_tacc_frontera_NAM.m: SUCCESS downloading fort.15: ', url15]);
        disp(msg);
    catch ME
        msg = sprintf(['get_files_tacc_frontera_NAM.m: **FAILED** downloading fort.15: ', url15]);
        disp(msg);
    end
end

%maxele figure
% maxfile = ['LA_SELA_' upper(storm) '_' adv '_nhcConsensus_maxele_0001.jpg'];
% url = ['http://fortytwo.cct.lsu.edu:8080/thredds/fileServer/tc/' storm '/' adv '/LA_v19k-WithUpperAtch_chk/queenbee.loni.org/' stormprefix 'nhcConsensus' '/' maxfile];
% websave(maxfile,url);