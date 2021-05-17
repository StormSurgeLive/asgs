% Get data from TACC Frontera for NAM forecasts

% Example URL:
% https://fortytwo.cct.lsu.edu/thredds/catalog/2020/al13/08/LA_v20a-WithUpperAtch_chk/qbc.loni.org/LAv20a_al132020_jgf/veerLeft100/

%==========================================================================================
mesh='LA_v20a-WithUpperAtch_chk';  % mesh name
% mesh='LAv20a';  % mesh name
storm='al28';  % storm name (string)
year='2020';    % Storm year (string)
adv='17';         % Advisory
asgs_instance='LAv20a_al282020_jgf';
hpc='supermic.hpc.lsu.edu';
en={'nhcConsensus'};
prefix = 'https://fortytwo.cct.lsu.edu/thredds/fileServer/';

% mesh='LAv20a';  % mesh name
% asgs_instance='LAv20a_al282020_bde';
% en={'veerLeft50'};
% hpc = 'lonestar5.tacc.utexas.edu';
% prefix = 'http://adcircvis.tacc.utexas.edu:8080/thredds/fileServer/asgs/';

%==========================================================================================

url = [prefix year '/' storm '/' adv '/' mesh '/' hpc '/' asgs_instance '/'];

%download and rename the files
for i=1:length(en)
    % fort.61
    msg = sprintf('get_files_fortytwo.m: Attempting to download fort.61.nc');
    disp(msg);
    try
        url61 = [url en{i} '/fort.61.nc'];
        fname = [upper(storm) '_' adv '_' en{i} '.fort.61.nc'];
        websave(fname,url61);
        msg = sprintf(['get_files_fortytwo.m: SUCCESS downloading fort.61.nc: ', url61]);
        disp(msg);
    catch ME
        msg = sprintf(['get_files_fortytwo.m: **FAILED** to download fort.61.nc: ',url61]);
        disp(msg);
    end
    
    % run.properties
    msg = sprintf('get_files_fortytwo.m: Attempting to download run.properties');
    disp(msg);
    try
        urlrp = [url en{i} '/run.properties'];
        fname = [upper(storm) '_' adv '_' en{i} '.run.properties'];
        websave(fname,urlrp);
        msg = sprintf(['get_files_fortytwo.m: SUCCESS downloading run.properties: ', urlrp]);
        disp(msg);
    catch ME
        msg = sprintf(['get_files_fortytwo.m: **FAILED** to download run.properties: ',urlrp]);
        disp(msg);
    end
    
    % maxele.63
    msg = sprintf('get_files_fortytwo.m: Attempting to download maxele.63.nc');
    disp(msg);
    try
        urlmax = [url en{i} '/maxele.63.nc'];
        fname = [upper(storm) '_' adv '_' en{i} '.maxele.63.nc'];
        websave(fname,urlmax);
        msg = sprintf(['get_files_fortytwo.m: SUCCESS downloading maxele.63.nc: ', urlmax]);
        disp(msg);
    catch ME
        msg = sprintf(['get_files_fortytwo.m: **FAILED** to download maxele.63.nc: ',urlmax]);
        disp(msg);
    end
    
    % fort.15
    msg = sprintf('get_files_fortytwo.m: Attempting to download fort.15');
    disp(msg);
    try
        url15 = [url en{i} '/fort.15'];
        fname = [upper(storm) '_' adv '_' en{i} '.fort.15'];
        websave(fname,url15);
        msg = sprintf(['get_files_fortytwo.m: SUCCESS downloading fort.15: ', url15]);
        disp(msg);
    catch ME
        msg = sprintf(['get_files_fortytwo.m: **FAILED** downloading fort.15: ', url15]);
        disp(msg);
    end
    
    % fort.22
    msg = sprintf('get_files_fortytwo.m: Attempting to download fort.22');
    disp(msg);
    try
        url22 = [url en{i} '/fort.22'];
        fname = [upper(storm) '_' adv '_' en{i} '.fort.22'];
        websave(fname,url22);
        msg = sprintf(['get_files_fortytwo.m: SUCCESS downloading fort.61.nc: ', url22]);
        disp(msg);
    catch ME
        msg = sprintf(['get_files_fortytwo.m: **FAILED** to download fort.61.nc: ',url22]);
        disp(msg);
    end
end

%maxele figure
% maxfile = ['LA_SELA_' upper(storm) '_' adv '_nhcConsensus_maxele_0001.jpg'];
% url = ['http://fortytwo.cct.lsu.edu:8080/thredds/fileServer/tc/' storm '/' adv '/LA_v19k-WithUpperAtch_chk/queenbee.loni.org/' stormprefix 'nhcConsensus' '/' maxfile];
% websave(maxfile,url);