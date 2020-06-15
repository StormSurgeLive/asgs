mesh='LAv19k';  % mesh name
storm='';  % storm name (string)
stormnum='';  % Storm # for the Atlantic Basin (string)
year='';    % Storm year (string)
adv='';         % Two digits padded by a zero if advisory is 1-9. (string)
en={'nhcConsensus','veerRight100','veerLeft100'};
% en={'nhcConsensus'};

stormprefix=strcat('al',stormnum,year,mesh,'/');
storm = lower(storm);

%download and rename the files
for i=1:length(en)
    %fort.61
    url = ['http://fortytwo.cct.lsu.edu:8080/thredds/fileServer/tc/' storm '/' adv '/LA_v19k-WithUpperAtch_chk/queenbee.loni.org/' stormprefix en{i} '/fort.61.nc'];
    fname = ['Adv',adv,'.',en{i},'.fort.61.nc'];
    websave(fname,url);
    
    %properties
    url = ['http://fortytwo.cct.lsu.edu:8080/thredds/fileServer/tc/' storm '/' adv '/LA_v19k-WithUpperAtch_chk/queenbee.loni.org/' stormprefix en{i} '/run.properties'];
    fname = ['Adv',adv,'.',en{i},'.run.properties'];
    websave(fname,url);
    
end

%maxele figure
maxfile = ['LA_SELA_' upper(storm) '_' adv '_nhcConsensus_maxele_0001.jpg'];
url = ['http://fortytwo.cct.lsu.edu:8080/thredds/fileServer/tc/' storm '/' adv '/LA_v19k-WithUpperAtch_chk/queenbee.loni.org/' stormprefix 'nhcConsensus' '/' maxfile];
websave(maxfile,url);

%previous advisory
prevAdv=num2str(str2num(adv)-1,'%02i');
%fort.61
url = ['http://fortytwo.cct.lsu.edu:8080/thredds/fileServer/tc/' storm '/' prevAdv '/LA_v19k-WithUpperAtch_chk/queenbee.loni.org/' stormprefix 'nhcConsensus' '/fort.61.nc'];
fname = ['Adv',prevAdv,'.nhcConsensus.fort.61.nc'];
websave(fname,url);
%properties
url = ['http://fortytwo.cct.lsu.edu:8080/thredds/fileServer/tc/' storm '/' prevAdv '/LA_v19k-WithUpperAtch_chk/queenbee.loni.org/' stormprefix 'nhcConsensus' '/run.properties'];
fname = ['Adv',prevAdv,'.nhcConsensus.run.properties'];
websave(fname,url);




