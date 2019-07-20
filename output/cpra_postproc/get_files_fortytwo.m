%config
%advisory number
storm='two'
adv='05';
en={'nhcConsensus','veerRight100','veerLeft100'};

%download and rename the files
for i=1:length(en)
    %fort.61
    url = ['http://fortytwo.cct.lsu.edu:8080/thredds/fileServer/tc/' storm '/' adv '/LA_v19k-WithUpperAtch_chk/queenbee.loni.org/al022019LAv19k/' en{i} '/fort.61.nc'];
    fname = ['Adv',adv,'.',en{i},'.fort.61.nc'];
    websave(fname,url);
    %properties
    url = ['http://fortytwo.cct.lsu.edu:8080/thredds/fileServer/tc/' storm '/' adv '/LA_v19k-WithUpperAtch_chk/queenbee.loni.org/al022019LAv19k/' en{i} '/run.properties'];
    fname = ['Adv',adv,'.',en{i},'.run.properties'];
    websave(fname,url);
end




