%clc; 
clear all;

filename = 'maxele.63.nc';

xmin = -92.25;
xmax = -88.3387;
ymin = 28.5383;
ymax = 30.7884;


% Store ADCIRC data
x = ncread(filename,'x');
y = ncread(filename,'y');
zeta_max = ncread(filename,'zeta_max');
numNodes = length(zeta_max);

zeta_max = zeta_max * 3.28084;

etaMax = -999;
for i = 1:numNodes
    if (x(i) < xmax) && (x(i) > xmin) && (y(i) < ymax) && (y(i) > ymin)
        if zeta_max(i) > etaMax
            etaMax = zeta_max(i);
        end
    end
end

ietaMax = ceil(etaMax);
fileID = fopen('etaMax.txt','w');
fprintf(fileID,'%f\n',ietaMax);
fclose(fileID);
