function [ imeds ] = read61nc(filename, coldStart)
%% function read61nc(filename)
% 
% Returns a formatted struct with IMEDS information
% contained.
% 
% Structure:
%    IMEDS ---> NumStations 
%               Station[NumStations] ----> NAME
%                                          X
%                                          Y
%                                          NumSnaps
%                                          DATE[NumSnaps] (date number)
%                                          DATA[NumSnaps]
%                                    
% OPTIONAl: coldStart YYYYMMDDhhmmss (example: 20050801000000)
%           If not specified it will be read in from the *.nc file.
%%
%---- LGPL --------------------------------------------------------------------
%
%
% This program is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License as published by
% the Free Software Foundation version 3.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public License
% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%
% contact: Matthew V Bilskie, mbilsk3@lsu.edu
% contact: Zachary Cobell, zachary.cobell@arcadis.com
% Louisiana State University
% Center for Coastal Resiliency
%
%
%------------------------------------------------------------------------------
% $Author$
% $Date$
% $Rev$
% $HeadURL$
%------------------------------------------------------------------------------
%  File: read61nc.m
%
%------------------------------------------------------------------------------
%%
%ncdisp('fort.61.nc');

staName = ncread(filename,'station_name');
imeds.NumStations = length(staName);
% Convert Station ID to string
for i = 1:imeds.NumStations
    % do some stuff
    sta1 = strtrim(staName(:,i));
    sta1 = cellstr(sta1); % Convert to cells
    sta1 = sta1(~cellfun('isempty',sta1)); % Removes empty cells
    sta1 = textscan(char(sta1),'%s');
    sta1 = char(sta1{1});
    stationNames{i} = sta1;
end


x = ncread(filename,'x');
y = ncread(filename,'y');

time = ncread(filename,'time');
if nargin < 2
    coldStart = ncreadatt(filename,'time','base_date');
    % Convert coldStart to Matlab DateTime
    coldStart = datetime(coldStart,'InputFormat','yyyy-MM-dd HH:mm:ss');
else
    coldStart = datetime(coldStart,'InputFormat','yyyyMMddHHmmss');
end
% Change time to dateTime
for i = 1:length(time)
    dt(i) = coldStart + seconds(time(i));
end

waterLevel = ncread(filename,'zeta');
% Replace NaN with -99999
waterLevel(isnan(waterLevel)) = -99999;

% Put everything into IMEDS format
for i=1:imeds.NumStations
        imeds.STATION{i}.NAME = stationNames{i};
        imeds.STATION{i}.X = x(i);
        imeds.STATION{i}.Y = y(i);
%         imeds.STATION{i}.DATE = dt;
        imeds.STATION{i}.DATE = datenum(dt);
        imeds.STATION{i}.DATA = waterLevel(i,:);
        imeds.STATION{i}.NumSnaps = length(time);
end
