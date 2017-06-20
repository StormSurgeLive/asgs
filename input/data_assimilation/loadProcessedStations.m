%--------------------------------------------------------------------------
% function loadProcessedStations.m
%
% Loads data at station locations after it has been processed, e.g.,
% after averaging over some period of interest.
%--------------------------------------------------------------------------
% Copyright(C) 2017 Jason Fleming
%
% This program is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
% (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public License
% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
% read the file as written by stationProcessor.f90
%# rundes: cy:MATTHEW47 ASGS runid:nowcast agrid:not_set
%# stationID ! operationType ! timestart(s) ! timeend(s) ! (result ! numObservations (c=1,num_components))
function [refStations,refwl,nrstat] = loadProcessedStations(filrefwl)
disp(['INFO: loadProcessedStations.m: Loading reference (adcirc) water level data from the file ' filrefwl ' (specified by the parameter filrefwl, as written by stationProcessor.f90.'])
waterLevelFileID=fopen(filrefwl,'r');
if waterLevelFileID==-1
   error(['offsetSurfaceGen.m: Failed to open reference (adcirc) water level file ' filrefwl '.'])
end
% first two lines are header lines
header1 = fgetl(waterLevelFileID);
disp(['The first header line from the file ' filrefwl ' is ' header1 '.']);
header2 = fgetl(waterLevelFileID);
disp(['The second header line from the file ' filrefwl ' is ' header2 '.']); 
field1 = 'refStationID';
field2 = 'refOperationType';
field3 = 'refTimeStart';
field4 = 'refTimeEnd';
field5 = 'refWaterLevel';
field6 = 'refNumObs';
tline = fgetl(waterLevelFileID);
tline
numRefStations = 0;
while ischar(tline)
   tlineTrimmed = strtrim(tline); % trim leading and trailing spaces
   c = strsplit(tlineTrimmed); % split on spaces
   if numRefStations==0
      refStations = struct(field1,strtrim(c(1)),field2,strtrim(c(2)),field3,str2double(c(3)),field4,str2double(c(4)),field5,str2double(c(5)),field6,str2double(c(6)));  
   else
      refStations(end+1) =  struct(field1,strtrim(c(1)),field2,strtrim(c(2)),field3,str2double(c(3)),field4,str2double(c(4)),field5,str2double(c(5)),field6,str2double(c(6))); 
   end
   numRefStations = numRefStations + 1;
   tline = fgetl(waterLevelFileID);
end
status = fclose(waterLevelFileID);
refwl = cell2mat({refStations.refWaterLevel});
if status==-1
   error(['offsetSurfaceGen.m: Failed to close a file ' filrefwl '.'])
end
%
% TODO: Remove reference stations that don't have enough observations.
% It is an open question how many observations are enough. 
% Most of the time, nearly all stations will have the same number of
% observations, using the median number as the criterion will 
% effectively remove all stations that have anything less than the
% full set of observations. 
nrstat=numel(refwl); % compute number of stations where reference (adcirc) water level data are available
end
