function [ imeds ] = readIMEDS(filename)
%% function readIMEDS(filename)
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
%

%%
%---- LGPL --------------------------------------------------------------------
%
% Copyright (C)  Arcadis, 2011-2015.
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
% contact: Zachary Cobell, zachary.cobell@arcadis.com
% Arcadis
% 11001 W. 120th Ave, Suite 200
% Broomfield, CO 80021
%
% All indications and logos of, and references to, "Arcadis" 
% are registered trademarks of Arcadis, and remain the property of
% Arcadis. All rights reserved.
%
%------------------------------------------------------------------------------
% $Author$
% $Date$
% $Rev$
% $HeadURL$
%------------------------------------------------------------------------------
%  File: readIMEDS.m
%
%------------------------------------------------------------------------------
%%

% Sense the file type here
[~,~,ext]=fileparts(filename);

% NetCDF IMEDS Format
if strcmp('.nc',ext)
    ncid = netcdf.open(filename,'NOWRITE');
    dimid = netcdf.inqDimID(ncid,'numStations');
    [~,imeds.NumStations] = netcdf.inqDim(ncid,dimid);
    
    varid_stationName = netcdf.inqVarID(ncid,'stationName');
    varid_x = netcdf.inqVarID(ncid,'stationXCoordinate');
    varid_y = netcdf.inqVarID(ncid,'stationYCoordinate');
    
    x = netcdf.getVar(ncid,varid_x);
    y = netcdf.getVar(ncid,varid_y);
    
    for i=1:imeds.NumStations
        imeds.STATION{i}.NAME = strtrim(transpose(...
            netcdf.getVar(ncid,varid_stationName,[0 i-1],[200 1])));
        imeds.STATION{i}.X = x(i);
        imeds.STATION{i}.Y = y(i);
        
        time_char = sprintf('time_station_%4.4i',i);
        var_char  = sprintf('data_station_%4.4i',i);
        
        varid_time = netcdf.inqVarID(ncid,time_char);
        varid_data = netcdf.inqVarID(ncid,var_char);
        
        refDateString = netcdf.getAtt(ncid,varid_time,'referenceDate');
        refDate = datenum(refDateString);
        
        imeds.STATION{i}.DATE = refDate + netcdf.getVar(ncid,varid_time);
        imeds.STATION{i}.DATA = netcdf.getVar(ncid,varid_data);
        imeds.STATION{i}.NumSnaps = size(imeds.STATION{i}.DATE,1);
    end

    netcdf.close(ncid);
    
% Ascii IMEDS Format
else
    
    %open file
    fid = fopen(filename);

    %read data from file
    nline = 0;
    while(~feof(fid))
        nline = nline + 1;
        line = fgetl(fid); 
        temp = textscan(line,'%s','delimiter',' ','multipleDelimsAsOne',1);
        data{nline} = temp{1};
        [len(nline),junki] = size(data{nline});
    end
    fclose(fid);

    %organize data by station
    i = 4;
    j = 0;

    while(i<=nline)
        j = j + 1;

        %Header for each station
        imeds.STATION{j}.NAME = data{i}{1};
        imeds.STATION{j}.X    = str2double(data{i}{3});
        imeds.STATION{j}.Y    = str2double(data{i}{2});
        i = i + 1;

        if(i<=nline)
            k = 0;
            %Body of dates and data for each station
            while(i<=nline && (len(i)==7 || len(i)==6) )
               k = k + 1;
               if(len(i)==7)
                   y = str2double(data{i}{1});
                   M = str2double(data{i}{2});
                   d = str2double(data{i}{3});
                   h = str2double(data{i}{4});
                   m = str2double(data{i}{5});
                   s = str2double(data{i}{6});
                   z = str2double(data{i}{7});
               else
                   y = str2double(data{i}{1});
                   M = str2double(data{i}{2});
                   d = str2double(data{i}{3});
                   h = str2double(data{i}{4});
                   m = str2double(data{i}{5});
                   s = 0;
                   z = str2double(data{i}{6});
               end
               imeds.STATION{j}.DATE(k) = datenum(y,M,d,h,m,s);
               imeds.STATION{j}.DATA(k) = z;
               if(imeds.STATION{j}.DATA(k)==-999)
                   imeds.STATION{j}.DATA(k)=NaN;
               end
               i = i + 1;
            end
            imeds.STATION{j}.NumSnaps = k;
        end
    end

    imeds.NumStations = j;
end
