#!/usr/bin/env python
#----------------------------------------------------------------
# station_list_combiner.py: Combines station lists, dealing with
# duplicates appropriately.
#----------------------------------------------------------------
# Copyright(C) 2016 Jason Fleming
#
# This file is part of the ADCIRC Surge Guidance System (ASGS).
#
# The ASGS is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# ASGS is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with the ASGS.  If not, see <http://www.gnu.org/licenses/>.
#----------------------------------------------------------------
# Station files must be in standard metadata format, i.e.:
#
# lon lat ! stationID ! Agency ! location description ! vertical datum  
#
#----------------------------------------------------------------
import sys
from optparse import OptionParser
def main(argv):
    #
    # create command line option parser
    parser = OptionParser()
    # create options
    parser.add_option("-s", "--station-file-list", dest="stationfiles", default ="nullstations1.txt nullstations2.txt", help="names of station files to read ")
    parser.add_option("-o", "--output-station-file", dest="outputfile", default ="combined_station_list.txt", help="names of combined station file to write")
    # parse command line options
    (options, args) = parser.parse_args()
    # 
    # break station files into a list
    stationfilelist = options.stationfiles.split(" ")
    # 
    # declare dictionaries
    stationIDsLongitudes = {}
    stationIDsLatitudes = {}
    stationIDsAgencies = {}
    stationIDsLocationDescriptions = {}
    stationIDsVerticalDatums = {}
    #
    # loop over station files
    for stationfile in stationfilelist:
        sfile = open(stationfile,'r')
        for line in sfile: 
            # split the line into fields and populate the hashes against station ID
            a = line.split("!")
            longitude = ((a[0].strip()).split(" "))[0]
            latitude = ((a[0].strip()).split(" "))[1]
            stationID = a[1].strip()
            agency = a[2].strip()
            locationDescription = a[3].strip()
            verticalDatum = a[4].strip()
            # check to see if the stationID is a duplicate
            if stationID in stationIDsLongitudes.keys():
                # check to see if the duplicated stationID has same coordinates
                if stationIDsLongitudes[stationID] == longitude and stationIDsLatitudes[stationID] == latitude :   
                    # same ID, same coordinates, true duplicate, skip station
                    continue
                else:
                    # same ID, different coordinates, modify stationID
                    counter = 1
                    # append the letter d (for duplicate) and a number
                    # (the number of times this stationID was found in the 
                    # list)
                    while ( stationID in stationIDsLongitudes.keys() ):
                        stationID = stationID + 'd' + str(counter)
                        counter += 1
            stationIDsLongitudes[stationID] = longitude
            stationIDsLatitudes[stationID] = latitude
            stationIDsAgencies[stationID] = agency
            stationIDsLocationDescriptions[stationID] = locationDescription
            stationIDsVerticalDatums[stationID] = verticalDatum
        sfile.close()
    # now write stations to output file
    outfile = open(options.outputfile,'w') 
    for stationID in stationIDsLongitudes.keys():
        outfile.write(stationIDsLongitudes[stationID] + ' ' + stationIDsLatitudes[stationID] + ' ! ' + stationID + ' ! ' + stationIDsAgencies[stationID] + ' ! ' + stationIDsLocationDescriptions[stationID] + ' ! ' + stationIDsVerticalDatums[stationID] + '\n')
    outfile.close()

if __name__ == "__main__":
    main(sys.argv[1:])
