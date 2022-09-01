#!/usr/bin/env python3
#----------------------------------------------------------------------
# properties.py : Set and get key/value pairs from run.properties file.
#----------------------------------------------------------------------
# Copyright(C) 2018 Jason Fleming
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
#----------------------------------------------------------------------
from optparse import OptionParser
import re
#
propertiesFile = "run.properties"
#
#     C O M M A N D   L I N E   O P T I O N S
#
parser = OptionParser()
# FIXME: add ability to search for runid
# jobtype
parser.add_option("-j", "--jobtype", dest="jobtype")
# path to run.properties file (optional)
parser.add_option("-d", "--dir", dest="directory", default='.')                
# name of input file ("run.properties" is the default) (optional)
parser.add_option("-i", "--input", dest="inputFile", default=propertiesFile)
(options, args) = parser.parse_args()
#
# Open run.properties and make property dictionary  
runProp = dict()
try: 
    f = open(options.directory + '/' + options.inputFile,'r')
    for line in f:
        fields = line.split(':',1)
        runProp[fields[0].strip()] = fields[1].strip()
    f.close()
except:
    print("properties.py: Could not open " + options.directory + "/" + options.inputFile + " file.")
    exit()
#
# if the value is present, write the property; if not, read it 
for key in runProp:
    if re.search(r'time.*' + options.jobtype + '.*start', key): 
        print(key)
        #
        # convert start time to python datetime object
        #advisory_dt = datetime.strptime(runProp['time.forecast.valid.cdt'],'%Y%m%d%H%M%S')
        #advisory_dt_long = datetime.strftime(advisory_dt,'%b-%d-%Y %H:%M')
