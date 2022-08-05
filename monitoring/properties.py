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
#
inputPropertiesFile = "run.properties"
outputPropertiesFile = "run.properties"
#
#     C O M M A N D   L I N E   O P T I O N S
#
parser = OptionParser()
# property key
parser.add_option("-p", "--property", dest="prop")
# property value
parser.add_option("-v", "--value", dest="value", default="null")            
# path to run.properties file (optional)
parser.add_option("-d", "--dir", dest="directory", default='.')                
# name of input file ("run.properties" is the default) (optional)
parser.add_option("-i", "--input", dest="inputFile", default=inputPropertiesFile)
# name of input file ("run.properties" is the default) (optional)
parser.add_option("-o", "--output", dest="outputFile", default=outputPropertiesFile)
(options, args) = parser.parse_args()
#
# If only the property is given, the value is retrieved from the run.properties
# file and written to stdout; if both the key and the value options are
# present, then the property and its value are added (or replaced if they
# were already in the file)
#
# Open run.properties and make property dictionary  
#print("start")
runProp = dict()
try: 
    f = open(options.directory + '/' + options.inputFile,'r')
    for line in f:
        fields = line.split(':',1)
        runProp[fields[0].strip()] = fields[1].strip()
    f.close()
    #print("try")
except:
    print("properties.py: Could not open " + options.directory + "/" + options.inputFile + " file.")
    #print("except")
    exit()
#print "start"
#
# if the value is present, write the property; if not, read it 
if options.value != "null":
    #print("options.value is " + options.value)
    runProp[options.prop] = options.value
    try: 
        f = open(options.directory + "/" + options.outputFile,'w')
        # sort the keys
        sortedKeys = sorted(runProp.keys())
        # write all properties to the file
        #for key in runProp:
        for key in sortedKeys:
            f.write(key + " : " + runProp[key] + "\n")
        f.close()
    except:
        print("properties.py: Could not open " + options.directory + "/" + options.outputFile + " file.")
        exit()
else:
    #print("options.value is " + options.value)
    print(runProp[options.prop])
