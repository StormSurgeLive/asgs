#!/usr/bin/env python
import sys          # load a library module
import getopt
import re
def main(argv):
   stationfile = 'stations.txt'
   bathy = 'bathy.txt'
   modeled = 'modeled.txt'
   measured = 'measured.txt'
   outputfile = ''
   try:
      opts, args = getopt.getopt(argv,"hs:b:m:n:o:",["stationfile=","bathy=","modeled=","measured=","outputfile="])
   except getopt.GetoptError:
      print 'hwm.py --stationfile <stationfile> --bathy <bathyfile> --modeled <modeledfile> --measured <measuredfile> --outputfile <outputfile>'
      sys.exit(2)
   for opt, arg in opts:
      if opt == '-h':
         print 'hwm.py --stationfile <stationfile> --bathy <bathyfile> --modeled <modeledfile> --measured <measuredfile> --outputfile <outputfile>'
         sys.exit()
      elif opt in ("-s", "--stationfile"):
         stationfile = arg
      elif opt in ("-b", "--bathy"):
         bathy = arg
      elif opt in ("-m", "--modeled"):
         modeled = arg
      elif opt in ("-n", "--measured"):
         measured = arg
      elif opt in ("-o", "--outputfile"):
         outputfile = arg            
   print 'stationfile is ',stationfile
   print 'bathy is ',bathy
   print 'modeled is ',modeled
   print 'measured is ',measured
   print 'outputfile is ',outputfile
   #
   # open station file and get lon lat
   lon = []
   lat = []
   for line in open(stationfile,'r'): 
      columns = line.split()
      lon.append(columns[0])
      lat.append(columns[1])     
   #
   # open bathy file and get bathy data at stations
   depth = []
   for line in open(bathy,'r'):
      columns = line.split()
      depth.append(columns[1])
   #
   # open measured data file and get high water marks (hwm)
   hwm = []
   for line in open(measured,'r'):
      columns = line.split(',')
      hwm.append(float(columns[7])*12.0*2.54/100.0) # convert from ft to m
   #
   # open modeled high water marks and get adcirc data
   modeldata = []
   for line in open(modeled,'r'):
      columns = line.split()
      modeldata.append(float(columns[1]))
   #
   # compute error
   error = []
   for i in zip(modeldata,hwm):
      error.append(i[0] - i[1])    
   #
   # open FigureGen data file and write
   # lon,lat,bath,measured,modeled,error 
   out = open(outputfile,'w')
   for i in zip(lon, lat, depth, hwm, modeldata, error):
      if i[4] != -99999.0:
         out.write(i[0] + ',' + i[1] + ',' + i[2] + ',' + str(i[3]) + ',' + str(i[4]) + ',' + str(i[5]) + '\n')
   out.close()

if __name__ == "__main__":
   main(sys.argv[1:])
