#!/usr/bin/env python
import sys, os
from netCDF4 import Dataset

def main(argv):
    if  ( len(sys.argv) !=2 ):
        fname='fort.61.nc'
    else:
        fname=sys.argv[1]

    if os.path.isfile(fname):
        fname=fname
    elif os.path.isdir(fname):
        fname=fname + "/fort.61.nc"
    else:
        print("%s not found\n" %fname)
        print("Syntax: FortCheck.py <looks for fort.61.nc locally.>")
        print("Syntax: FortCheck.py directory_name <will look for fort.61.nc in directory_name.>")
        sys.exit()
    
    ds=Dataset(fname,"r")
    rnday=ds.rnday
    nt=len(ds.variables['time'])
    time=ds.variables['time']
    #print nt,rnday,len(time)

    ds.close

    if (time.size == 0):
        pct=0
    else:
        dt=time[1]-time[0]
        t1=time[0]/86400
        tnow=time[-1]/86400
        rt=rnday-t1
        pct=100*(tnow-t1)/rt
    
    print("%.2f" % (pct))

if __name__ == "__main__":
    	main(sys.argv[1:])

