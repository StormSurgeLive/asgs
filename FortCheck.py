import sys, os
import netCDF4 as nc

if  ( len(sys.argv) !=2 ):
    fname='fort.61.nc'
else:
    fname=sys.argv[1]

#print "%s\n" % fname

if os.path.isfile(fname):
   fname=fname
#    print "%s is a file\n" % fname
elif os.path.isdir(fname):
#    print "%s is a dir\n" % fname
    fname=fname + "/fort.61.nc"
else:
    print "%s not found\n" %fname
    print "Syntax: FortCheck.py <looks for fort.61.nc locally.>"
    print "Syntax: FortCheck.py directory_name <will look for fort.61.nc in directory_name.>"
    sys.exit()


#print "%s\n" % fname


ds = nc.Dataset(fname)
#print ds.variables.keys()

rnday=ds.rnday
nt=len(ds.variables['time'])
time=ds.variables['time']
#print "%d\n" % time.size
if (time.size == 0):
    pct=0
else:
    dt=time[1]-time[0]
    t1=time[0]/86400
    tnow=time[-1]/86400
    rt=rnday-t1
    pct=100*(tnow-t1)/rt
    
print "%s %.2f" % ( fname , pct)

if __name__ == "__main__":
    	main(sys.argv[1:])

