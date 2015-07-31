## Importing all the necessary Python Modules ##
import matplotlib
## matplotlib.use('Agg') is used to specify a matplotlib backend ##
## which if not specified creates an error while running in cluster##
matplotlib.use('Agg')
import pylab as pl
import matplotlib.pyplot as pplot
from shapely.geometry import mapping, Polygon, LineString, LinearRing, Point
import fiona
import netCDF4
import datetime
import time
import numpy as np
import collections
import simplekml
import math

## time0 : the current time denoting the time when program run starts ##
time0=time.time()

## Menu ##
print (30 * '-')
print ("   M A I N - M E N U")
print (30 * '-')
print ("1. BATHYMETRY")
print ("2. MAXIMUM WATER ELEVATION")
print ("3. MAXIMUM WIND VELOCITY")
print ("4. MAXIMUM WAVE HEIGHT")
print ("5. NODAL ELEVATION")
print ("6. NODAL WIND VELOCITY")
print ("7. NODAL WAVE HEIGHT")
print ("8. PEAK WAVE PERIOD")
print ("9. NODAL PEAK WAVE PERIOD")
print ("10. NODAL AVERAGE WAVE PERIOD")
print ("A. POLYLINE")
print ("B. POLYGON")
print ("X. GIS SHAPEFILE")
print ("Y. GOOGLE EARTH") 
print (30 * '-')

     
## ACCEPTING USER INPUT ##
storm = raw_input('Enter name of storm :')
choice = raw_input('Enter your choice of ADCIRC output to be visualized [1-10] : ')
filechoice = int(choice)
choice = raw_input('Enter your choice of vector shape (Polyline or Polygon) (A/B): ')
if choice == 'A':
    shape = 'polyline'
elif choice == 'B':
    shape = 'polygon'
vchoice = raw_input('Enter your choice of visualization (GIS Shape File or Google Earth (KMZ)) (X/Y): ')
domain = raw_input('Do you want to make subplots first (with longlat boxes) and then plot full domain(Y/N)')

## DEFINING BINS FOR KML CREATION ##
if vchoice == 'Y':
    gdomain = [50,5,-60,-100]
    l= raw_input('Enter local box : NorthLat SouthLat EastLong WestLong :')
    local = map(float,l.split())
    bins = []
    bins.append([local[1],gdomain[1],gdomain[2],gdomain[3]])
    north = local[1] + 0.5  
    south = local[1]
    while (north <= local[0]):       
        bins.append([north,south,gdomain[2],gdomain[3]])
        south = north
        north = north +0.5             
    bins.append([gdomain[0],local[0],gdomain[2],gdomain[3]])
    print bins

## ROUTINE TO CALCULATE CONTOUR LEVELS ##
def contlevels(min,max,n):
    x = (max-min)/n
    return range(min,int(max+x),int(x))

## INPUT INFORMATION - assigning appropriate .nc files ,variable name, number of contour levels, file name, color palette for visualization ##

## 1 - BATHYMETRY ##
if filechoice == 1:
    nc=netCDF4.Dataset('maxele.63.nc').variables
    #nc=netCDF4.Dataset('http://opendap.renci.org:1935/thredds/dodsC/ASGS/arthur/10/nc_inundation_v9.99/hatteras.renci.org/nchi/nhcConsensus/maxele.63.nc').variables
    vname='depth'
    var = nc[vname][:]
    #np.place(var,var > 500,[499.99])
    #num = int(raw_input('Enter number of levels : (multiple of 4 for kml)'))
    palettename = 'mesh-bathy.pal'
    levels = range(0,8500,500)
    #levels = range(0,550,25)
    #var = np.multiply(-1,var)
    print min(var)
    #var = np.multiply(-1,var)
    #levels = [-40,-30,-20,-10,-5,0,2,4,6,8,10,15,20,30,40,50,100,200,300,400,500,550]
    #levels = [-600, -400, -200, -100, -50,-30,-10,-6,-4,-2,-1,-0.8, -0.4, 0, 0.4, 0.8, 1, 2, 4, 8, 10,30,40]
    if vchoice == 'X':
        outputname = 'mesh-bathy'
    else:    
        foldname = 'Bathymetry'
        lonlatbuffer = float(raw_input('enter long/lat buffer: '))
## 2 - MAXIMUM WATER LEVELS ##
elif filechoice == 2:
    nc=netCDF4.Dataset('maxele.63.nc').variables
    #nc=netCDF4.Dataset('http://opendap.renci.org:1935/thredds/dodsC/ASGS/arthur/10/nc_inundation_v9.99/hatteras.renci.org/nchi/nhcConsensus/maxele.63.nc').variables
    vname='zeta_max'
    palettename = 'water-level.pal'
    var = nc[vname][:]
    print type(var)
    if vchoice == 'Y':
        var = var.filled(-99999)
    # Converting water levels in meter to feet 
    var = np.multiply(3.28084,var)
    np.place(var,var > 8,[7.99])    
    levels = [0,1,2,3,4,5,6,7,8]
    #levels = [0,0.25,0.5,0.75,1,1.25,1.5,1.75,2.0,2.25,2.5,2.75,3,3.25,3.5,3.75,4,4.25]  
    print levels
    if vchoice == 'X':
        outputname = 'water-level'
    else:
        foldname = 'Maximum-Water-Levels'
        lonlatbuffer = float(raw_input('enter long/lat buffer: '))
## 3 - MAXIMUM WIND VELOCITY ##        
elif filechoice == 3:
    nc=netCDF4.Dataset('maxwvel.63.nc').variables
    #nc=netCDF4.Dataset('http://opendap.renci.org:1935/thredds/dodsC/ASGS/arthur/10/nc_inundation_v9.99/hatteras.renci.org/nchi/nhcConsensus/maxwvel.63.nc').variables
    vname='wind_max'
    var = nc[vname][:]
    print type(var)
    palettename = 'wavht.pal' 
    #levels = range(0,int(max(nc[vname][:])+ 1),1)
    levels = [0,5,10,15,20,25,30,35,40,45]
    print levels
    if vchoice == 'X':
        outputname = 'wind-speed'
    else:
       foldname = 'Maximum Wind Velocity'         
       lonlatbuffer = float(raw_input('enter long/lat buffer: '))
## 4 - MAXIMUM WAVE HEIGHT ##
elif filechoice == 4:
    nc=netCDF4.Dataset('swan_HS_max.63.nc').variables
    #nc=netCDF4.Dataset('http://opendap.renci.org:1935/thredds/dodsC/ASGS/arthur/10/nc_inundation_v9.99/hatteras.renci.org/nchi/nhcConsensus/swan_HS_max.63.nc').variables
    vname='swan_HS_max'
    #palettename = 'wavht.pal'
    palettename = 'water-level.pal'
    var = nc[vname][:]
    if vchoice == 'Y':
        var = var.filled(-99999)
    # converting wave heights in meter to feet
    var = np.multiply(3.28084,var)
    np.place(var,var > 32,[31.99])
    levels = [0,4,8,12,16,20,24,28,32]
##    levels = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14]
    print levels
    if vchoice == 'X':
        outputname = 'wave-height'
    else:
       foldname = 'Maximum-Wave-Heights'
       lonlatbuffer = float(raw_input('enter long/lat buffer: '))
## 5 - NODAL WATER LEVEL ##
elif filechoice == 5:
    nc=netCDF4.Dataset('fort.63.nc').variables
    #nc=netCDF4.Dataset('http://opendap.renci.org:1935/thredds/dodsC/ASGS/arthur/10/nc_inundation_v9.99/hatteras.renci.org/nchi/nhcConsensus/fort.63.nc').variables
    vname='zeta'
    outputname = 'nodalelev'+'_'+ shape + '_'+ str(storm)   
    if vchoice == 'Y':
        print 'KMZ files cannot be created for fort.63.nc'
## 6 - NODAL WIND VELOCITY ##
elif filechoice == 6:
    nc=netCDF4.Dataset('fort.74.nc').variables
    #nc=netCDF4.Dataset('http://opendap.renci.org:1935/thredds/dodsC/ASGS/arthur/10/nc_inundation_v9.99/hatteras.renci.org/nchi/nhcConsensus/fort.74.nc').variables   
    # Trying to read in data using the opendap url created an unresolved runtime error
    windx = nc['windx'][:]
    windy = nc['windy'][:]
    wind = [[None for i in range(len(windx[j]))]for j in range(len(windx))]
    outputname = 'nodalwvel'+'_'+ shape + '_'+ str(storm)
    if vchoice == 'Y':
        print 'KMZ files cannot be created for fort.74.nc'
## 7 - NODAL WAVE HEIGHT ##
elif filechoice == 7:
    nc=netCDF4.Dataset('swan_HS.63.nc').variables
    #nc=netCDF4.Dataset('http://opendap.renci.org:1935/thredds/dodsC/ASGS/arthur/12/nc_inundation_v9.99/hatteras.renci.org/nchi/nhcConsensus/swan_HS.63.nc').variables
    vname = 'swan_HS'
    outputname = 'nodalwavht'+'_'+ shape + '_'+ str(storm)
    if vchoice == 'Y':
        print 'KMZ files cannot be created for swan_HS.63.nc'
## 8 - PEAK WAVE PERIOD ##    
elif filechoice == 8:
    nc = netCDF4.Dataset('swan_TPS_max.63.nc').variables
    #nc = netCDF4.Dataset('http://opendap.renci.org:1935/thredds/dodsC/ASGS/arthur/12/nc_inundation_v9.99/hatteras.renci.org/nchi/nhcConsensus/swan_TPS_max.63.nc').variables
    vname = 'swan_TPS_max'
    palettename = 'wavht.pal'
    var = nc[vname][:]
    if vchoice == 'Y':
        var = var.filled(-100)
    levels = [0,5,10,15,20,25]
    if vchoice == 'X':
        outputname = 'wave-period'
    else:
       foldname = 'Maximum-Wave-Period'
       lonlatbuffer = float(raw_input('enter long/lat buffer: '))
elif filechoice == 9:
    nc = netCDF4.Dataset('swan_TPS.63.nc').variables
    vname = 'swan_TPS'
    outputname = 'nodalpeakpd'+'_'+shape+'_'+str(storm)
elif filechoice == 10:
    nc = netCDF4.Dataset('swan_TMM10.63.nc').variables
    vname = 'swan_TMM10'
    outputname = 'avgpd'+'_'+shape+'_'+str(storm)
    
else:
    print "Error in input"
## Timestep variable ##
timestep = 'time'
    
## Extracting longitude and latitude of all mesh nodes ##
lon = nc['x'][:]
lat = nc['y'][:]

## Extracting element description details (list of 3 vertices specified for each triangle denoted by index number)from the output file ##
nv = nc['element'][:,:] -1
    
## Extracting time step information from the output file ##
time_var= nc['time']
## startdate specifies start time of ADCIRC computations ##
startdate = time_var.units

## Converting the time step information into datetime objects ##
dtime = netCDF4.num2date(time_var[:],startdate)

## Converting datetime objects to string format - YYMMDDHHMMSS ##
a = []
for j in dtime:
    a.append(j.strftime("%Y%m%d%H%M%S"))

## RUNNING TIME ##
print 'Finished organizing input information after %d seconds' % (time.time()-time0)

## FUNCTION TO CHECK IF VERTEX FALLS WITHIN A SPECIFIED LONG/LAT BOX##
def vertexcheck(LatN,LatS,LongE,LongW,vertex):
    if lon[vertex]<LongE and lon[vertex]>LongW and lat[vertex]>LatS and lat[vertex]<LatN:
        return True

## FUNCTION TO CREATE LOCAL MESH WITHIN SPECIFIED LONG/LAT BOX ##
def latlonbox(LatN,LatS,LongE,LongW,lonlatbuffer):
    nvertex= len(lon)
    nele = len(nv)
    locallat = []
    locallon = []
    varlocal = []
    localelem = []
    global2local = {}
    includeele1 = []
    includeele2 = []
    includevertex1 = []
    includevertex2 =[]
    includeele1 = [0]*nele
    includeele2 = [0]*nele
    includevertex1 = [0]*nvertex
    includevertex2 =[0]*nvertex
    LatN = LatN+lonlatbuffer
    LatS = LatS-lonlatbuffer
    LongE = LongE-lonlatbuffer
    LongW = LongW-lonlatbuffer
    for i in range(len(nv)):
        for j in nv[i]:
            if vertexcheck(LatN,LatS,LongE,LongW,j):
                includeele1[i] = 1
                includevertex1[nv[i][0]] = 1
                includevertex1[nv[i][1]] = 1
                includevertex1[nv[i][2]] = 1
                break
    for i in range(len(nv)):
        for j in nv[i]:
            if includevertex1[j] == 1:
                includeele2[i] = 1
                includevertex2[nv[i][0]] = 1
                includevertex2[nv[i][1]] = 1
                includevertex2[nv[i][2]] = 1
                break
    j = -1
    for i in range(len(lon)):
        if includevertex2[i] == 1:
            j = j+1
            global2local[i] = j
            locallat.append(lat[i])
            locallon.append(lon[i])
            varlocal.append(var[i])
                
    for i in range(len(nv)):
        if includeele2[i] == 1:
            localelem.append((global2local[nv[i][0]],global2local[nv[i][1]],global2local[nv[i][2]]))                   
    return locallat,locallon,localelem,varlocal

## READING IN A COLOR PALETTE FILE (.PAL) ##

linenum = 0
palette = dict()
palette['value'] = []
palette['r'] = []
palette['g'] = []
palette['b'] = []
if vchoice == 'Y':
    with open(palettename,'r') as file:      
        for line in file:
            linenum = linenum + 1
            if linenum >= 5:
                a = line.split(" ")
                palette['value'].append(float(a[0]))
                palette['r'].append(float(a[1]))
                palette['g'].append(float(a[2]))
                palette['b'].append(float(a[3]))

## INTERPOLATING FROM COLOR PALETTE BASED ON NUMBER OF LEVELS ##

def interpolate(num,palette):
    newpalette = dict()
    newpalette['value'] = []
    newpalette['r'] = []
    newpalette['g'] = []
    newpalette['b'] = []
    scale =  np.arange(0,1,1/num)
    for i in range(len(scale)):
        #print scale[i]
        newpalette['value'].append(scale[i])
        for j in range(len(palette['value'])):
            if scale[i] == palette['value'][j]:
                newpalette['r'].append(palette['r'][j])
                newpalette['g'].append(palette['g'][j])
                newpalette['b'].append(palette['b'][j])
                break
            elif scale[i] > palette['value'][j] and scale[i] < palette['value'][j+1]:
                newpalette['r'].append(int(palette['r'][j] + ((palette['r'][j+1]-palette['r'][j])/(palette['value'][j+1]
                                            -palette['value'][j]))*(scale[i]-palette['value'][j])))
                newpalette['g'].append(int(palette['g'][j] + ((palette['g'][j+1]-palette['g'][j])/(palette['value'][j+1]
                                            -palette['value'][j]))*(scale[i]-palette['value'][j])))
                newpalette['b'].append(int(palette['b'][j] + ((palette['b'][j+1]-palette['b'][j])/(palette['value'][j+1]
                                            -palette['value'][j]))*(scale[i]-palette['value'][j])))
                break
    RGB = list()
    for i in range(len(newpalette['r'])):
        RGB.append((newpalette['r'][i],newpalette['g'][i],newpalette['b'][i]))
    return RGB

## TO CONVERT FROM RGB COLOR CODE TO HEX COLOR CODE ##
def rgb_to_hex(rgb):
    return '#%02x%02x%02x' % rgb

## CREATE COLOR BAR FOR THE CONTOUR PLOT ##
def createcolorbar(hexlist,levels):
    fig = pplot.figure(figsize = (1.5,8))
    ax = fig.add_axes([0.3,0.05,0.3,0.9])
    cmap = matplotlib.colors.ListedColormap(hexlist[:(len(hexlist)-1)],'contour')
    norm = matplotlib.colors.BoundaryNorm(levels, cmap.N)
    cb = matplotlib.colorbar.ColorbarBase(ax, cmap = cmap,norm=norm,boundaries=levels,extend='neither',ticks=levels,spacing='proportional',orientation='vertical')
    if filechoice== 2:
        cb.set_label('Water Level (feet)')
        pl.savefig('Colorbar-water-levels.png')
    elif filechoice == 4:
        cb.set_label('Wave Height (feet)')
        pl.savefig('Colorbar-wave-heights.png')
    elif filechoice == 3:
        cb.set_label('Wind Speed (m/s)')
        pl.savefig('Colorbar-wind-speeds.png')       
    elif filechoice == 8:
        cb.set_label('Peak Wave Period (s)')
        pl.savefig('Colorbar-wave-periods.png')
    elif filechoice == 1:
        cb.set_label('Bathymetry (m)')
        pl.savefig('Colorbar-bathymetry.png')       

## CREATING SCREEN OVERLAYS (COLOR BAR AND LOGO) FOR THE KML FILE ##
def screen(kml):
    screen1 = kml.newscreenoverlay(name='Colorbar')
    if filechoice == 2:
        screen1.icon.href = 'Colorbar-water-levels.png'
    elif filechoice == 4:
        screen1.icon.href = 'Colorbar-wave-heights.png'
    elif filechoice == 8:
        screen1.icon.href = 'Colorbar-wave-periods.png'
    elif filechoice == 3:
        screen1.icon.href = 'CColorbar-wave-periods.png'
    elif filechoice == 1:
        screen1.icon.href = 'Colorbar-bathymetry.png'
    screen1.overlayxy = simplekml.OverlayXY(x=0,y=0,xunits=simplekml.Units.fraction,
                                       yunits=simplekml.Units.fraction)
    screen1.screenxy = simplekml.ScreenXY(x=0,y=0.1,xunits=simplekml.Units.fraction,
                                     yunits=simplekml.Units.fraction)
    screen1.size.x = 0.1
    screen1.size.y = 0.72
    screen1.size.xunits = simplekml.Units.fraction
    screen1.size.yunits = simplekml.Units.fraction
    screen2 = kml.newscreenoverlay(name='logo')
    screen2.icon.href = 'logo.png'
    screen2.overlayxy = simplekml.OverlayXY(x=0,y=1,xunits=simplekml.Units.fraction,
                                       yunits=simplekml.Units.fraction)
    screen2.screenxy = simplekml.ScreenXY(x=0,y=1,xunits=simplekml.Units.fraction,
                                     yunits=simplekml.Units.fraction)
    screen2.size.x = 0.85
    screen2.size.y = 0.08
    screen2.size.xunits = simplekml.Units.fraction
    screen2.size.yunits = simplekml.Units.fraction
    print 'Screen Overlay completed'
    
## GENERAL REQUIREMENTS FOR SHAPE FILES AND KML FILES ##
if vchoice == 'Y':
    ## CREATING A SIMPLEKML OBJECT ##
    kml = simplekml.Kml()
    box = simplekml.Box(north = 46.86, south = 4.32, east = -57.523, west = -90)
    lod = simplekml.Lod(minlodpixels=128, maxlodpixels=-1, minfadeextent=0, maxfadeextent=0)
    reg = simplekml.Region(box,lod)
    c = interpolate(float(len(levels)),palette)
    hex = []
    for i in c:
        hex.append(rgb_to_hex(i))
    createcolorbar(hex,levels)
    screen(kml)
else:
    ## DEFINING SPATIAL REFERENCE ##
    crs = {'no_defs': True, 'ellps': 'WGS84', 'datum': 'WGS84', 'proj': 'longlat'}
    ## DEFINING OGR DRIVER ##
    driver = 'ESRI Shapefile'


## Triangulating the entire domain ##
tri = matplotlib.tri.Triangulation(lon,lat,triangles=nv)
## geoms is an ordered dictionary which stores the details of geometry of the polygons which describe the individual contour levels ##
geoms = collections.OrderedDict()

def classify_polygons(polys):
    outer = []
    inner = []
    for p in polys:
        if signed_area(p) >= 0:
            outer.append(p)
        else:
            inner.append(p)
    return outer,inner

def points_inside_poly(points, polygon):
    p = matplotlib.path.Path(polygon)
    return p.contains_points(points)

def reverse_geometry(p):
  return np.flipud(p)

## To calculate the signed area of an irregular polyon ##
def signed_area(ring):
    """Return the signed area enclosed by a ring in linear time using the 
    algorithm at: http://www.cgafaq.info/wiki/Polygon_Area.
    """
    #xs, ys = ring.coords.xy
    #xs.append(xs[1])
    #ys.append(ys[1])
    #return sum(xs[i]*(ys[i+1]-ys[i-1]) for i in range(1, len(ring.coords)))/2.0
    v2 = np.roll(ring, -1, axis=0)
    return np.cross(ring, v2).sum() / 2.0
     
## CREATING CONTOUR AND EXTRACTING LINESTRINGS/POLYGONS FOR EVERY TIMESTEP ##
for i in range(len(time_var)):
    print "Time step "
    print i
    if vchoice == 'Y':
        fol = kml.newfolder(name = foldname)
        fol.region = reg
        store = {}
        for k in range(len(levels)):
            multipol = fol.newmultigeometry(name= 'Level' + str(k))
            store[k] = multipol
    if filechoice == 6:
        wind[i] = []
        for j in range(len(windx[i])):
            wind[i].append(math.sqrt(windx[i][j]**2+windy[i][j]**2))
    print vchoice,domain,choice
    if vchoice == 'Y' and domain == 'Y' and choice == 'B':
        ## Creating polygon KML files by combining contour plots of subdomains specified in bins ##
        for v in bins:
            ## Printing the long/lat box ##       
            print v
            localy = []
            localx =[]
            localelements = []
            ## Extracting local mesh for each long/latbox ## 
            localy,localx,localelements,localvar = latlonbox(v[0],v[1],v[2],v[3],lonlatbuffer)
            if localelements ==[]:
                print localelements
                continue
            ## Triangulating for each local mesh ##
            tri = matplotlib.tri.Triangulation(localx,localy,triangles=localelements)
                      
            ## Plotting filled contour for each local mesh ##
            contour = pplot.tricontourf(tri, localvar,levels=levels,shading='faceted')
          
            m = 0           
            for colli,coll in enumerate(contour.collections):
                vmin,vmax = contour.levels[colli:colli+2]
                print 'Level %d' %m
                print vmin,vmax
                ## Extracting the path objects corresponding to each contour level ##         
                for p in coll.get_paths():
                    p.simplify_threshold = 0.0
                    ## converting each path object to a set of polygons  - polys ##
                    #polys = []
                    #polys = p.to_polygons()
                    #j = -1
                    ## Deleting any polygons having less than 3 vertices ## 
                    polys = [g for g in p.to_polygons() if g.shape[0] >=3] 
                    if len(polys)>0:
                       #for k in polys:
                            #j = j+1
                            ## Deleting any polygons having less than 3 vertices ##
                            #if k.shape[0]<3:
                                #polys.pop(j)          
                       ## polys is converted to a list of tuples (polys1). Each tuple representing a polygon ##
                       polys1 = []
                       for g in polys:
                            polys1.append(list(map(tuple,g)))   
                       inner = []
                       outer = []
                       ## Finding out which of the polygons within polys1 are outer and inner polygons (or holes)
                       ## by calculating their signed area. If area is positive it is an outer polygon and if it is negative
                       ## it is a inner polygon
                       for i in range(len(polys1)):
                            polys1[i] = LinearRing(polys[i])
                            s = float(1.0)
                            if signed_area(polys1[i])/s >=0.0:
                                outer.append(list(polys1[i].coords))
                            else:
                                inner.append(list(polys1[i].coords))
                       ## Need to identify which are the inner polygons (from inner) which fall inside each of the outer polygons (from outer).
                       ## topo = {<index of Outer Polygon 1 in outer>:<index of polygon in inner which falls in this outer polygon>, <index of Outer Polygon 2 in outer>: <.......>, ...}         
                       topo = {}
                       for i in range(len(outer)):
                            topo[i] = []
                            for j in range(len(inner)):
                                inside = 0
                                for k in inner[j]:
                                    if Polygon(outer[i]).contains(Point(k)):
                                        inside = 1
                                        break
                                    else:
                                        inside = 0
                                        break
                                if inside == 1:
                                    topo[i].append(j)
                            pol = store[m].newpolygon(name= 'Level' + str(m))
                            pol.outerboundaryis = outer[i]
                            if topo[i]!= []:
                                holes = []
                                for k in topo[i]:
                                    holes.append(inner[k])
                                pol.innerboundaryis = holes[:]
                            pol.gxaltitudemode = simplekml.GxAltitudeMode.clampToSeaFloor
                ## Setting the kml multigeometry object properties ##
                store[m].visibility = 1
                store[m].style.polystyle.fill = 1
                print m
                store[m].style.polystyle.color = simplekml.Color.hex(hex[m][1:])
                store[m].style.polystyle.outline = 0
                store[m].style.linestyle.color = simplekml.Color.hex(hex[m][1:])
                ## These are all optional properties ##
                        #multipol.style.linestyle.width = 3.0 
                #s = "                         min = " + str(vmin) + " ft, max = " +str(vmax) +" ft" 
                #store[m].style.balloonstyle.text = s
                #store[m].style.balloonstyle.bgcolor = simplekml.Color.greenyellow
                #store[m].style.balloonstyle.textcolor = simplekml.Color.black
                #store[m].description = s
                m = m+1
                
    else:
               ## Writing shapefiles/KML files  for entire domain ##
               ## Writing KML files for entire domain in one step may not allow polygons to be rendered correctly ##
                if choice == 'A':
                    ## To create POLYLINE files ##
                    if filechoice == 5:
                        #levels = linspace(-1,var[i].data.max(),num)
                        levels = [0,1,2,3,4,5,6,7,8]
                        contour = pplot.tricontour(tri, var[i],levels=levels)                    
                    elif filechoice == 6:
                        levels = linspace(min(wind[i]),max(wind[i]),num)
                        contour = pplot.tricontour(tri,wind[i],levels=levels)
                    elif filechoice == 7:    
                        levels = linspace(0,var[i].data.max(),num)
                        contour = pplot.tricontour(tri, var[i],levels=levels)
                    else:
                        contour = pplot.tricontour(tri, var,levels = levels)
                    geoms[time_var[i]] = []
                    print time_var[i]
                    m = 0
                    for colli,coll in enumerate(contour.collections):
                        val = contour.levels[colli]
                        print 'Level %d' %m
                        print val
                        for pp in coll.get_paths():
                            if len(pp.vertices) > 1:
                                ## Extracting the vertices of each of the paths corrsponding to each contour level ##
                                ## and wrapping them up in a shapely Linestring object ##
                                geoms[time_var[i]].append((LineString(pp.vertices),val))
                        m = m + 1
                elif choice == 'B':
                    ## To create POLYGON files ##
                    if filechoice == 5:
                        #levels = np.linspace(-1,var[i].data.max(),num)
                        levels = [0,0.25,0.5,0.75,1,1.25,1.5,1.75,2.0,2.25,2.5]
                        var = nc[vname][i][:]
                        print levels
                        #var = np.multiply(3.28084,var)
                        #np.place(var,var > 8,[7.99])
                        if var.mask.any():
                            point_mask_indices = np.where(var.mask)
                            tri_mask = np.any(np.in1d(nv,point_mask_indices).reshape(-1,3),axis=1)
                            print len(tri_mask)
                            tri.set_mask(tri_mask)                
                        np.place(var,var > 2.5,[2.49])
                        np.place(var, (-100 < var) & (var < 0),[0])    
                        contour = pplot.tricontourf(tri, var,levels=levels)                    
                    elif filechoice == 6:
                        #levels = np.linspace(min(wind[i]),max(wind[i]),num)
                        #levels = [0,2.5,5,7.5,10,12.5,15,17.5,20,22.5,25,27.5,30,32.5,35,37.5,40,42.5,45]
                        levels = [0,5,10,15,20,25,30,35,40]
                        print levels
                        contour = pplot.tricontourf(tri,wind[i],levels=levels)
                    elif filechoice == 7:
                        var = nc[vname][i][:]
                        print type(var)                     
                        #var = np.multiply(3.28084,var)
                        # np.place(var,var > 10,[9.99])
                        #levels = np.linspace(0,var[i].data.max(),num)
                        levels = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14]
                        print type(var)
                        #print stop
                        if var.mask.any():
                            print "yes"
                            point_mask_indices = np.where(var.mask)
                            tri_mask = np.any(np.in1d(nv,point_mask_indices).reshape(-1,3),axis=1)
                            print len(tri_mask)
                            tri.set_mask(tri_mask)                
                        contour = pplot.tricontourf(tri, var,levels=levels)
                    elif filechoice == 9:
                        var = nc[vname][i][:]
                        print 'Maximum = {0}'.format(var.data.max())
                        np.place(var,var > 20,[19.99])
                        levels = [0,2,4,6,8,10,12,14,16,18,20]
                        #levels = [0,4,8,12,16,20,24,28,32]
                        if var.mask.any():
                            print "yes"
                            point_mask_indices = np.where(var.mask)
                            tri_mask = np.any(np.in1d(nv,point_mask_indices).reshape(-1,3),axis=1)
                            print len(tri_mask)
                            tri.set_mask(tri_mask)                                       
                        contour = pplot.tricontourf(tri, var,levels=levels)
                        pplot.show()
                    elif filechoice == 10:
                        var = nc[vname][i][:]
                        np.place(var,var > 15,[14.99])
                        print 'Maximum = {0}'.format(var.data.max())
                        #levels = [0,30,60,90,120,150,180,210,240]
                        #levels = [0,2,4,6,8,10,12,14,16]
                        levels = [0,4,8,16,20,24,28,32]
                        contour = pplot.tricontourf(tri, var,levels=levels)                       
                    else :
                        print var.dtype
                        # Identifying elements with masked values
                        if  filechoice != 1 and filechoice != 3 and var.mask.any():
                            point_mask_indices = np.where(var.mask)
                            tri_mask = np.any(np.in1d(nv,point_mask_indices).reshape(-1,3),axis=1)
                            tri.set_mask(tri_mask)

                        contour = pplot.tricontourf(tri, var,levels = levels)
                    geoms[time_var[i]] = []
                    m = 0
                    l = len(contour.collections)
                    for colli,coll in enumerate(contour.collections):
                        print 'Level %d'%m
                        vmin,vmax = contour.levels[colli:colli+2]
                        print vmin,vmax
                        if vchoice == 'Y':
                           multipol = fol.newmultigeometry(name= 'Level' + str(m))                         
                        for p in coll.get_paths():
                            p.simplify_threshold = 0.0
                            # Removing polygons which have less than threee coordinates to describe its boundary
                            polys = [g for g in p.to_polygons() if g.shape[0] >=3] 
                            ''' Extracting the vertices of each of the paths corrsponding to each contour level
                             and wrapping them up in shapely Polygon objects. These Polygons are classified into
                            outer and inner polygons and grouped accordingly. Some are classified as outer with
                            corresponding inner polygons that occur within them. Other self existing polygons (outer/inner)
                            are stored as separate outer polygons. Finally, the geometry information with the contour
                            levels is stored in geoms and later on written into the corresponding shapefile'''
                            outer,inner = classify_polygons(polys)
                            if len(inner)>0:
                                inner_points = [pts[0] for pts in inner]
                            overall_inout = np.zeros((len(inner),),dtype = np.bool)
                            
                            for out in outer:
                                if len(inner) > 0:
                                    inout = points_inside_poly(inner_points,out)
                                    overall_inout = np.logical_or(overall_inout, inout)
                                    out_inner = [g for f, g in enumerate(inner) if inout[f]]
                                    poly = Polygon(out, out_inner)
                                else:
                                    poly = Polygon(out)

                                # clean-up polygons (remove intersections)
                                if not poly.is_valid:
                                    poly = poly.buffer(0.0)
                                if poly.is_empty:
                                    continue
                                geoms[time_var[i]].append((poly,vmin,vmax))
                            # collect all interiors which do not belong to any of the exteriors
                            outer_interiors = [interior for s,interior in enumerate(inner) if not overall_inout[s]]
                            for k in outer_interiors:
                                poly = Polygon(reverse_geometry(k))
                                # clean-up polygons (remove intersections)
                                if not poly.is_valid:
                                    poly = poly.buffer(0.0)
                                if poly.is_empty:
                                    continue
                                geoms[time_var[i]].append((poly,vmin,vmax))
                            print len(geoms[time_var[i]])
                            if vchoice == 'Y':
                                ## Creating kml files for the whole domain - this is not recommended for very fne meshes ##
                                ## due to the restrictions on the maximum number of vertices (31000) for a kml polygon object. ##
                                ## This restriction does not allow the polygons to be plotted correctly ##
                                for i in range(len(polys)):
                                    polys[i] = np.vstack([polys[i],polys[i][0]])         
                                    pol = multipol.newpolygon(name = 'Polygon'+str(i))
                                    pol.outerboundaryis = polys[i]
                                multipol.visibility = 1
                                multipol.style.polystyle.fill = 1
                                multipol.style.polystyle.color = simplekml.Color.hex(d['hex'][m][1:])
                                multipol.style.polystyle.outline = 0
                                multipol.style.linestyle.color = simplekml.Color.hex(d['hex'][m][1:]) 
                                s = "       min = " + str(vmin) + " ft, max = " +str(vmax) +" ft"
                                pol.style.balloonstyle.text = s
                                pol.style.balloonstyle.bgcolor = simplekml.Color.brown
                                pol.style.balloonstyle.textcolor = simplekml.Color.black
                                pol.description = s
                        m = m+1
                    
## KEEPING TRACK OF RUNNING TIME ##
if  vchoice == 'X':
    print 'Finished contouring, extracting information and creating polygons after %d seconds'% (time.time()-time0)
else:
    print 'Finished contouring, extracting information and creating multigeometry polygons after %d seconds'% (time.time()-time0)

if vchoice == 'X':
    ## DEFINING SCHEMA & WRITING SHAPE FILE  ##                    
    if filechoice == 1 and choice == 'A':        
            schema = { 'geometry': 'LineString', 'properties': {'waterdepth': 'float'}}
            with fiona.open(outputname, 'w',driver,schema,crs) as c:
                for geom in geoms:
                    for g in geoms[geom]:
                        c.write({'geometry': mapping(g[0]),'properties': {'waterdepth': g[1]}})
    elif filechoice == 1 and choice == 'B':                
            schema = { 'geometry': 'Polygon', 'properties': {'dmin': 'float', 'dmax': 'float', 'davg':'float'}}
            with fiona.open(outputname, 'w',driver,schema,crs) as c:
                for geom in geoms:
                    for g in geoms[geom]:
                        c.write({'geometry': mapping(g[0]),'properties': {'dmin': g[1], 'dmax': g[2], 'davg': (g[1]+g[2])/2.0}})
    elif filechoice == 2 and choice == 'A':
            schema = { 'geometry': 'LineString', 'properties': {'maxelev': 'float'}}
            with fiona.open(outputname, 'w',driver,schema,crs) as c:
                for geom in geoms:
                    for g in geoms[geom]:
                        c.write({'geometry': mapping(g[0]),'properties': {'maxelev': g[1]}})
    elif filechoice == 2 and choice == 'B':
            schema = { 'geometry': 'Polygon', 'properties': { 'elemin': 'float', 'elemax': 'float', 'eleavg': 'float' } }
            with fiona.open(outputname, 'w', 'ESRI Shapefile', schema,crs) as c:
                for geom in geoms:
                    for g in geoms[geom]:
                        c.write({'geometry': mapping(g[0]),'properties': {'elemin': g[1], 'elemax': g[2],'eleavg': (g[1]+g[2])/2.0}})
    elif filechoice == 3 and choice == 'A':
            schema = { 'geometry': 'LineString', 'properties': {'wvel': 'float'}}
            with fiona.open(outputname, 'w',driver,schema,crs) as c:
                for geom in geoms:
                    for g in geoms[geom]:
                        c.write({'geometry': mapping(g[0]),'properties': {'wvel': g[1]}})
    elif filechoice == 3 and choice == 'B':
            schema = { 'geometry': 'Polygon', 'properties': { 'wvelmin': 'float', 'wvelmax': 'float', 'wvelavg': 'float' } }
            with fiona.open(outputname, 'w', 'ESRI Shapefile', schema,crs) as c:
                for geom in geoms:
                    for g in geoms[geom]:
                        c.write({'geometry': mapping(g[0]),'properties': {'wvelmin': g[1], 'wvelmax': g[2], 'wvelavg': (g[1]+g[2])/2.0}})
    elif filechoice == 4 and choice == 'A':
            schema = { 'geometry': 'LineString', 'properties': {'wvht': 'float'}}
            with fiona.open(outputname, 'w',driver,schema,crs) as c:
                for geom in geoms:
                    for g in geoms[geom]:
                        c.write({'geometry': mapping(g[0]),'properties': {'wvel': g[1]}})
    elif filechoice == 4 and choice == 'B':
            schema = { 'geometry': 'Polygon', 'properties': { 'wvhtmin': 'float', 'wvhtmax': 'float', 'wvhtavg': 'float' } }
            with fiona.open(outputname, 'w', 'ESRI Shapefile', schema,crs) as c:
                for geom in geoms:
                    for g in geoms[geom]:
                        c.write({'geometry': mapping(g[0]),'properties': {'wvhtmin': g[1], 'wvhtmax': g[2], 'wvhtavg': (g[1]+g[2])/2.0}})
    elif filechoice == 5 and choice == 'A':        
        schema = { 'geometry': 'LineString', 'properties': {'elevat': 'float','timestep':'str'}}
        with fiona.open(outputname, 'w',driver,schema,crs) as c:
            for geom in geoms:
                k = list(time_var).index(geom)
                print k
                for g in geoms[geom]:
                    c.write({'geometry': mapping(g[0]),'properties': {'elevat': g[1],'timestep':a[k]}})
        
    elif filechoice == 5 and choice == 'B':                
            schema = { 'geometry': 'Polygon', 'properties': { 'elemin': 'float', 'elemax': 'float','eleavg': 'float','timestep':'str' ,'t' : 'float'} }
            with fiona.open(outputname, 'w', 'ESRI Shapefile', schema,crs) as c:
                for geom in geoms:
                    k = list(time_var).index(geom)
                    print k
                    for g in geoms[geom]:
                        c.write({'geometry': mapping(g[0]),'properties': {'elemin': g[1], 'elemax': g[2],'eleavg': (g[1]+g[2])/2.0,'timestep':a[k],'t':time_var[k]}})
    elif filechoice == 6 and choice == 'A':        
        schema = { 'geometry': 'LineString', 'properties': {'wvel': 'float','timestep':'str'}}
        with fiona.open(outputname, 'w',driver,schema,crs) as c:
            for geom in geoms:
                k = list(time_var).index(geom)
                print k
                for g in geoms[geom]:
                    c.write({'geometry': mapping(g[0]),'properties': {'wvel': g[1],'timestep':a[k]}})

    elif filechoice == 6 and choice == 'B':                
        schema = { 'geometry': 'Polygon', 'properties': { 'wvelmin': 'float', 'wvelmax': 'float','wvelavg':'float','timestep':'str' ,'t' : 'float'} }
        with fiona.open(outputname, 'w', 'ESRI Shapefile', schema,crs) as c:
            for geom in geoms:
                k = list(time_var).index(geom)
                print k
                for g in geoms[geom]:
                    c.write({'geometry': mapping(g[0]),'properties': {'wvelmin': g[1], 'wvelmax': g[2], 'wvelavg': (g[1]+g[2])/2.0,'timestep':a[k],'t':time_var[k]}})
    elif filechoice == 7 and choice == 'A':        
        schema = { 'geometry': 'LineString', 'properties': {'wavht': 'float','timestep':'str'}}
        with fiona.open(outputname, 'w',driver,schema,crs) as c:
            for geom in geoms:
                k = list(time_var).index(geom)
                print k
                for g in geoms[geom]:
                    print type(g)
                    c.write({'geometry': mapping(g[0]),'properties': {'wavht': g[1],'timestep':a[k]}})

    elif filechoice == 7 and choice == 'B':                
            schema = { 'geometry': 'Polygon', 'properties': { 'wavhtmin': 'float', 'wavhtmax': 'float','wavhtavg':'float','timestep':'str' ,'t' : 'float'} }
            with fiona.open(outputname, 'w', 'ESRI Shapefile', schema,crs) as c:
                for geom in geoms:
                    k = list(time_var).index(geom)
                    print k
                    for g in geoms[geom]:
                        
                        c.write({'geometry': mapping(g[0]),'properties': {'wavhtmin': g[1], 'wavhtmax': g[2],'wavhtavg': (g[1]+g[2])/2.0,'timestep':a[k],'t':time_var[k]}})
                        
    elif filechoice == 8 and choice == 'B':
            schema = { 'geometry': 'Polygon', 'properties': { 'wvpdmin': 'float', 'wvpdmax': 'float', 'wvpdavg': 'float' } }
            with fiona.open(outputname, 'w', 'ESRI Shapefile', schema,crs) as c:
                for geom in geoms:
                    for g in geoms[geom]:
                        c.write({'geometry': mapping(g[0]),'properties': {'wvpdmin': g[1], 'wvpdmax': g[2], 'wvpdavg': (g[1]+g[2])/2.0}})

    elif filechoice == 9 and choice == 'B':                
            schema = { 'geometry': 'Polygon', 'properties': { 'peakpdmin': 'float', 'peakpdmax': 'float','peakpdavg':'float','timestep':'str' ,'t' : 'float'} }
            with fiona.open(outputname, 'w', 'ESRI Shapefile', schema,crs) as c:
                for geom in geoms:
                    k = list(time_var).index(geom)
                    print k
                    for g in geoms[geom]:
                        c.write({'geometry': mapping(g[0]),'properties': {'peakpdmin': g[1], 'peakpdmax': g[2],'peakpdavg': (g[1]+g[2])/2.0,'timestep':a[k],'t':time_var[k]}})                          
    elif filechoice == 10 and choice == 'B':                
            schema = { 'geometry': 'Polygon', 'properties': { 'meanpdmin': 'float', 'meanpdmax': 'float','meanpdavg':'float','timestep':'str' ,'t' : 'float'} }
            with fiona.open(outputname, 'w', 'ESRI Shapefile', schema,crs) as c:
                for geom in geoms:
                    k = list(time_var).index(geom)
                    print k
                    for g in geoms[geom]:
                        c.write({'geometry': mapping(g[0]),'properties': {'meanpdmin': g[1], 'meanpdmax': g[2],'meanpdavg': (g[1]+g[2])/2.0,'timestep':a[k],'t':time_var[k]}})                          


    
elif vchoice == 'Y':
    ## Specifying the kml file name and saving file ##
    filename = foldname +'.kml'
    kml.save(filename)

    
## KEEPING TRACK OF RUNNING TIME ##
if vchoice == 'X':
    print 'Finished generating shapefile after  %d seconds'% (time.time()-time0)
else:
    print 'Finished generating KML file after %d seconds'% (time.time()-time0)

        
 
                    
