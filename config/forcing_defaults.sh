#!/bin/bash
#----------------------------------------------------------------
# forcing_defaults.sh : Functions required for initializing
# parameters that are only related to the forcing.
#----------------------------------------------------------------
# Copyright(C) 2019--2024 Jason Fleming
#
# This file is part of the ADCIRC Surge Guidance System (ASGS).
#
# The ASGS is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# ASGS is distributed in the hope that it will be useful,
# # but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with the ASGS.  If not, see <http://www.gnu.org/licenses/>.
#----------------------------------------------------------------
THIS=$(basename -- $0)
#
# Tidal forcing
#
TIDEFAC="on"
tidalConstituents=( "m2" "s2" "n2" "k1" "k2" "o1" "q1" "p1" )
tidal_forcing=$TIDEFAC
tidefac_file="tide_fac.out" # need full path
tidal_potential_comment="tidal_potential_comment:notset"
tidal_boundary_comment="tidal_boundary_comment:notset"
#
# Meteorological Forcing: General
#
WTIMINC="notset"
BACKGROUNDMET="on"
TROPICALCYCLONE="off"
# if windExposure is set to "surface" then all nodal attributes
# will be used; if it is set to "10m", then the nodal attributes
# "surface_canopy_coefficient" and "directional_effective_roughness_length"
# will be deactivated
windExposure="surface"
#
# Meteorological Forcing : OWI Win/Pre ASCII Format (NWS=12)
#
declare -g -A owiWinPre
owiWinPre["NWSET"]=1      # number of win/pre ascii datasets (i.e., domains)
owiWinPre["NWBS"]=0       # number of blank snaps (i.e., datasets)
owiWinPre["DWM"]=1.0      # wind multiplier (unitless)
owiWinPre["startDateTime"]=1970010100  # yyyymmddhh24
owiWinPre["endDateTime"]=1980010100    # yyyymmddhh24
#
#  Meteorological Forcing : Tropical Cyclones
#
STORMNAME=stormname
storm_name="notset"
#RSSSITE=filesystem
#FTPSITE=filesystem
#FDIR=~/asgs/branches/2014stable/input/sample_advisories/2018
#HDIR=${FDIR}
STORM=07                         # storm number, e.g. 05=ernesto in 2006
YEAR=2018                        # year of the storm
TRIGGER="rssembedded"            # ftp|rss|rssembedded|ATCF|auto
RSSSITE="www.nhc.noaa.gov"       # site information for retrieving advisories
FTPSITE="ftp.nhc.noaa.gov"       # hindcast/nowcast ATCF formatted files
FDIR="/atcf/afst"                # forecast dir on nhc ftp site
HDIR="/atcf/btk"                 # hindcast dir on nhc ftp site
RMAX="default"
PERCENT="default"
VORTEXMODEL="GAHM"
PSEUDOSTORM="no"
BLADJ=0.9
#
#  N W S  0 8  C O N T R O L   N A M E L I S T
#
# symmetric vortex model control namelist
declare -g -A nws08Control
nws08Control["vortexModel"]="Holland" # Holland | CLE15
nws08Control["backgroundWindModel"]="radialVelocityWeighted" # radialVelocityWeighted | LC12
nws08Control["BCalc"]="limited"       # limited | exact
nws08Control["thetaLatDep"]=".false." # theta used in calculation of radius dependent upon latitude
nws08Control["useInflow"]=".false."   # whether to use inflow angle in translation speed calc
nws08Control["windspeed_averaging_minute"]="1" # 1 | 10
nws08Control["w_cool"]="2.0"          # magnitude of the radiative-subsidence rate in the free troposphere (Chavas and Lin, 2016)
nws08Control["CkCd_calc"]=".false."   # use constant (false) | best fit (true) for CkCd
nws08Control["CkCd"]="1.0"            # ratio of exchange coefficients of enthalpy (Ck) and momentum
nws08Control["WindMultiplier"]="1.0"  # wind multiplier applied to wind speed (default = 1.0, Holland model only)
nws08Control["write_nws08ControlNamelist"]="no"  # yes|no
nws08Control["write_vortexModel"]="nondefault"   # yes|no|nondefault
nws08Control["write_backgroundWindModel"]="nondefault"
nws08Control["write_BCalc"]="nondefault"
nws08Control["write_thetaLatDep"]="nondefault"
nws08Control["write_useInflow"]="nondefault"
nws08Control["write_windspeed_averaging_minute"]="nondefault"
nws08Control["write_w_cool"]="nondefault"
nws08Control["write_CkCd_calc"]="nondefault"
nws08Control["write_CkCd"]="nondefault"
nws08Control["write_WindMultiplier"]="nondefault"
#
#  Meteorological Forcing : North American Mesoscale (NAM) Model
#
FORECASTCYCLE="06"
BACKSITE="ftp.ncep.noaa.gov"          # NAM forecast data from NCEP
BACKDIR="/pub/data/nccf/com/nam/prod" # contains the nam.yyyymmdd files
FORECASTLENGTH=84                     # hours of NAM forecast to run (max 84)
PTFILE="ptFile_oneEighth.txt"         # the lat/lons for the OWI background met
ALTNAMDIR="/projects/ncfs/data/asgs5463","/projects/ncfs/data/asgs14174"
VELOCITYMULTIPLIER=1.0
SPATIALEXTRAPOLATIONRAMP=yes
SPATIALEXTRAPOLATIONRAMPDISTANCE=1.0
forecastSelection="latest"            # "latest" or "strict"
forecastDownload="only-to-run"        # "only-to-run" or "all"
#
#  Meteorological Forcing : Global Forecast System (GFS) Model
#
GFSBACKSITE="ftp.ncep.noaa.gov"             # forecast data from NCEP
GFSBACKDIR="/pub/data/nccf/com/gfs/v16.3"   # contains the GFS files
# GFS subset
declare -g -A gfsDomain
gfsDomain['leftlon']='-110'
gfsDomain['rightlon']='-45'
gfsDomain['toplat']='50'
gfsDomain['bottomlat']='0'
gfsDomain['coverage']='basin'
GFSFORECASTLENGTH=${GFSFORECASTLENGTH:-120} # hours of GFS forecast to run
#
# Subsetting downloaded grib2 data using wgrib2
# wgrib2 regridding expects longitude to range
# from 0 to 360 with origin at the Prime Meridian
# lat0, lon0 = degrees of lat/lon for 1st grid point
# nlon = number of longitudes
# nlat = number of latitudes
# dlon = grid cell size in degrees of longitude
# dlat = grid cell size in degrees of latitude
declare -g -A gfsLatLonGrid
gfsLatLonGrid['lon0']='-100'
gfsLatLonGrid['nlon']='600'
gfsLatLonGrid['dlon']='0.1'
gfsLatLonGrid['lat0']='5'
gfsLatLonGrid['nlat']='600'
gfsLatLonGrid['dlat']='0.1'
#
#  Meteorological Forcing : Rapid Refresh Forecast System (RRFS) Model
#
# set variables to be used in json template
declare -g -A rrfs
rrfs['BaseURL']=https://noaa-rrfs-pds.s3.amazonaws.com/rrfs_a
rrfs['PollingInterval']=5  # minutes
rrfs['LookAhead']=30       # how far in the future (from currentCycle) to look ahead for new data (days)
rrfs['ForecastLength']=84  # hours of RRFS forecast to run
# json file for status checker and RRFS downloader
rrfs['TemplateName']="get_rrfs_template.json"
rrfs['FilledTemplateName']="asgs_main.sh_get_rrfs_status.json"
declare -g -A rrfsDomain
rrfsDomain['coverage']='basin'
# RRFS regridding
declare -g -A rrfsLatLonGrid
rrfsLatLonGrid['lon0']='-100'
rrfsLatLonGrid['nlon']='1800'
rrfsLatLonGrid['dlon']='0.03'
rrfsLatLonGrid['lat0']='5'
rrfsLatLonGrid['nlat']='1800'
rrfsLatLonGrid['dlat']='0.03'
#
# Meteorological Blending
pureVortex=3.0
pureBackground=5.0
#
#
# External data sources : River Flux
#
RIVERSITE="ftp.nssl.noaa.gov"
RIVERDIR="/projects/ciflow/adcirc_info"
RIVERUSER="ldm"
RIVERDATAPROTOCOL="scp"
HINDCASTRIVERFLUX=null
RIVERINIT=null        # file (fort.88) containing cold start river water elevations
RIVERFLUX=null        # file containing aperiodic flux boundary condition data
VARFLUX=off           # off|on|default (on|default specifies aperiodic flux)
USERIVERFILEONLY=no
PERIODICFLUX=null     # static file containing periodic flux boundary condition
FLUXCALCULATOR=static # static|%filename%.pl (static if a static file should be used)
#
# Model coupling : SWAN
#
WAVES=off
REINITIALIZESWAN="no"
SWANHSCOMPRESSION="no"
SWANHSFULL="yes"
wave_model="SWAN"