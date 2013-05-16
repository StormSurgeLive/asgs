#!/bin/bash
#
#Taylor Asher
#Updated August 13 2012

#Subfunctions
function readinput
{
 eval "exec $fidin<$filin"                                    #open input file
 
 read <&$fidin filfort13                                    #nodal attribute file name

 #Read in name of fort.14 file and initialize it
 read <&$fidin filfort14
 if [ ! -f $filfort14 ] ; then
    echo "!!!!!MESH FILE $filfort14 NOT FOUND, EXITING!!!!!"
    exit
 fi
 initfort14

 read <&$fidin fillc                                        #land cover data file name
 if [ ! -f $fillc ] ; then
    echo "!!!!!LAND COVER FILE $fillc NOT FOUND, EXITING!!!!!"
    exit
 fi
 read <&$fidin nattr                                        #number of attributes

 echo "Nodal attrs for $headerfort14">$filfort13
 echo "$nn" >>$filfort13
 echo "$nattr" >>$filfort13
#echo "DEBUG $fillc" 
 #Loop over the reading of the attribute names and properties
 for cntri in $(seq 1 $nattr) ; do
    read <&$fidin curnamattr
    echo "Reading attribute $cntri of $nattr, $curnamattr"
    case "$curnamattr" in
    $nammnasf)
       echo "Initializing $nammnasf"
       domnasf=1
       initmnasf
       ;;
    $nampwice)
       echo "Initializing $nampwice"
       dopwice=1
       initpwice
       ;;
    $namscc)
       echo "Initializing $namscc"
       doscc=1
       initscc
       ;;
    $namsderl)
       echo "Initializing $namsderl"
       dosderl=1
       initsderl
       ;;
    $namsshag)
       echo "Initializing $namsshag"
       dosshag=1
       initsshag
       ;;
    $namsss)
       echo "Initializing $namsss"
       dosss=1
       initsss
       ;;
    $namwris)
       echo "Initializing $namwris"
       dowris=1
       initwris
       ;;
    *)
       echo "Nodal attribute $curnamattr not found!!!"
       echo "Program exiting"
       eval "exec $fidin<&-"
       exit
       ;;
    esac

    #Append the info for the nodal attribute to the fort.13 file
    echo "$curnamattr" >>$filfort13
    echo "$curunit" >>$filfort13
    echo "$curvpn" >>$filfort13
    echo "$curdefval" >>$filfort13
 done
 
 eval "exec $fidin<&-"                                       #close input file
}
function initfort14
{
 #Open mesh file, read number of nodes, then close file
 eval "exec $fidfort14<$filfort14"
 read <&$fidfort14 headerfort14                             #fort.14 file header
 read <&$fidfort14 ne nn                                    #number of elements and nodes
 eval "exec $fidfort14<&-"
}
function initmnasf
{
 read <&$fidin fillc2mnasf                                  #file with table converting land cover to mnasf values
 if [ ! -f $fillc2mnasf ] ; then
    echo "!!!!!LAND COVER TO MNASF FILE $fillc2mnasf NOT FOUND, EXITING!!!!!"
    exit
 fi
 read <&$fidin diflcmnasf difdefmnasf                       #whether to use different land cover data (y=yes), whether different default mnasf (y=yes)
 if [ "$diflcmnasf" == 'y' ] ; then
    read <&$fidin fillcmnasf
 else
   fillcmnasf=$fillc
 fi
 if [ "$difdefmnasf" == 'y' ] ; then
    read <&$fidin defmnasf
 fi
 curunit=$unitmnasf; curvpn=$vpnmnasf; curdefval=$defmnasf
}
function initpwice
{
 curunit=$unitpwice; curvpn=$vpnpwice; curdefval=$defpwice
}
function initscc
{
 read <&$fidin fillc2scc
 if [ ! -f $fillc2scc ] ; then
    echo "!!!!!LAND COVER TO SCC FILE $fillc2scc NOT FOUND, EXITING!!!!!"
    exit
 fi
 read <&$fidin diflcscc
 if [ "$diflcscc" == 'y' ] ; then
    read <&$fidin fillcscc
 else
   fillcscc=$fillc
 fi
 curunit=$unitscc; curvpn=$vpnscc; curdefval=$defscc
}
function initsderl
{
 read <&$fidin sderlrmax                                    #max search distance for finding points in sderl
 read <&$fidin sderlrw                                      #weighting distance for sderl
 read <&$fidin sderlcalcmode                                #whether to do sector (1) or linear (2) calculation of sderl coefficients
 read <&$fidin fillc2sderl
 if [ ! -f $fillc2sderl ] ; then
    echo "!!!!!LAND COVER TO SDERL FILE $fillc2sderl NOT FOUND, EXITING!!!!!"
    exit
 fi
 read <&$fidin diflcsderl
 if [ "$diflcsderl" == 'y' ] ; then
    read <&$fidin fillcsderl
 else
   fillcsderl=$fillc
 fi
 curunit=$unitsderl; curvpn=$vpnsderl; curdefval=$defsderl
}
function initsshag
{
 read <&$fidin defsshag
 curunit=$unitsshag; curvpn=$vpnsshag; curdefval=$defsshag
}
function initsss
{
 read <&$fidin nseed
 echo $nseed >$filsssseedin
 for cntsss in $(seq 1 $nseed) ; do
    read <&$fidin seedx seedy
    echo "$seedx  $seedy" >>$filsssseedin
 done
 read <&$fidin difwetelelimsss
 if [ "$difwetelelimsss" == 'y' ] ; then
    read <&$fidin wetelelimsss
 elif [ "$difwetelelimsss" == 'n' ] ; then
    wetelelimsss=0
 elif [ "$difwetelelimsss" == 'sshagdef' ] ; then
    usesshag='y'                                            #this variable is currently not used, but may be in the future  
 else
    echo "!!!!!UNKNOWN VALUE FOR difwetelelimsss $difwetelelimsss, EXITING!!!!!"
    exit
 fi
 curunit=$unitsss; curvpn=$vpnsss; curdefval=$defsss
}
function initwris
{
 read <&$fidin wrisx1 wrisy1 wrisx2 wrisy2
 curunit=$unitwris; curvpn=$vpnwris; curdefval=$defwris
}


#Main
echo "Begin $0 `date`"


#Initializing whether to calculate selected nodal attributes
domnasf=0                                                   #mannings n at sea floor
dopwice=0                                                   #primitive weighting in continuity equation
doscc=0                                                     #surface canopy coefficient
dosderl=0                                                   #surface directional effective roughness length
dosshag=0                                                   #sea surface height above geoid
dosss=0                                                     #surface submergence state
dowris=0                                                    #wave refraction in swan

#Attribute default values
defmnasf='0.0'
defpwice='0.03'
defscc='1'
defsderl='0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0'
defsshag='0.0'
defsss='0'
defwris='1'

#Attribute names
nammnasf='mannings_n_at_sea_floor'
nampwice='primitive_weighting_in_continuity_equation'
namscc='surface_canopy_coefficient'
namsderl='surface_directional_effective_roughness_length'
namsshag='sea_surface_height_above_geoid'
namsss='surface_submergence_state'
namwris='wave_refraction_in_swan'

#Attribute values per node
vpnmnasf=1
vpnpwice=1
vpnscc=1
vpnsderl=12
vpnsshag=1
vpnsss=1
vpnwris=1

#Attribute units
unitmnasf='meters'
unitpwice='unitless'
unitscc='unitless'
unitsderl='meters'
unitsshag='meters'
unitsss='unitless'
unitwris='unitless'

#File names and file identifiers
fidin=10                                                    #input file identifier
fidfort14=14                                                #fort.14 file identifier
filin=$1                                                    #input file
filsssseedin='autosssseed.in'                               #input basin seed file for sss
filmnasfout='mnasf.out'
filpwiceout='pwice.out'
filsccout='scc.out'
filsderlout='sderl.out'
filsssout='sss.out'
filwrisout='wris.out'

#Executables
exmnasf='bin/mannings_n_finder_v10auto.x'
expwice='bin/tau0_genauto.x'
exscc='bin/surface_canopy_v6auto.x'
exsderl='bin/surface_roughness_calc_v16auto.x'
exsss='bin/sssider.x'
exwris='bin/wrisoutabox.x'


#Read and parse input file
if [ ! -f $filin ] ; then
   echo "!!!!!INPUT FILE $filin NOT FOUND, EXITING!!!!!"
   exit
fi
echo "Opening and reading input file $filin"
readinput
echo "Done reading input file $filin"


#Generate non-default values for each attribute, then write them to the 
#fort.13 file.  The generalized steps for this process are as follows:  
#Use the fortran routine to get all the nodes not matching their default 
#values, writing this out to a file.  Then, count the lines 
#in the file to determine the number of nodes without the default value.  
#With the necessary info, write the attribute name, number of non-default 
#nodes, and their values to the fort.13 file.  
if [ $domnasf == '1' ] ; then
   echo "Processing $nammnasf"
   echo "$exmnasf $filfort14 $filmnasfout $fillcmnasf $fillc2mnasf"
   $exmnasf $filfort14 $filmnasfout $fillcmnasf $fillc2mnasf
   nndmnasf=`cat $filmnasfout | wc -l`
   echo $nammnasf >>$filfort13
   echo $nndmnasf >>$filfort13
   cat $filmnasfout >>$filfort13
   rm $filmnasfout
fi
if [ $dopwice == '1' ] ; then
   echo "Processing $nampwice"
   $expwice $filfort14 $filpwiceout
   nndpwice=`cat $filpwiceout | wc -l`
   echo $nampwice >>$filfort13
   echo $nndpwice >>$filfort13
   cat $filpwiceout >>$filfort13
   rm $filpwiceout
fi
if [ $doscc == '1' ] ; then
   echo "Processing $namscc"
   $exscc $filfort14 $filsccout $fillcscc $fillc2scc
   nndscc=`cat $filsccout | wc -l`
   echo $namscc >>$filfort13
   echo $nndscc >>$filfort13
   cat $filsccout >>$filfort13
   rm $filsccout
fi
if [ $dosderl == '1' ] ; then
   echo "Processing $namsderl"
   $exsderl $filfort14 $filsderlout $fillcsderl $fillc2sderl $sderlrmax $sderlrw $sderlcalcmode
   nndsderl=`cat $filsderlout | wc -l`
   echo $namsderl >>$filfort13
   echo $nndsderl >>$filfort13
   cat $filsderlout >>$filfort13
   rm $filsderlout
fi
if [ $dosshag == '1' ] ; then
   echo "Processing $namsshag"
   echo $namsshag >>$filfort13
   echo 0 >>$filfort13
fi
if [ $dosss == '1' ] ; then
   echo "Processing $namsss"
   if [ "$difwetelelimsss" == 'sshagdef' ] ; then
      if [ "$dosshag" == '1' ] ; then
         wetelelimsss=$defsshag
      fi
   fi
   $exsss $filfort14 $filsssout $filsssseedin $wetelelimsss
   rm $filsssseedin
   nndsss=`cat $filsssout | wc -l`
   echo $namsss >>$filfort13
   echo $nndsss >>$filfort13
   cat $filsssout >>$filfort13
   rm $filsssout
fi
if [ $dowris = '1' ] ; then
   echo "Processing $namwris"
   $exwris $filfort14 $filwrisout $wrisx1 $wrisy1 $wrisx2 $wrisy2
   nndwris=`cat $filwrisout |wc -l`
   echo $namwris >>$filfort13
   echo $nndwris >>$filfort13
   cat $filwrisout >>$filfort13
   rm $filwrisout
fi



echo "Done $0 `date`"
