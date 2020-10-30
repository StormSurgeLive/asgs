#!/usr/bin/env perl
#--------------------------------------------------------------------------
#
# adc2vtk.pl
#
# This script reformats ADCIRC input or output files to vtk xml format for
# visualization and/or analysis.
#--------------------------------------------------------------------------
# Copyright(C) 2010--2017 Jason Fleming
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
#--------------------------------------------------------------------------
#
use strict;
$^W++;
use Getopt::Long;
#
my %adcirctypes = ("maxele.63", "MaximumElevation",
                   "maxwvel.63", "MaximumWindSpeed",
                   "minpr.63", "MinimumBarometricPressure",
                   "fort.19", "AperiodicElevationBoundary",
                   "fort.63", "WaterSurfaceElevation",
                   "fort.64", "WaterCurrentVelocity",
                   "fort.73", "BarometricPressure",
                   "fort.74", "WindVelocity",
                   "gradient.txt","WaterSurfaceElevationGradient",
                   "maxgradient.txt","MaxWaterSurfaceElevationGradient",
                   "positives.100","PositiveElementNumbers",
                   "negatives.100","NegativeElementNumbers",
                   "absolutes.100","AbsoluteElementNumbers",
                   "fdrepeats.100","RepeatedElementNumbersWithinSubdomain",
                   "subdomains.100","SubdomainNumbers",
                   "noff.100","ElementWetDryState",
                   "noffornot.100","InconsistentElementWetDryState",
                   "nodecode.63","NodeWetDryState",
                   "ESLNodes.63","ElementalSlopeLimitAtNodesDiagnostic",                   
                   "residents.63","ResidentNodeNumbers",
                   "ghosts.63","GhostNodeNumbers",
                   "ghostmem.63","GhostNodeSubdomainMembership",
                   "absolutes.63","AbsoluteNodeNumbers",
                   "subdomains.63","SubdomainsFromFort18",
                   "psubdomains.63","SubdomainsFromPartmesh");
my $R = 6378206.4;           # radius of the earth
my $pi = 3.141592653589793;
my $deg2rad = 2*$pi/360.0;
our @x; # x coordinates or longitude
our @y; # y coordinates or latitude
our @z; # depth
our @conn; # list of nodal connectivity indices
my @time; # in seconds since cold start for each dataset in the file
my @timestep; # integer number, since coldstart, for each dataset in the file
#
# nodal attributes related variables
my $nattr; # number of nodal attributes in the fort.13
my $attrName; # the name of a single attribute
my %namesUnits; # relates the name of an attribute to its units
my %namesNumValues; # how many values at each node
my %namesDefaultValues; # the value(s) of the attribute at most nodes
my %namesNumNonDefaults; # how many of the nodes have nondefault values
my @attrValues; # at every node in the mesh
our $getNodeIndices;      # defined if the node array indices should be recorded
our @nodeIndices;         # node array indices from data file 
our $getElementIndices;   # defined if the element array indices should be recorded
our @elementIndices;      # element array indices from data file 
#
my $meshfile = "null";
my $cpp;  # 1 to reproject to cpp (carte parallelogrammatique projection)
   
my $slam0 = 265.5; # longitude at center of projection
my $sfea0 = 29.0;  # latitude at center of projection
my $datacentered = "PointData";
#
# If the storm characteristics change, but the track does not, the 
# track lines will plot right on top of each other. The jitter is
# a kludge to bump up the overlandSpeed and vmax tracks in the z 
# direction to differentiate them visually. 
my $jitter;
my @adcircfiles;    # fulldomain adcirc output file names, comma separated
                    # with no spaces
my @trackfiles;     # storm track files (fort.22) 
#
GetOptions(
           "jitter" => \$jitter,
           "meshfile=s" => \$meshfile,
           "cpp" => \$cpp,
           "slam0=s" => \$slam0,
           "sfea0=s" => \$sfea0,
           "getNodeIndices" => \$getNodeIndices,
           "getElementIndicess" => \$getElementIndices,           
           "trackfiles=s" => \@trackfiles,
           "adcircfiles=s" => \@adcircfiles
         );
#
# Process track files, producing a single VTP file containing all 
# the tracks that were listed on the command line
if ( !  @trackfiles ) {
   $trackfiles[0] = "none";
} else {
   @trackfiles = split(/,/,join(',',@trackfiles)); # get a list w/o commas
   my $outfile = "tracks.vtp";
   # start writing vtk-formatted file
   unless (open(OUT,">$outfile")) {
      stderrMessage("ERROR","Failed to open vtk file $outfile for writing: $!.");
      die;
   }
   # write header for VTP (track line file)
   printf OUT "<?xml version=\"1.0\"?>\n";
   printf OUT "<VTKFile type=\"PolyData\" version=\"0.1\" byte_order=\"LittleEndian\">\n";
   printf OUT "   <PolyData>\n";
   foreach my $file (@trackfiles) {
      stderrMessage("INFO","Processing $file.");
      # make sure we can actually open the adcirc file before going further
      unless (open(ADCIRCFILE,"<$file")) {
         stderrMessage("ERROR",
          "Failed to open ADCIRC file $file for reading: $!.");
         next;
      }
      my $z = 0.0;
      if (defined $jitter) {
         if ( $file =~ /Speed/ ) {
            $z = 20000.0;
         }
         if ( $file =~ /maxWind/ ) {
            $z = 40000.0;
         }
      }
      my $start_date = "null";
      my $previous_date;
      my $cycle = 0;
      my @vmax; # max wind speed at each track point; read in knots; written in m/s
      while(<ADCIRCFILE>) {
         my @fields = split(',',$_);
         my $date = $fields[2];
         if ( $start_date eq "null" ) {
            $previous_date = $date;
            $start_date = $date;
         } elsif ( $date != $previous_date ) {
            $cycle = $cycle + 1;
         }
         # grab coordinates of storm center
         $fields[6] =~/(\d+)(N|S)/; # tenths of degrees plus orientation, e.g., "217N"
         $y[$cycle] =  $1/10.0;         # convert from tenths of degrees to degrees
         my $yhemisphere = $2;
         if ( $yhemisphere eq "S" ) {
            $y[$cycle] *= -1.0;
         }
         $fields[7] =~/(\d+)(E|W)/; # tenths of degrees plus orientation, e.g., "767W"
         $x[$cycle] =  $1/10.0;         # convert from tenths of degrees to degrees
         my $xhemisphere = $2;
         if ( $xhemisphere eq "W" ) {
            $x[$cycle] *= -1.0;
         }                 
         # reproject to cpp if requested
         if ( defined $cpp ) {
            $x[$cycle] = $R*($x[$cycle]*$deg2rad-$slam0*$deg2rad)*cos($sfea0*$deg2rad);
            $y[$cycle] = $y[$cycle]*$deg2rad*$R;
         }
         $vmax[$cycle] = $fields[8] * 0.51444444; # convert knots to m/s
         $previous_date = $date;
      }
      close(ADCIRCFILE);
      my $numTrackPoints = $cycle+1;
      my $numLineSegments = $cycle;
      printf OUT "      <!-- from track file $file -->\n";
      printf OUT "      <Piece NumberOfPoints=\"$numTrackPoints\" NumberOfLines=\"$numLineSegments\">\n";
      #
      # vmax values at each track point
      printf OUT "         <PointData Scalars=\"vmax\">\n";
      printf OUT "            <DataArray type=\"Float64\" Name=\"vmax\" format=\"ascii\">\n";
      printf OUT "               ";
      for (my $i=0; $i<$numTrackPoints; $i++ ) {
         printf OUT $vmax[$i] . " "; 
      }
      printf OUT "\n";
      printf OUT "            </DataArray>\n";
      printf OUT "         </PointData>\n";
      #
      # track point locations
      printf OUT "         <Points>\n";
      printf OUT "            <DataArray NumberOfComponents=\"3\" type=\"Float64\" Name=\"PointLocations\" format=\"ascii\">\n";
      printf OUT "               ";
      for (my $i=0; $i<$numTrackPoints; $i++ ) {         
         printf OUT $x[$i] . " " . $y[$i] . " " . $z . "  "; 
      }
      printf OUT "\n"; 
      printf OUT "            </DataArray>\n";
      printf OUT "         </Points>\n";
      #
      # line connectivity/topology
      printf OUT "         <Lines>\n";
      printf OUT "            <DataArray type=\"Int32\" Name=\"connectivity\" format=\"ascii\">\n";
      printf OUT "               "; 
      for ( my $i=0; $i<$numTrackPoints; $i++ ) {
         printf OUT $i . " " . ($i+1) . "  "; 
      }
      printf OUT "\n";          
      printf OUT "            </DataArray>\n";
      printf OUT "            <DataArray type=\"Int32\" Name=\"offsets\" format=\"ascii\">\n";
      printf OUT "               ";
      for ( my $i=0; $i<$numTrackPoints; $i++ ) {
         printf OUT (2*($i+1)) . " "; 
      }
      printf OUT "\n";
      printf OUT "            </DataArray>\n";
      printf OUT "         </Lines>\n";
      printf OUT "      </Piece>\n";
   }      
   # write VTP track(s) file footer
   printf OUT "   </PolyData>\n";
   printf OUT "</VTKFile>\n";
   close(OUT);     
}

unless ( @adcircfiles ) {
   $adcircfiles[0] = "none";
} else {
   @adcircfiles = split(/,/,join(',',@adcircfiles)); # get a list w/o commas
}
#
if ( $meshfile eq "null" ) {
   stderrMessage("INFO","Mesh file name was not provided or the file was not found. adc2vtk.pl is finished.");
   exit;
}
unless (open(MESH,"<$meshfile")) {
   stderrMessage("ERROR","Failed to open mesh file $meshfile for reading: $!.");
   die;
}
# read number of nodes and number of elements from adcirc mesh file
my $agrid = <MESH>;     # read AGRID (comment line in mesh file)
my $line = <MESH>;        # read number of elements and number of points line
my @fields = split(' ',$line);
my $ne = $fields[0];
my $np = $fields[1];
# read the node table
for (my $i=0; $i<$np; $i++) {
   $line = <MESH>;
   @fields = split(' ',$line);
   # if node labels were specified
   if ( defined $getNodeIndices ) {
      $nodeIndices[$i] = $fields[0];
   }
   $x[$i] = $fields[1];
   $y[$i] = $fields[2];
   $z[$i] = $fields[3];
}
# reproject to cpp if requested
if ( defined $cpp ) {
   for (my $i=0; $i<$np; $i++) {
      $x[$i] = $R*($x[$i]*$deg2rad-$slam0*$deg2rad)*cos($sfea0*$deg2rad);
      $y[$i] = $y[$i]*$deg2rad*$R;
   }
}
# read the element table
for (my $i=0; $i<$ne; $i++) {
   $line = <MESH>;
   @fields = split(' ',$line);
   # if node labels were specified
   if ( defined $getElementIndices ) {
      $elementIndices[$i] = $fields[0];
   }
   # have to subtract 1 from the ADCIRC node numbers because vtk is 0 indexed
   my $i1 = $fields[2]-1;
   my $i2 = $fields[3]-1;
   my $i3 = $fields[4]-1;
   $conn[$i] = " $i1 $i2 $i3 ";
}
# 
# Now read the elevation-specified boundary tables and write out as vtkPoints
my $vtkElevationBoundaryFileName = $meshfile . "_elevBoundaries.vtp";
unless (open(VTKELEVBOUNDARY,">$vtkElevationBoundaryFileName")) {
   stderrMessage("ERROR","Failed to open $vtkElevationBoundaryFileName for writing: $!.");
   die;
}
$line = <MESH>;
@fields = split(' ',$line);
my $nope = $fields[0];
$line = <MESH>;
@fields = split(' ',$line);
my $neta = $fields[0];
# write header for boundaries file   
printf VTKELEVBOUNDARY "<?xml version=\"1.0\"?>\n";
printf VTKELEVBOUNDARY "<VTKFile type=\"PolyData\" version=\"0.1\" byte_order=\"LittleEndian\">\n";
printf VTKELEVBOUNDARY "   <PolyData>\n";
printf VTKELEVBOUNDARY "      <Piece NumberOfPoints=\"$neta\">\n";
printf VTKELEVBOUNDARY "         <Points>\n";
printf VTKELEVBOUNDARY "            <DataArray type=\"Float64\" NumberOfComponents=\"3\" format=\"ascii\">\n";
# read all boundary data into 1D arrays
my @elevBoundaryTypes; # ibtypee
my @elevBoundaryElevs; # bathytopo elevation
my @elevBoundaryLons;  # longitude (degrees E)
my @elevBoundaryLats;  # latitude (degrees N)
my @elevBoundaryNodes; # node number 1-indexed
my $elevBoundaryCount = 0;
for (my $i=0; $i<$nope; $i++) {
   $line = <MESH>;
   my @fields = split(' ',$line); 
   my $nvdll = $fields[0];
   # my $ibtypee = $fields[1]; # many mesh files don't have this field
   my $ibtypee = 0;
   for (my $j=0; $j<$nvdll; $j++) {
      my @nbdvFields = split(' ',<MESH>);
      my $nbdv = $nbdvFields[0];
      $elevBoundaryNodes[$elevBoundaryCount] = $nbdv;  
      $elevBoundaryLons[$elevBoundaryCount] = $x[$nbdv-1];
      $elevBoundaryLats[$elevBoundaryCount] = $y[$nbdv-1];
      $elevBoundaryElevs[$elevBoundaryCount] = $z[$nbdv-1];
      $elevBoundaryTypes[$elevBoundaryCount] = $ibtypee;
      $elevBoundaryCount++;
      printf VTKELEVBOUNDARY "$x[$nbdv-1] $y[$nbdv-1] 0.0 ";
   }
}        
printf VTKELEVBOUNDARY "\n";
printf VTKELEVBOUNDARY "            </DataArray>\n";
printf VTKELEVBOUNDARY "         </Points>\n";
printf VTKELEVBOUNDARY "         <PointData>\n";
printf VTKELEVBOUNDARY "            <DataArray Name=\"IBTYPEE\" type=\"Int32\" NumberOfComponents=\"1\" format=\"ascii\">";
for (my $i=0; $i<$elevBoundaryCount; $i++) {
   printf VTKELEVBOUNDARY " $elevBoundaryTypes[$i]";
}
printf VTKELEVBOUNDARY "            </DataArray>\n";
printf VTKELEVBOUNDARY "            <DataArray Name=\"Elevation\" type=\"Float64\" NumberOfComponents=\"1\" format=\"ascii\">";
for (my $i=0; $i<$elevBoundaryCount; $i++) {
   printf VTKELEVBOUNDARY " $elevBoundaryElevs[$i]";
}
printf VTKELEVBOUNDARY "            </DataArray>\n";
printf VTKELEVBOUNDARY "         </PointData>\n";
printf VTKELEVBOUNDARY "      </Piece>\n";
printf VTKELEVBOUNDARY "   </PolyData>\n";
printf VTKELEVBOUNDARY "</VTKFile>\n";
close(VTKELEVBOUNDARY);

#-----------------------------------------------------------------------
#                         F O R T  1 9 
#                        X D M F   X M L 
#-----------------------------------------------------------------------
# if a fort.19 supplied, write the time varying positions of the elevation
# specified boundary nodes in XDMF xml format
# write data from adcirc file(s)
my $haveFort19 = "null";
foreach my $file (@adcircfiles) {
   if ($file eq "fort.19" ) {
      stderrMessage("INFO","Writing time varying aperiodic elevation boundary position.");
      $haveFort19 = $file;
      #TODO: remove this file from the list so the script can continue
      # after writing the time varying elev boundary pts
   }
}
if ($haveFort19 ne "null") {   
   my $fort19BoundaryFileName = $meshfile . "_timeVaryingElevBoundaries.xmf";
   unless (open(FORT19BOUNDARY,">$fort19BoundaryFileName")) {
      stderrMessage("ERROR","Failed to open $fort19BoundaryFileName for writing: $!.");
      die;
   }
   printf FORT19BOUNDARY "<?xml version=\"1.0\"?>\n";
   printf FORT19BOUNDARY "<!DOCTYPE Xdmf SYSTEM \"Xdmf.dtd\" []>\n";
   printf FORT19BOUNDARY "<Xdmf Version=\"2.0\">\n";
   printf FORT19BOUNDARY "   <Domain Name=\"$agrid\">\n";
   printf FORT19BOUNDARY "      <Grid Name=\"TimeSeries\" GridType=\"Collection\" CollectionType=\"Temporal\">\n";
   # open and start reading fort.19 file
   unless (open(FORT19DATA,"<fort.19")) {
      stderrMessage("ERROR","Failed to open $fort19BoundaryFileName for writing: $!.");
      die;
   }
   my $timeinc19 = <FORT19DATA>; # time step for fort.19 data in seconds
   my $timesec = 0.0;
   my @eta19;    
   while(<FORT19DATA>) {
      # read one dataset from fort.19
      $eta19[0] = $_;
      for (my $i=1; $i<$neta; $i++ ) {
         $eta19[$i] = <FORT19DATA>;
      }
      chomp(@eta19);
      # write one dataset to XDMF xml file 
      printf FORT19BOUNDARY "         <Grid Name=\"Time=$timesec\" GridType=\"Uniform\">\n";
      printf FORT19BOUNDARY "            <Time Value=\"$timesec\"/>\n";
      printf FORT19BOUNDARY "            <Topology TopologyType=\"POLYVERTEX\" NumberOfElements=\"$neta\" NodesPerElement=\"1\"/>\n";
      printf FORT19BOUNDARY "            <Geometry GeometryType=\"XYZ\">\n";
      printf FORT19BOUNDARY "               <DataItem ItemType=\"Uniform\" Dimensions=\"$neta 3\" Format=\"XML\">\n"; 
      for (my $i=0; $i<$neta; $i++ ) {
         printf FORT19BOUNDARY "                  $elevBoundaryLons[$i] $elevBoundaryLats[$i] $eta19[$i]\n";
      }
      printf FORT19BOUNDARY "               </DataItem>\n";
      printf FORT19BOUNDARY "            </Geometry>\n";
      printf FORT19BOUNDARY "         </Grid>\n";
      $timesec = $timesec + $timeinc19;
   }
   close(FORT19DATA);
   printf FORT19BOUNDARY "      </Grid>\n";
   printf FORT19BOUNDARY "   </Domain>\n";   
   printf FORT19BOUNDARY "</Xdmf>\n";
   close(FORT19BOUNDARY);
   exit;
}
# 
# Now read the flux-specified boundary tables and write out as vtkPoints
my $vtkFluxBoundaryFileName = $meshfile . "_fluxBoundaries.vtp";
unless (open(VTKFLUXBOUNDARY,">$vtkFluxBoundaryFileName")) {
   stderrMessage("ERROR","Failed to open $vtkFluxBoundaryFileName for writing: $!.");
   die;
}
$line = <MESH>;
@fields = split(' ',$line);
my $nbou = $fields[0];
$line = <MESH>;
@fields = split(' ',$line);
my $nvel = $fields[0];
# write header for boundaries file   
printf VTKFLUXBOUNDARY "<?xml version=\"1.0\"?>\n";
printf VTKFLUXBOUNDARY "<VTKFile type=\"PolyData\" version=\"0.1\" byte_order=\"LittleEndian\">\n";
printf VTKFLUXBOUNDARY "   <PolyData>\n";
printf VTKFLUXBOUNDARY "      <Piece NumberOfPoints=\"$nvel\">\n";
printf VTKFLUXBOUNDARY "         <Points>\n";
printf VTKFLUXBOUNDARY "            <DataArray type=\"Float64\" NumberOfComponents=\"3\" format=\"ascii\">\n";
my @fluxBoundaryTypes;
my @fluxBoundaryElevs; 
my $fluxBoundaryCount = 0;
for (my $i=0; $i<$nbou; $i++) {
   $line = <MESH>;
   my @fields = split(' ',$line); 
   my $nvell = $fields[0];
   my $ibtype = $fields[1];
   for (my $j=0; $j<$nvell; $j++) {
      $line = <MESH>;
      my @nvellFields = split(' ',$line);
      my $nbvv = $nvellFields[0];
      printf VTKFLUXBOUNDARY "$x[$nbvv-1] $y[$nbvv-1] 0.0 ";
      $fluxBoundaryTypes[$fluxBoundaryCount] = $ibtype;
      $fluxBoundaryElevs[$fluxBoundaryCount] = $z[$nbvv-1];      
      $fluxBoundaryCount++;
   }
}        
#print "fluxBoundaryCount is $fluxBoundaryCount\n"; # @jasonfleming debug
printf VTKFLUXBOUNDARY "\n";
printf VTKFLUXBOUNDARY "            </DataArray>\n";
printf VTKFLUXBOUNDARY "         </Points>\n";
printf VTKFLUXBOUNDARY "         <PointData>\n";
printf VTKFLUXBOUNDARY "            <DataArray Name=\"IBTYPE\" type=\"Int32\" NumberOfComponents=\"1\" format=\"ascii\">";
for (my $i=0; $i<$fluxBoundaryCount; $i++) {
   printf VTKFLUXBOUNDARY " $fluxBoundaryTypes[$i]";
}
printf VTKFLUXBOUNDARY "            </DataArray>\n";
printf VTKFLUXBOUNDARY "            <DataArray Name=\"Elevation\" type=\"Float64\" NumberOfComponents=\"1\" format=\"ascii\">";
for (my $i=0; $i<$fluxBoundaryCount; $i++) {
   printf VTKFLUXBOUNDARY " $fluxBoundaryElevs[$i]";
}
printf VTKFLUXBOUNDARY "            </DataArray>\n";
printf VTKFLUXBOUNDARY "         </PointData>\n";
printf VTKFLUXBOUNDARY "      </Piece>\n";
printf VTKFLUXBOUNDARY "   </PolyData>\n";
printf VTKFLUXBOUNDARY "</VTKFile>\n";
close(VTKFLUXBOUNDARY);
#
close(MESH);
#
# write data from adcirc file(s)
foreach my $file (@adcircfiles) {
   stderrMessage("INFO","Processing $file.");
   my $num_components = 0; # 1 if scalar, 2 if 2D vector, 3 if 3D vector
   my $num_datasets = 0;   # 0 if unknown
   my $scalars_name = "";
   # set some parameters based on the type of file we are working with
   if ( $file eq "none" || $file eq "fort.14" ) {
      my $outfile = $meshfile . ".vtu";
      # start writing vtk-formatted file
      unless (open(OUT,">$outfile")) {
         stderrMessage("ERROR","Failed to open vtk file $outfile for writing: $!.");
         die;
      }
      &writeHeader($ne, $np);
      printf OUT "         <PointData Scalars=\"BathymetricDepth\">\n";
      &writeMesh($ne, $np);
      &writeFooter();
      close(OUT);
      next;
   }
   $datacentered = "PointData";
   my $datatype = "Float64";
   if ( $file eq "positives.100"  || $file eq "negatives.100" || $file eq "absolutes.100" || $file eq "subdomains.100" || $file eq "fdrepeats.100" ) {
      $num_components = 1;
      $num_datasets = 1;
      $datacentered = "CellData";
      $datatype = "Int32";
   }
   if ( $file eq "maxele.63" || $file eq "maxwvel.63" || $file eq "minpr.63" || $file eq "ESLNodes.63" ) {
      $num_components = 1;
      $num_datasets = 1;
      $datatype = "Float64";
   }
   if ( $file eq "residents.63" || $file eq "ghosts.63" || $file eq "absolutes.63" || $file eq "subdomains.63" || $file eq "psubdomains.63" || $file eq "ghostmem.63" ) {
      $num_components = 1;
      $num_datasets = 1;
      $datatype = "Int32";
   }
   if ( $file eq "noff.100" || $file eq "noffornot.100" ) {
      $num_components = 1;
      $num_datasets = 0;
      $datatype = "Int32";
      $datacentered = "CellData"; 
   }
   if ( $file eq "nodecode.63" ) {
      $num_components = 1;
      $num_datasets = 0;
      $datatype = "Int32";
   }  
   if ( $file eq "fort.63" || $file eq "fort.73" ) {
      $num_components = 1;
      $num_datasets = 0;
   }
   if ( $file eq "fort.74" || $file eq "fort.64" ) {
      $num_components = 2;
      $num_datasets = 0;
   }
   if ( $file eq "gradient.txt" ) {
      $num_components = 1;
      $num_datasets = 0;
      $datacentered = "CellData"; 
   }
   if ( $file eq "maxgradient.txt" ) {
      $num_components = 1;
      $num_datasets = 1;
      $datacentered = "CellData"; 
   }
   # make sure we can actually open the adcirc file before going further
   unless (open(ADCIRCFILE,"<$file")) {
      stderrMessage("ERROR",
          "Failed to open ADCIRC file $file for reading: $!.");
         next;
   }     
   # 
   # for nodal attributes, we read the file entirely differently from an
   # output file
   if ( $file eq "fort.13" ) {
      my $outfile = $file . ".vtu";
      unless (open(OUT,">$outfile")) {
         stderrMessage("ERROR","Failed to open vtk file $outfile for writing: $!.");
         die;
      }
      &writeHeader($ne, $np);
      printf OUT "         <PointData Scalars=\"NodalAttributes\">\n"; 
      # 
      # read nodal attributes file header
      $line = <ADCIRCFILE>; # read comment line (not used)
      $line = <ADCIRCFILE>; # number of nodes (not used)
      $nattr = <ADCIRCFILE>; # number of nodal attributes
      for (my $i=0; $i<$nattr; $i++ ) {
         $attrName = <ADCIRCFILE>;
         chomp($attrName);
         $attrName =~ s/\s+//;
         $namesUnits{$attrName} = <ADCIRCFILE>;
         $namesNumValues{$attrName} = <ADCIRCFILE>;
         $line = <ADCIRCFILE>;
         chomp($line);      
         $line =~ s/\s+//;
         $namesDefaultValues{$attrName} = $line;        
      }
      #
      # now read body of nodal attributes file
      for (my $i=0; $i<$nattr; $i++ ) {
         $attrName = <ADCIRCFILE>; # name of the nodal attribute
         chomp($attrName);
         $attrName =~ s/\s+//;
         $namesNumNonDefaults{$attrName} = 0; # number of non default values for this attribute
         $line = <ADCIRCFILE>; 
         chomp($line);
         $namesNumNonDefaults{$attrName} = $line; # number of non default values for this attribute
         # set all values to the default 
         for (my $n=0; $n<$np; $n++ ) {
            $attrValues[$n] = $namesDefaultValues{$attrName};
         }
         for (my $n=0; $n<$namesNumNonDefaults{$attrName}; $n++) {
            $line = <ADCIRCFILE>;
            if ( defined $line ) {
               chomp($line);
               @fields = split(' ',$line);
            } else {
               stderrMessage("ERROR","Ran out of data: $!.");
               die;
            }
            $attrValues[$fields[0]-1] = $fields[1];
         }
         $scalars_name = "Scalars=\"$attrName\"";
         # write out dataset from ADCIRC file
         my $vtk_components = 1;
         printf OUT "            <DataArray Name=\"$attrName\" type=\"Float64\" NumberOfComponents=\"$vtk_components\" format=\"ascii\">\n";
         for (my $i=0; $i<$np; $i++) {
            printf OUT "$attrValues[$i]\n";
         }
         printf OUT "            </DataArray>\n";
      }
      &writeMesh($ne, $np);
      &writeFooter();
      close(OUT);
      next;
   }
   $line = <ADCIRCFILE>;  # read comment line (not used)
   print $line;
   $line = <ADCIRCFILE>;  # read header line (not used)
   print $line;
   if ( $num_datasets == 0 ) {
      # we don't know how many datasets are in this file, it is likely more
      # than one, so we need to start a small separate pvd file that lists
      # the data files in the collection
      my $outfile = $file . ".pvd";
      unless (open(PVD,">$outfile")) {
         stderrMessage("ERROR",
            "Failed to open vtk file $outfile for writing: $!.");
         die;
      }
      printf PVD "<?xml version=\"1.0\"?>\n";
      printf PVD "<VTKFile type=\"Collection\" version=\"0.1\" byte_order=\"LittleEndian\">\n";
      printf PVD "   <Collection>\n";
   }
   my $dataset = 0;
   my @comp; # components of the dataset
   while (<ADCIRCFILE>) {
      @fields = split(' ',$_);
      $time[$dataset] = $fields[0];
      $timestep[$dataset] = $fields[1];
      #stderrMessage("DEBUG","time is $time[$dataset], timestep is $timestep[$dataset]");
      my @mag; # for holding vector magnitudes
      my $io_success = "true";
      my $lim=$np; # nodal values are the default
      if ( $datacentered eq "CellData" ) {
         $lim=$ne;
      }
      #
      # read adcirc data file
      for (my $i=0; $i<$lim; $i++) {
         $line = <ADCIRCFILE>;
         if ( defined $line ) {
            @fields = split(' ',$line);
         } else {
            stderrMessage("ERROR","Ran out of data: $!.");
            die;
         }

         # get rid of the node number or node ID
         shift(@fields);
         if ( $num_components == 2 ) {
            # calculate vector magnitude
            $mag[$i] = sqrt($fields[0]**2 + $fields[1]**2);
            push(@fields,"0.0"); # vtk expects all vectors to be 3D
         }
         $comp[$i] = join(' ',@fields);
      }
      #
      # create data set characteristics
      my $outfile = $file;
      my $dataset_ext = "";
      if ( $num_datasets == 0 ) {
         $dataset_ext = sprintf("%03d",$dataset+1);
         $outfile = $outfile . "_" . $dataset_ext;
      }
      $scalars_name = "Scalars=\"$adcirctypes{$file}\"";
      my $vectors_name = "";
      if ( $num_components > 1 ) {
         $scalars_name = "Scalars=\"$adcirctypes{$file}Magnitude\"";
         $vectors_name = "Vectors=\"$adcirctypes{$file}\"";
      }
      $outfile .= ".vtu";
      # start writing vtk-formatted file
      unless (open(OUT,">$outfile")) {
         stderrMessage("ERROR","Failed to open vtk file $outfile for writing: $!.");
         die;
      }
      if ( $num_datasets == 0 ) {
         printf PVD "         <DataSet timestep=\"$time[$dataset]\" group=\"\" part=\"0\" file=\"$outfile\"/>\n";
      }
      &writeHeader($ne, $np);
      printf OUT "         <$datacentered $scalars_name $vectors_name>\n"; 
      # write out dataset from ADCIRC file
      my $vtk_components = $num_components;
      if ( $num_components == 2 ) {
         $vtk_components = $num_components + 1; # for vtk all vectors are 3D
      }
      printf OUT "            <DataArray Name=\"$adcirctypes{$file}\" type=\"$datatype\" NumberOfComponents=\"$vtk_components\" format=\"ascii\">\n";
      for (my $i=0; $i<$lim; $i++) {
         printf OUT "$comp[$i]\n";
      }
      printf OUT "            </DataArray>\n";
      # write vector magnitude if this is a vector dataset
      if ( $num_components > 1 ) {
         printf OUT "            <DataArray Name=\"$adcirctypes{$file}Magnitude\" type=\"$datatype\" NumberOfComponents=\"1\" format=\"ascii\">\n";
         for (my $i=0; $i<$lim; $i++) {
            printf OUT "$mag[$i]\n";
         }
         printf OUT "            </DataArray>\n";
      }
      if ($datacentered eq "CellData") {
         printf OUT "         </CellData>\n";
         printf OUT "         <PointData>\n";
      }
      &writeMesh($ne, $np);    # write out bathymetric depth as a dataset
      &writeFooter();
      close(OUT);
      $dataset++;
   }
   if ( $num_datasets == 0 ) {
      printf PVD "   </Collection>\n";
      printf PVD "</VTKFile>\n";
      close(PVD);
   }
   close(ADCIRCFILE);
}

sub writeHeader () {
   my $ne = shift;
   my $np = shift;
   printf OUT "<?xml version=\"1.0\"?>\n";
   printf OUT "<VTKFile type=\"UnstructuredGrid\" version=\"0.1\" byte_order=\"LittleEndian\">\n";
   printf OUT "   <UnstructuredGrid>\n";
   printf OUT "      <Piece NumberOfPoints=\"$np\" NumberOfCells=\"$ne\">\n";
}

sub writeFooter () {
   printf OUT "      </Piece>\n";
   printf OUT "   </UnstructuredGrid>\n";
   printf OUT "</VTKFile>\n";
}

sub writeMesh () {
   my $ne = shift;
   my $np = shift;
   #
   # write node IDs if specified
   if ( defined $getNodeIndices ) {
      printf OUT "         <DataArray Name=\"NodeArrayIndices\" type=\"Int32\" NumberOfComponents=\"1\" format=\"ascii\">\n";
      for (my $i=0; $i<$np; $i++) {
         printf OUT "$nodeIndices[$i]\n";
      }
      printf OUT "            </DataArray>\n";
   }
   #
   # write the BathymetricDepth
   printf OUT "            <DataArray Name=\"BathymetricDepth\" type=\"Float64\" NumberOfComponents=\"1\" format=\"ascii\">\n";
   for (my $i=0; $i<$np; $i++) {
      printf OUT "$z[$i]\n";
   }
   printf OUT "            </DataArray>\n";
   printf OUT "         </PointData>\n";
   printf OUT "         <Points>\n";
   printf OUT "            <DataArray type=\"Float64\" NumberOfComponents=\"3\" format=\"ascii\">\n";
   for (my $i=0; $i<$np; $i++) {
      printf OUT "$x[$i] $y[$i] 0.0\n";
   }
   printf OUT "            </DataArray>\n";
   printf OUT "         </Points>\n";
   #
   # write element IDs if specified
   if ( defined $getElementIndices ) {
      printf OUT "         <CellData Scalars=\"ElementArrayIndices\">\n"; 
      printf OUT "         <DataArray Name=\"ElementArrayIndices\" type=\"Int32\" NumberOfComponents=\"1\" format=\"ascii\">\n";
      for (my $i=0; $i<$ne; $i++) {
         printf OUT "$elementIndices[$i]\n";
      }
      printf OUT "            </DataArray>\n";
      printf OUT "         </CellData>\n";
   }
   # write element connectivity indices   
   printf OUT "         <Cells>\n";
   printf OUT "            <DataArray type=\"Int32\" Name=\"connectivity\" format=\"ascii\">\n";
   for (my $i=0; $i<$ne; $i++) {
      printf OUT "$conn[$i]\n";
   }
   printf OUT "            </DataArray>\n";
   printf OUT "            <DataArray type=\"Int32\" Name=\"offsets\" format=\"ascii\">\n";
   for (my $i=0; $i<$ne; $i++) {
      my $offset = $i*3 + 3;
      printf OUT "$offset\n";
   }
   printf OUT "            </DataArray>\n";
   printf OUT "            <DataArray type=\"UInt8\" Name=\"types\" format=\"ascii\">\n";
   for (my $i=0; $i<$ne; $i++) {
      printf OUT "5\n";   # triangles
   }
   printf OUT "            </DataArray>\n";
   printf OUT "         </Cells>\n";
}

sub stderrMessage () {
   my $level = shift;
   my $message = shift;
   my @months = qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);
   (my $second, my $minute, my $hour, my $dayOfMonth, my $month, my $yearOffset, my $dayOfWeek, my $dayOfYear, my $daylightSavings) = localtime();
   my $year = 1900 + $yearOffset;
   my $hms = sprintf("%02d:%02d:%02d",$hour, $minute, $second);
   my $theTime = "[$year-$months[$month]-$dayOfMonth-T$hms]";
   printf STDERR "$theTime $level: adc2vtk.pl: $message\n";
}
