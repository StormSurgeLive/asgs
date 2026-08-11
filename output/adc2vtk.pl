#!/usr/bin/env perl
#--------------------------------------------------------------------------
#
# adc2vtk.pl
#
# This script reformats ADCIRC input or output files to vtk xml format for
# visualization and/or analysis.
#--------------------------------------------------------------------------
# Copyright(C) 2010--2026 Jason Fleming
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
use ASGSUtil;
use XML::Writer;
# XML::Writer instances are created per XML output file.
my $outWriter;
my $pvdWriter;

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
                   "particles_count_peak.200","ElementPeakParticleCount",
                   "particles_perarea_peak.200","ElementPeakParticleCountPerArea",
                   "particles_pervolume_peak.200","ElementPeakParticleCountPerVolume",
                   "nodecode.63","NodeWetDryState",
                   "ESLNodes.63","ElementalSlopeLimitAtNodesDiagnostic",
                   "residents.63","ResidentNodeNumbers",
                   "ghosts.63","GhostNodeNumbers",
                   "ghostmem.63","GhostNodeSubdomainMembership",
                   "absolutes.63","AbsoluteNodeNumbers",
                   "subdomains.63","SubdomainsFromFort18",
                   "psubdomains.63","SubdomainsFromPartmesh",
                   "minedgelengths.63","MinimumEdgeLengthAtNode",
                   "edgelengthgradients.63","EdgeLengthGradientAtNode",
                   "elementareas.100","ElementArea",
                   "maxtimestepsizes.63","MaxTimeStepSizes"
                   );
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
my $fluxBoundaries2dm; # defined if flux boundaries should be written as mesh geometry in *.2dm files
my $arcMapCSV; # write mesh and flux boundary node pairs to ArcMap CSV files
my $translate; # 1 to translate the mesh x-y coordinates to the center of the cpp projection
my $centerx; # xcoord (deg) at center of the 2D mesh; conv to (m) if --cpp
my $centery; # ycoord (deg) at center of the 2D mesh; conv to (m) if --cpp
my $scale = 1.0; # number to muliply the x-y coordinates if they should be scaled
my $comment; # contains the command used to generate the file as a comment at the top of the file

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
my @trackfiles;     # storm track files (fort.22),comma separated with no spaces
my $excludeNonLeveeFluxBoundaries; # if only levee geometry should be generated
my $test;    # true if this is being executed as a unit test
#
GetOptions(
           "jitter" => \$jitter,
           "meshfile=s" => \$meshfile,
           "cpp" => \$cpp,
           "fluxBoundaries2dm" => \$fluxBoundaries2dm,
           "arcMapCSV" => \$arcMapCSV,
           "slam0=s" => \$slam0,
           "sfea0=s" => \$sfea0,
           "translate" => \$translate,
           "scale=s" => \$scale,
           "getNodeIndices" => \$getNodeIndices,
           "getElementIndices" => \$getElementIndices,
           "excludeNonLeveeFluxBoundaries" => \$excludeNonLeveeFluxBoundaries,
           "trackfiles=s" => \@trackfiles,
           "adcircfiles=s" => \@adcircfiles,
           "test" => \$test
         );
#
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
      ASGSUtil::stderrMessage("ERROR","Failed to open vtk file $outfile for writing: $!.",$test);
      die;
   }
   # write header for VTP (track line file)
   $outWriter = newXMLWriter(\*OUT);
   $outWriter->xmlDecl();
   $outWriter->startTag("VTKFile",
      type => "PolyData", version => "0.1", byte_order => "LittleEndian");
   $outWriter->startTag("PolyData");

   foreach my $file (@trackfiles) {
      ASGSUtil::stderrMessage("INFO","Processing $file.",$test);
      # make sure we can actually open the adcirc file before going further
      unless (open(ADCIRCFILE,"<$file")) {
         ASGSUtil::stderrMessage("ERROR",
          "Failed to open ADCIRC file $file for reading: $!.",$test);
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
         $fields[6] =~/(\d+)(N|S)/;   # tenths of degrees plus orientation, e.g., "217N"
         $y[$cycle] =  $1/10.0;       # convert from tenths of degrees to degrees
         my $yhemisphere = $2;
         if ( $yhemisphere eq "S" ) {
            $y[$cycle] *= -1.0;
         }
         $fields[7] =~/(\d+)(E|W)/;   # tenths of degrees plus orientation, e.g., "767W"
         $x[$cycle] =  $1/10.0;       # convert from tenths of degrees to degrees
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

      $outWriter->comment("from track file $file");
      $outWriter->startTag("Piece",
         NumberOfPoints => $numTrackPoints,
         NumberOfLines => $numLineSegments);
      # vmax values at each track point
      $outWriter->startTag("PointData", Scalars => "vmax");
      $outWriter->startTag("DataArray", type => "Float64", Name => "vmax", format => "ascii");
      $outWriter->characters(join(" ", @vmax[0..$numTrackPoints-1]));
      $outWriter->endTag("DataArray");
      $outWriter->endTag("PointData");
      # track point locations
      $outWriter->startTag("Points");
      $outWriter->startTag("DataArray",
         NumberOfComponents => "3", type => "Float64", Name => "PointLocations", format => "ascii");
      $outWriter->characters(join("  ", map { "$x[$_] $y[$_] $z" } 0..$numTrackPoints-1));
      $outWriter->endTag("DataArray");
      $outWriter->endTag("Points");
      # line connectivity/topology
      $outWriter->startTag("Lines");
      $outWriter->startTag("DataArray", type => "Int32", Name => "connectivity", format => "ascii");
      $outWriter->characters(join("  ", map { "$_ " . ($_+1) } 0..$numTrackPoints-1));
      $outWriter->endTag("DataArray");
      $outWriter->startTag("DataArray", type => "Int32", Name => "offsets", format => "ascii");
      $outWriter->characters(join(" ", map { 2*($_+1) } 0..$numTrackPoints-1));
      $outWriter->endTag("DataArray");
      $outWriter->endTag("Lines");
      $outWriter->endTag("Piece");
   }
   # write VTP track(s) file footer
   $outWriter->endTag("PolyData");
   $outWriter->endTag("VTKFile");
   $outWriter->end();
   close(OUT);
}

unless ( @adcircfiles ) {
   $adcircfiles[0] = "none";
} else {
   @adcircfiles = split(/,/,join(',',@adcircfiles)); # get a list w/o commas
}
#
if ( $meshfile eq "null" ) {
   ASGSUtil::stderrMessage("INFO","Mesh file name was not provided or the file was not found. adc2vtk.pl is finished.",$test);
   exit;
}
unless (open(MESH,"<$meshfile")) {
   ASGSUtil::stderrMessage("ERROR","Failed to open mesh file $meshfile for reading: $!.", $test);
   die;
}
# read number of nodes and number of elements from adcirc mesh file
ASGSUtil::stderrMessage("INFO","Reading mesh file '$meshfile'.",$test);
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
   $centerx = 0.0;
   $centery = $sfea0*$deg2rad*$R;
} else {
   $centerx = $slam0;
   $centery = $sfea0;
}
# "--translate" : move the coordinates to the center of the projection
if ( defined $translate ) {
   for (my $i=0; $i<$np; $i++) {
      $x[$i] = $x[$i] - $centerx;
      $y[$i] = $y[$i] - $centery;
   }
}
# "--scale 0.001" : scale the x-y coordinates
for (my $i=0; $i<$np; $i++) {
   $x[$i] = $x[$i]*$scale;
   $y[$i] = $y[$i]*$scale;
}

if ( defined $arcMapCSV ) {
   # write out the flux-specified node tables as XYZ CSV that can be read into ArcMap
   my $arcMapCSVMeshFileName = $meshfile . "_meshNodes.csv";
   my $ARCMAPMESHNODES;
   if (not open($ARCMAPMESHNODES,">","$arcMapCSVMeshFileName")) {
      ASGSUtil::stderrMessage("ERROR","Failed to open $arcMapCSVMeshFileName for writing: $!.",$test);
      die;
   }
   # node number , x , y, internal barrier height (m), bathymetric depth (m)
   for (my $i=0; $i<$np; $i++) {
      my $nodeNumber = $i + 1;
      printf $ARCMAPMESHNODES "$nodeNumber,$x[$i],$y[$i],-99999,$z[$i]\n";
   }
   close($ARCMAPMESHNODES);
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
   ASGSUtil::stderrMessage("ERROR","Failed to open $vtkElevationBoundaryFileName for writing: $!.",$test);
   die;
}
$line = <MESH>;
@fields = split(' ',$line);
my $nope = $fields[0];
$line = <MESH>;
@fields = split(' ',$line);
my $neta = $fields[0];
# write header for boundaries file
my $elevWriter = newXMLWriter(\*VTKELEVBOUNDARY);
$elevWriter->xmlDecl();
$elevWriter->startTag("VTKFile",
   type => "PolyData", version => "0.1", byte_order => "LittleEndian");
$elevWriter->startTag("PolyData");
$elevWriter->startTag("Piece", NumberOfPoints => $neta);
$elevWriter->startTag("Points");
$elevWriter->startTag("DataArray",
   type => "Float64", NumberOfComponents => "3", format => "ascii");

# read all boundary data into 1D arrays
my @elevBoundaryTypes; # ibtypee
my @elevBoundaryElevs; # bathytopo elevation
my @elevBoundaryLons;  # longitude (degrees E)
my @elevBoundaryLats;  # latitude (degrees N)
my @elevBoundaryNodes; # node number 1-indexed
my @elevBoundaryExternalBoundaryIndices; # 1-based index into the total external boundary array
my @elevBoundaryLocalBoundaryIndices;    # 1-based index into the total external boundary array
my $elevBoundaryCount = 0;
my @elevBoundaryPoints;
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
      $elevBoundaryExternalBoundaryIndices[$elevBoundaryCount] = $elevBoundaryCount+1;
      $elevBoundaryLocalBoundaryIndices[$elevBoundaryCount] = $elevBoundaryCount+1;
      push @elevBoundaryPoints, "$x[$nbdv-1] $y[$nbdv-1] 0.0";
      $elevBoundaryCount++;
   }
}
$elevWriter->characters(join("\n", @elevBoundaryPoints));
$elevWriter->endTag("DataArray");
$elevWriter->endTag("Points");
$elevWriter->startTag("PointData");

my @elevArrays = (
   ["IBTYPEE", "Int32", \@elevBoundaryTypes],      # elevation boundary type
   ["Elevation", "Float64", \@elevBoundaryElevs],  # bathy/topo elevation (positive downward)
   ["Longitude", "Float64", \@elevBoundaryLons],   # longitudes (degrees west)
   ["Latitude", "Float64", \@elevBoundaryLats],    # latitudes (degrees north)
   ["NodeNumber", "Int32", \@elevBoundaryNodes],   # node numbers
   ["ExternalBoundaryIndex", "Int32", \@elevBoundaryExternalBoundaryIndices], # external boundary indices
   ["LocalBoundaryIndex", "Int32", \@elevBoundaryLocalBoundaryIndices]        # local boundary indices
);
foreach my $array (@elevArrays) {
   my ($name, $type, $vals) = @$array;
   $elevWriter->startTag("DataArray",
      Name => $name,
      type => $type,
      NumberOfComponents => "1",
      format => "ascii");
   $elevWriter->characters(join(" ", @$vals[0..$elevBoundaryCount-1]));
   $elevWriter->endTag("DataArray");
}
$elevWriter->endTag("PointData");
$elevWriter->endTag("Piece");
$elevWriter->endTag("PolyData");
$elevWriter->endTag("VTKFile");
$elevWriter->end();
close(VTKELEVBOUNDARY);
#
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
      ASGSUtil::stderrMessage("INFO","Writing time varying aperiodic elevation boundary position.",$test);
      $haveFort19 = $file;
      #TODO: remove this file from the list so the script can continue
      # after writing the time varying elev boundary pts
   }
}
if ($haveFort19 ne "null") {
   my $fort19BoundaryFileName = $meshfile . "_timeVaryingElevBoundaries.xmf";
   unless (open(FORT19BOUNDARY,">$fort19BoundaryFileName")) {
      ASGSUtil::stderrMessage("ERROR","Failed to open $fort19BoundaryFileName for writing: $!.",$test);
      die;
   }
   my $fort19Writer = newXMLWriter(\*FORT19BOUNDARY);
   $fort19Writer->xmlDecl();
   $fort19Writer->doctype("Xdmf", undef, "Xdmf.dtd");
   chomp($agrid);
   $fort19Writer->startTag("Xdmf", Version => "2.0");
   $fort19Writer->startTag("Domain", Name => $agrid);
   $fort19Writer->startTag("Grid",
      Name => "TimeSeries",
      GridType => "Collection",
      CollectionType => "Temporal");
   # open and start reading fort.19 file
   unless (open(FORT19DATA,"<fort.19")) {
      ASGSUtil::stderrMessage("ERROR","Failed to open $fort19BoundaryFileName for writing: $!.",$test);
      die;
   }
   my $timeinc19 = <FORT19DATA>;  # time step for fort.19 data in seconds
   my $timesec = 0.0;
   my @eta19;
   while(<FORT19DATA>) {
      $eta19[0] = $_;
      for (my $i=1; $i<$neta; $i++ ) {
         $eta19[$i] = <FORT19DATA>;
      }
      chomp(@eta19);
      # read one dataset from fort.19

      $fort19Writer->startTag("Grid", Name => "Time=$timesec", GridType => "Uniform");
      $fort19Writer->emptyTag("Time", Value => $timesec);
      $fort19Writer->emptyTag("Topology",
         TopologyType => "POLYVERTEX",
         NumberOfElements => $neta,
         NodesPerElement => "1");
      $fort19Writer->startTag("Geometry", GeometryType => "XYZ");
      $fort19Writer->startTag("DataItem",
         ItemType => "Uniform",
         Dimensions => "$neta 3",
         Format => "XML");
      $fort19Writer->characters(join("\n", map {
         "$elevBoundaryLons[$_] $elevBoundaryLats[$_] $eta19[$_]"
      } 0..$neta-1));
      $fort19Writer->endTag("DataItem");
      $fort19Writer->endTag("Geometry");
      $fort19Writer->endTag("Grid");
      $timesec = $timesec + $timeinc19;
   }
   close(FORT19DATA);
   $fort19Writer->endTag("Grid");
   $fort19Writer->endTag("Domain");
   $fort19Writer->endTag("Xdmf");
   $fort19Writer->end();
   close(FORT19BOUNDARY);
   exit;
}
#
#-----------------------------------------------------------------------
#                 F L U X   B O U N D A R I E S
#               V T K   A N D    X D M F   X M L
#-----------------------------------------------------------------------
#
# echo the flux boundary file data for troubleshooting
my $vtkEchoFluxBoundaryFileName = $meshfile . "_echoFluxBoundaries.txt";
unless (open(VTKECHOFLUXBOUNDARY,">$vtkEchoFluxBoundaryFileName")) {
   ASGSUtil::stderrMessage("ERROR","Failed to open $vtkEchoFluxBoundaryFileName for writing: $!.",$test);
   die;
}
#
# write out the flux-specified boundary tables as vtkPoints to
# show boundary node location and height (if applicable)
my $vtkFluxBoundaryFileName = $meshfile . "_fluxBoundaries.vtp";
unless (open(VTKFLUXBOUNDARY,">$vtkFluxBoundaryFileName")) {
   ASGSUtil::stderrMessage("ERROR","Failed to open $vtkFluxBoundaryFileName for writing: $!.",$test);
   die;
}
# write header for boundary points file
my $vtkFluxWriter = newXMLWriter(\*VTKFLUXBOUNDARY);
$vtkFluxWriter->xmlDecl();
$vtkFluxWriter->startTag("VTKFile",
   type => "PolyData", version => "0.1", byte_order => "LittleEndian");
$vtkFluxWriter->startTag("PolyData");

# write out the flux-specified boundary tables as XDMF 3DSMESH geometry
# to show boundary height
my $xdmfFluxBoundaryGeometryFileName = $meshfile . "_fluxBoundaryGeometry.xmf";
unless (open(XDMFFLUXBOUNDARY,">$xdmfFluxBoundaryGeometryFileName")) {
   ASGSUtil::stderrMessage("ERROR","Failed to open $xdmfFluxBoundaryGeometryFileName for writing: $!.",$test);
   die;
}
my $xdmfFluxWriter = newXMLWriter(\*XDMFFLUXBOUNDARY);
$xdmfFluxWriter->xmlDecl();
$xdmfFluxWriter->doctype("Xdmf", undef, "Xdmf.dtd");
chomp($agrid);
$xdmfFluxWriter->startTag("Xdmf",
   "xmlns:xi" => "http://www.w3.org/2001/XInclude",
   Version => "3.0");
$xdmfFluxWriter->startTag("Domain", Name => $agrid);
$xdmfFluxWriter->startTag("Grid",
   CollectionType => "Spatial",
   GridType => "Collection",
   Name => "Levees");
#
# support for writing levee geometry as meshes in SMS 2dm format
my $twodmFluxBoundaryGeometryNodeFileName;
my $TWODMNODEFLUXBOUNDARY;
my $twodmFluxBoundaryGeometryElementFileName;
my $TWODMELEMENTFLUXBOUNDARY;
my $fullDomainElementID;
if ( defined $fluxBoundaries2dm ) {
   # write out the flux-specified node tables as 2DM mesh geometry
   $twodmFluxBoundaryGeometryNodeFileName = $meshfile . "_fluxBoundaryGeometry.nd";
   $TWODMNODEFLUXBOUNDARY;
   if (not open($TWODMNODEFLUXBOUNDARY,">","$twodmFluxBoundaryGeometryNodeFileName")) {
      ASGSUtil::stderrMessage("ERROR","Failed to open $twodmFluxBoundaryGeometryNodeFileName for writing: $!.",$test);
      die;
   }
   # write out the flux-specified element tables as 2DM mesh geometry
   $twodmFluxBoundaryGeometryElementFileName = $meshfile . "_fluxBoundaryGeometry.e4q";
   $TWODMELEMENTFLUXBOUNDARY;
   if (not open($TWODMELEMENTFLUXBOUNDARY,">","$twodmFluxBoundaryGeometryElementFileName")) {
      ASGSUtil::stderrMessage("ERROR","Failed to open $twodmFluxBoundaryGeometryElementFileName for writing: $!.",$test);
      die;
   }
   $fullDomainElementID = 1;
}
#
# write out the flux-specified boundary tables as ADCIRC fort.14 mesh geometry
# to show boundary height ; start with a node table file and an element
# table file, build them up separately, accumulating the number of nodes and
# the number of elements and then concatenate them into an adcirc mesh
# file with the number of elements and nodes at the top
my $adcFluxBoundaryGeometryNodeFileName = $meshfile . "_fluxBoundaryGeometry.nod";
my $ADCNODFLUXBOUNDARY;
if (not open($ADCNODFLUXBOUNDARY,">$adcFluxBoundaryGeometryNodeFileName")) {
   ASGSUtil::stderrMessage("ERROR","Failed to open '$adcFluxBoundaryGeometryNodeFileName' for writing: $!.",$test);
   die;
}
my $adcNodeID = 1;
my $adcFluxBoundaryGeometryElementFileName = $meshfile . "_fluxBoundaryGeometry.ele";
my $ADCELEFLUXBOUNDARY;
unless (open($ADCELEFLUXBOUNDARY,">$adcFluxBoundaryGeometryElementFileName")) {
   ASGSUtil::stderrMessage("ERROR","Failed to open '$adcFluxBoundaryGeometryElementFileName' for writing: $!.",$test);
   die;
}
my $adcElementID = 1;
#
# write out the flux-specified boundary tables as ArcMap CSV
my $arcMapCSVFluxBoundaryGeometryNodeFileName;
my $ARCMAPFLUXBOUNDARY;
if ( defined $arcMapCSV ) {
   # write out the flux-specified node tables as ArcMap CSV
   $arcMapCSVFluxBoundaryGeometryNodeFileName = $meshfile . "_fluxBoundaryGeometry.csv";
   if (not open($ARCMAPFLUXBOUNDARY,">","$arcMapCSVFluxBoundaryGeometryNodeFileName")) {
      ASGSUtil::stderrMessage("ERROR","Failed to open $arcMapCSVFluxBoundaryGeometryNodeFileName for writing: $!.",$test);
      die;
   }
}
#
# now start reading the boundary table from the mesh (fort.14) file
$line = <MESH>;
@fields = split(' ',$line);
my $nbou = $fields[0];
$line = <MESH>;
@fields = split(' ',$line);
my $nvel = $fields[0];
# echo the data
printf VTKECHOFLUXBOUNDARY "$nbou ! NBOU\n";
printf VTKECHOFLUXBOUNDARY "$nvel ! NVEL\n";
my @fluxBoundaryTypes; # ibtypee
my @fluxBoundaryElevs; # bathytopo elevation
my @fluxBoundaryLons;  # longitude (degrees E)
my @fluxBoundaryLats;  # latitude (degrees N)
my @fluxBoundaryNodes; # node number 1-indexed
my @fluxBoundaryFullDomainBoundaryIndices; # 1-based index into the total external boundary array
my @fluxBoundaryLocalBoundaryIndices; # 1-based index into the total external boundary array
my $fluxBoundaryCount = 0;
my $fluxBoundaryCountStart = 0;       # total count at the start of a new boundary
#
# loop over the total number of flux boundaries
for (my $i=0; $i<$nbou; $i++) {
   $line = <MESH>;
   my @fields = split(' ',$line);
   my $nvell = $fields[0];
   my $ibtype = $fields[1];
   my $numPoints = $nvell;
   my $numPointsPerBoundaryNode = 1;
   # levee boundaries have two points across the top
   if ( $ibtype == 4 || $ibtype == 14 || $ibtype == 24 || $ibtype == 5 || $ibtype == 15 || $ibtype == 25 ) {
      $numPointsPerBoundaryNode = 2;
   }  elsif ( defined $excludeNonLeveeFluxBoundaries ) {
      # this is a non levee flux boundary -- skip its nodes
      for (my $j=0; $j<$nvell; $j++) {
         $line = <MESH>;
         $fluxBoundaryCount++;
      }
      next;
   }

   $numPoints = $nvell * $numPointsPerBoundaryNode;
   my @fluxBoundaryNodeElevs;
   my @nbvv;
   my @ibconn;
   my @topZ;
   # read
   for (my $j=0; $j<$nvell; $j++) {
      $line = <MESH>;
      $fluxBoundaryCount++;
      if ( $j == 0 ) {
         $fluxBoundaryCountStart = $fluxBoundaryCount;
      }
      my @nvellFields = split(' ',$line);
      $nbvv[$j] = $nvellFields[0];   # node number is first value on the line
      $fluxBoundaryNodeElevs[$j] = "null";
      # no-flux (land and island) boundaries only have the node number
      if ( $ibtype == 0 || $ibtype == 10 || $ibtype == 20 || $ibtype == 1 || $ibtype == 11 || $ibtype == 21 ) {
         $fluxBoundaryNodeElevs[$j] = -1.0*$z[$nbvv[$j]-1];
      }
      # river boundaries only have the node number
      if ( $ibtype == 2 || $ibtype == 12 || $ibtype == 22 || $ibtype == 52 ) {
         $fluxBoundaryNodeElevs[$j] = -1.0*$z[$nbvv[$j]-1];
      }
      # external overflow boundaries have the node number and height
      if ( $ibtype == 3 || $ibtype == 13 || $ibtype == 23 ) {
         $fluxBoundaryNodeElevs[$j] = $nvellFields[1];
      }
      # levee boundaries have the node number, backside node, and height
      if ( $ibtype == 4 || $ibtype == 14 || $ibtype == 24 ) {
         $ibconn[$j] = $nvellFields[1];
         $fluxBoundaryNodeElevs[$j] = $nvellFields[2];
      }
      # levee boundaries with cross barrier pipes have the node number, backside node, and height
      if ( $ibtype == 5 || $ibtype == 15 || $ibtype == 25 ) {
         $ibconn[$j] = $nvellFields[1];
         $fluxBoundaryNodeElevs[$j] = $nvellFields[2];
      }
      $fluxBoundaryTypes[$fluxBoundaryCount] = $ibtype; # ibtype
      $fluxBoundaryLons[$fluxBoundaryCount] = $x[$nbvv[$j]-1];  # longitude (degrees E)
      $fluxBoundaryLats[$fluxBoundaryCount] = $y[$nbvv[$j]-1];  # latitude (degrees N)
      $fluxBoundaryElevs[$fluxBoundaryCount] = $z[$nbvv[$j]-1]; # bathytopo elevation
      $fluxBoundaryNodes[$fluxBoundaryCount] = $nbvv[$j];       # node number 1-indexed
      $fluxBoundaryFullDomainBoundaryIndices[$fluxBoundaryCount] = $fluxBoundaryCount; # 1-based index into the total external boundary array
      $fluxBoundaryLocalBoundaryIndices[$fluxBoundaryCount] = $j+1; # 1-based index into the total external boundary array
      if ( $fluxBoundaryNodeElevs[$j] eq "null" ) {
         ASGSUtil::stderrMessage("ERROR","The flux boundary type '$ibtype' was not recognized.",$test);
      }
   }
   #
   #  E C H O   F L U X   B O U N D A R Y
   #
   my $seg=$i+1;  # 1-indexed annotation
   printf VTKECHOFLUXBOUNDARY "$nvell $ibtype ! seg = $seg\n";
   for (my $j=0; $j<$nvell; $j++) {
      if ( $numPointsPerBoundaryNode == 1 ) {
         printf VTKECHOFLUXBOUNDARY "$nbvv[$j] ";
         if ( $numPointsPerBoundaryNode == 2 ) {
            printf VTKECHOFLUXBOUNDARY "$ibconn[$j] $fluxBoundaryNodeElevs[$j]";
         }
         printf VTKECHOFLUXBOUNDARY "\n";
      }
   }
   #
   #  F L U X   B O U N D A R Y   A S   V T K P O I N T S
   #
   # write the boundary point locations for this flux boundary
   $vtkFluxWriter->comment("seg = $seg");
   $vtkFluxWriter->startTag("Piece", NumberOfPoints => $numPoints);
   $vtkFluxWriter->startTag("Points");
   $vtkFluxWriter->startTag("DataArray",
      type => "Float64",
      NumberOfComponents => "3",
      format => "ascii");
   my @vtkFluxPoints;
   for (my $j=0; $j<$nvell; $j++) {
      push @vtkFluxPoints, "$x[$nbvv[$j]-1] $y[$nbvv[$j]-1] 0.0";
      if ( $numPointsPerBoundaryNode == 2 ) {
         push @vtkFluxPoints, "$x[$ibconn[$j]-1] $y[$ibconn[$j]-1] 0.0";
      }
   }
   $vtkFluxWriter->characters(join("\n", @vtkFluxPoints));
   # finish writing boundary points
   $vtkFluxWriter->endTag("DataArray");
   $vtkFluxWriter->endTag("Points");
   $vtkFluxWriter->startTag("PointData");

   # ibtype
   $vtkFluxWriter->startTag("DataArray",
      Name => "IBTYPE", type => "Int32", NumberOfComponents => "1", format => "ascii");
   $vtkFluxWriter->characters(join(" ", map { ($ibtype) x $numPointsPerBoundaryNode } 0..$nvell-1));
   $vtkFluxWriter->endTag("DataArray");

   # elevation
   $vtkFluxWriter->startTag("DataArray",
      Name => "Elevation", type => "Float64", NumberOfComponents => "1", format => "ascii");
   my @fluxElevVals;
   for (my $j=0; $j<$nvell; $j++) {
      push @fluxElevVals, ($fluxBoundaryNodeElevs[$j]) x $numPointsPerBoundaryNode;
   }
   $vtkFluxWriter->characters(join(" ", @fluxElevVals));
   $vtkFluxWriter->endTag("DataArray");

   # latitudes
   $vtkFluxWriter->startTag("DataArray",
      Name => "Latitude", type => "Float64", NumberOfComponents => "1", format => "ascii");
   my @fluxLatVals;
   for (my $j=0; $j<$nvell; $j++) {
      push @fluxLatVals, ($fluxBoundaryLats[$fluxBoundaryCountStart+$j]) x $numPointsPerBoundaryNode;
   }
   $vtkFluxWriter->characters(join(" ", @fluxLatVals));
   $vtkFluxWriter->endTag("DataArray");

   # longitudes
   $vtkFluxWriter->startTag("DataArray",
      Name => "Longitude", type => "Float64", NumberOfComponents => "1", format => "ascii");
   my @fluxLonVals;
   for (my $j=0; $j<$nvell; $j++) {
      push @fluxLonVals, ($fluxBoundaryLons[$fluxBoundaryCountStart+$j]) x $numPointsPerBoundaryNode;
   }
   $vtkFluxWriter->characters(join(" ", @fluxLonVals));
   $vtkFluxWriter->endTag("DataArray");

   # node number
   $vtkFluxWriter->startTag("DataArray",
      Name => "NodeNumber", type => "Int32", NumberOfComponents => "1", format => "ascii");
   my @fluxNodeVals;
   for (my $j=0; $j<$nvell; $j++) {
      push @fluxNodeVals, ($fluxBoundaryNodes[$fluxBoundaryCountStart+$j]) x $numPointsPerBoundaryNode;
   }
   $vtkFluxWriter->characters(join(" ", @fluxNodeVals));
   $vtkFluxWriter->endTag("DataArray");

   # full domain boundary index
   $vtkFluxWriter->startTag("DataArray",
      Name => "ExternalBoundaryIndex", type => "Int32", NumberOfComponents => "1", format => "ascii");
   my @fluxExternalVals;
   for (my $j=0; $j<$nvell; $j++) {
      push @fluxExternalVals, ($fluxBoundaryFullDomainBoundaryIndices[$fluxBoundaryCountStart+$j]) x $numPointsPerBoundaryNode;
   }
   $vtkFluxWriter->characters(join(" ", @fluxExternalVals));
   $vtkFluxWriter->endTag("DataArray");

   # local boundary index
   $vtkFluxWriter->startTag("DataArray",
      Name => "LocalBoundaryIndex", type => "Int32", NumberOfComponents => "1", format => "ascii");
   my @fluxLocalVals;
   for (my $j=0; $j<$nvell; $j++) {
      push @fluxLocalVals, ($fluxBoundaryLocalBoundaryIndices[$fluxBoundaryCountStart+$j]) x $numPointsPerBoundaryNode;
   }
   $vtkFluxWriter->characters(join(" ", @fluxLocalVals));
   $vtkFluxWriter->endTag("DataArray");
   $vtkFluxWriter->endTag("PointData");
   $vtkFluxWriter->endTag("Piece");

   #  F L U X   B O U N D A R Y   A S   X D M F   3 D S M E S H   G E O M E T R Y
   # write the boundary point geometry for this flux boundary
   my $numXYZValsPerNode = 2 * $numPointsPerBoundaryNode;
   my $numXYZVals = $nvell * $numXYZValsPerNode * 3;
   $xdmfFluxWriter->startTag("Grid", Name => "seg = $seg");
   $xdmfFluxWriter->startTag("Geometry", Type => "XYZ");
   $xdmfFluxWriter->startTag("DataItem",
      DataType => "Float",
      Dimensions => $numXYZVals,
      Format => "XML",
      Precision => "8");

   for (my $j=0; $j<$nvell; $j++) {
      # conpute the z value of the top of the boundary geometry
      $topZ[$j] = 1.0;  # arbitrary default
      # if the boundary node elevation is above the datum (negative)
      # then make the top of the boundary 1.0m above the boundary node elev
      if ( $z[$nbvv[$j]-1] < 0.0 ) {
         $topZ[$j] = -1.0*$z[$nbvv[$j]-1] + 1.0;
      }
      # if this is a levee boundary, the top of the boundary
      # geometry is the same as the specified levee height
      if ( $numPointsPerBoundaryNode == 2 ) {
         $topZ[$j] = $fluxBoundaryNodeElevs[$j];
      }
   }
   my $zrev;

   my @xdmfXYZ;
   # create the base front face boundary vertices (i.e., boundary node elevation)
   for (my $j=0; $j<$nvell; $j++) {
      $zrev = -1.0 * $z[$nbvv[$j]-1];
      push @xdmfXYZ, "$x[$nbvv[$j]-1] $y[$nbvv[$j]-1] $zrev";
   }
   # create the top front face boundary vertices
   for (my $j=0; $j<$nvell; $j++) {
      push @xdmfXYZ, "$x[$nbvv[$j]-1] $y[$nbvv[$j]-1] $topZ[$j]";
   }
   # if this is a levee boundary, create the back side geometry
   if ( $numPointsPerBoundaryNode == 2 ) {
      for (my $j=0; $j<$nvell; $j++) {
         push @xdmfXYZ, "$x[$ibconn[$j]-1] $y[$ibconn[$j]-1] $topZ[$j]";
      }
      for (my $j=0; $j<$nvell; $j++) {
         $zrev = -1.0 * $z[$ibconn[$j]-1];
         push @xdmfXYZ, "$x[$ibconn[$j]-1] $y[$ibconn[$j]-1] $zrev";
      }
   }
   $xdmfFluxWriter->characters(join("\n", @xdmfXYZ));
   $xdmfFluxWriter->endTag("DataItem");
   $xdmfFluxWriter->endTag("Geometry");
   $xdmfFluxWriter->emptyTag("Topology",
      Dimensions => "$numXYZValsPerNode $nvell 1",
      Type => "3DSMesh");
   $xdmfFluxWriter->endTag("Grid");
   my $adcStartNodeID;
   my $numFluxBoundaryGeometryElements;
   if ( defined $fluxBoundaries2dm ) {
      #
      #  F L U X   B O U N D A R Y   A S   2 D M   M E S H   G E O M E T R Y
      #
      # write out the flux-specified boundary tables as 2DM mesh geometry
      # to show boundary height, ref: https://www.xmswiki.com/wiki/SMS:2D_Mesh_Files_*.2dm
      # for reading into QGIS, ref:
      # https://docs.qgis.org/3.16/en/docs/user_manual/working_with_mesh/mesh_properties.html
      # https://github.com/lutraconsulting/MDAL
      my $boundaryNumber = sprintf("%04d",$i);
      my $twodmFluxBoundaryGeometryFileName = $meshfile . "_fluxBoundaryGeometry_$boundaryNumber.2dm";
      my $TWODMFLUXBOUNDARY;
      if (not open($TWODMFLUXBOUNDARY,">","$twodmFluxBoundaryGeometryFileName")) {
         ASGSUtil::stderrMessage("ERROR","Failed to open $twodmFluxBoundaryGeometryFileName for writing: $!.",$test);
         die;
      }
      # write header for boundary geometry file
      $numFluxBoundaryGeometryElements = $nvell - 1;
      if ( $numPointsPerBoundaryNode == 2 ) {
         $numFluxBoundaryGeometryElements *= 3;
      }
      printf $TWODMFLUXBOUNDARY "MESH2D\n";
      printf $TWODMFLUXBOUNDARY "NUM_MATERIALS_PER_ELEM 1\n";

      # write the node table for this flux boundary
      # ** use computed z values for boundary nodes from XDMF calculations above **
      my $j=0;                             #        2--3
      my $nodeID=1;                        # front  |  |  back
      $adcStartNodeID = $adcNodeID;     #        1  4
      while ( $j<$nvell ) {
         $zrev = -1.0 * $z[$nbvv[$j]-1];
         # write the base front face boundary vertex (i.e., boundary node elevation)
         printf $TWODMFLUXBOUNDARY "ND $nodeID $x[$nbvv[$j]-1] $y[$nbvv[$j]-1] $zrev\n";
         printf $TWODMNODEFLUXBOUNDARY "ND $adcNodeID $x[$nbvv[$j]-1] $y[$nbvv[$j]-1] $zrev\n";
         printf $ADCNODFLUXBOUNDARY "$adcNodeID $x[$nbvv[$j]-1] $y[$nbvv[$j]-1] $zrev\n";
         $nodeID++;
         $adcNodeID++;
         # write the top front face boundary vertex
         printf $TWODMFLUXBOUNDARY "ND $nodeID  $x[$nbvv[$j]-1] $y[$nbvv[$j]-1] $topZ[$j]\n";
         printf $TWODMNODEFLUXBOUNDARY "ND $adcNodeID $x[$nbvv[$j]-1] $y[$nbvv[$j]-1] $topZ[$j]\n";
         printf $ADCNODFLUXBOUNDARY "$adcNodeID  $x[$nbvv[$j]-1] $y[$nbvv[$j]-1] $topZ[$j]\n";
         $nodeID++;
         $adcNodeID++;
         # if there is a back side (i.e., this is a levee, not an external boundary) then
         # add those to the node table
         if ( $numPointsPerBoundaryNode == 2 ) {
            printf $TWODMFLUXBOUNDARY "ND $nodeID $x[$ibconn[$j]-1] $y[$ibconn[$j]-1] $topZ[$j]\n";
            printf $TWODMNODEFLUXBOUNDARY "ND $adcNodeID $x[$ibconn[$j]-1] $y[$ibconn[$j]-1] $topZ[$j]\n";
            printf $ADCNODFLUXBOUNDARY "$adcNodeID $x[$ibconn[$j]-1] $y[$ibconn[$j]-1] $topZ[$j]\n";
            $nodeID++;
            $adcNodeID++;
            $zrev = -1.0 * $z[$ibconn[$j]-1];
            printf $TWODMFLUXBOUNDARY "ND $nodeID $x[$ibconn[$j]-1] $y[$ibconn[$j]-1] $zrev\n";
            printf $TWODMNODEFLUXBOUNDARY "ND $adcNodeID $x[$ibconn[$j]-1] $y[$ibconn[$j]-1] $zrev\n";
            printf $ADCNODFLUXBOUNDARY "$adcNodeID $x[$ibconn[$j]-1] $y[$ibconn[$j]-1] $zrev\n";
            $nodeID++;
            $adcNodeID++;
         }
         $j++;
      }
      #
      # write the element table for this flux boundary
      #                                         6__7
      #      1: 1 2 4 3     2: 1 2 6 5          |\  \
      #                        2 3 7 6          | \  \
      #                        3 4 8 7         5 \ 2--3
      my $n=1; #                           front  \|  | back
      my $elementID=1;  #                          1  4
      my $n_full = $adcStartNodeID; # last one that was actually used (unless this is the first boundary being written)
      while ( $elementID<$numFluxBoundaryGeometryElements ) {
         if ( $numPointsPerBoundaryNode == 1 ) {
            my $a = $n;
            my $b = $n+1;
            my $c = $b+2;
            my $d = $b+1;
            printf $TWODMFLUXBOUNDARY "E4Q $elementID $a $b $c $d 1\n";
            $a = $n_full;
            $b = $n_full+1;
            $c = $b+2;
            $d = $b+1;
            printf $TWODMELEMENTFLUXBOUNDARY "E4Q $fullDomainElementID $a $b $c $d 1\n";
            $elementID++;
            $fullDomainElementID++;
         }
         if ( $numPointsPerBoundaryNode == 2 ) {
            for (my $e=0; $e<3; $e++) {
               my $a = $n+$e;
               my $b = $n+$e+1;
               my $c = $b+4;
               my $d = $b+3;
               printf $TWODMFLUXBOUNDARY "E4Q $elementID $a $b $c $d 1\n";
               $a = $n_full+$e;
               $b = $n_full+$e+1;
               $c = $b+4;
               $d = $b+3;
               printf $TWODMELEMENTFLUXBOUNDARY "E4Q $fullDomainElementID $a $b $c $d 1\n";
               $elementID++;
               $fullDomainElementID++;
            }
         }
         $n = $n + 2*$numPointsPerBoundaryNode;
         $n_full = $n_full + 2*$numPointsPerBoundaryNode;
      }
      close($TWODMFLUXBOUNDARY);
      #
      #            F L U X   B O U N D A R Y   A S
      #  A D C I R C   N O D E   A N D   E L E M E N T   T A B L E S
      #
      # already wrote the node table for this flux boundary in the 2dm section above
      # write the element table for this flux boundary
      my $n = $adcStartNodeID; # last one that was actually used (unless this is the first boundary being written)
      my $elementID=1;
      while ( $elementID<(2*$numFluxBoundaryGeometryElements) ) {
         if ( $numPointsPerBoundaryNode == 1 ) {
            my $a = $n;
            my $b = $n+1;
            my $c = $b+2;
            my $d = $b+1;
            printf $ADCELEFLUXBOUNDARY "$adcElementID 3 $a $b $d\n";
            $elementID++;
            $adcElementID++;
            printf $ADCELEFLUXBOUNDARY "$adcElementID 3 $b $c $d\n";
            $elementID++;
            $adcElementID++;
         }
         if ( $numPointsPerBoundaryNode == 2 ) {
            for (my $e=0; $e<3; $e++) {
               my $a = $n+$e;
               my $b = $n+$e+1;
               my $c = $b+4;
               my $d = $b+3;
               printf $ADCELEFLUXBOUNDARY "$adcElementID 3 $a $b $d\n";
               $elementID++;
               $adcElementID++;
               printf $ADCELEFLUXBOUNDARY "$adcElementID 3 $b $c $d\n";
               $elementID++;
               $adcElementID++;
            }
         }
         $n = $n + 2*$numPointsPerBoundaryNode;
      }
   }
   #
   #  F L U X   B O U N D A R Y   A S
   #      A R C  M A P   C S V
   # write out the flux-specified boundary tables as ArcMap CSV
   if ( defined $arcMapCSV && $numPointsPerBoundaryNode == 2 ) {
      for (my $j=0; $j<$nvell; $j++) {
         printf $ARCMAPFLUXBOUNDARY "$nbvv[$j],$x[$nbvv[$j]-1],$y[$nbvv[$j]-1],$fluxBoundaryNodeElevs[$j],$z[$nbvv[$j]-1]\n";
         printf $ARCMAPFLUXBOUNDARY "$ibconn[$j] $x[$ibconn[$j]-1] $y[$ibconn[$j]-1] $fluxBoundaryNodeElevs[$j] $z[$ibconn[$j]-1]\n";
      }
   }
}
close(MESH);
# finish echo boundary table
close(VTKECHOFLUXBOUNDARY);
# finish writing boundary as vtk points (.vtp file)
$vtkFluxWriter->endTag("PolyData");
$vtkFluxWriter->endTag("VTKFile");
$vtkFluxWriter->end();
close(VTKFLUXBOUNDARY);
# finish writing boundary as xdmf geometry (.xmf file)
$xdmfFluxWriter->endTag("Grid");
$xdmfFluxWriter->endTag("Domain");
$xdmfFluxWriter->endTag("Xdmf");
$xdmfFluxWriter->end();
close(XDMFFLUXBOUNDARY);
# finish writing boundary geometry as adcirc mesh (.14 file)
close($ADCNODFLUXBOUNDARY);
close($ADCELEFLUXBOUNDARY);
# close arcmap csv file (if specified)
if ( defined $arcMapCSV  ) {
   close($ARCMAPFLUXBOUNDARY);
}
# open a file for the full mesh
my $adcFluxBoundaryGeometryFileName = $meshfile . "_fluxBoundaryGeometry.14";
my $ADCFLUXBOUNDARY;
if (not open($ADCFLUXBOUNDARY,">","$adcFluxBoundaryGeometryFileName")) {
   ASGSUtil::stderrMessage("ERROR","Failed to open '$adcFluxBoundaryGeometryFileName' for writing: $!.",$test);
   die;
}
my $adcFluxBoundaryGeometryNodeFileName = $meshfile . "_fluxBoundaryGeometry.nod";
my $ADCNODFLUXBOUNDARY;
if (not open($ADCNODFLUXBOUNDARY,"<","$adcFluxBoundaryGeometryNodeFileName")) {
   ASGSUtil::stderrMessage("ERROR","Failed to open '$adcFluxBoundaryGeometryNodeFileName' for writing: $!.",$test);
   die;
}
my $adcFluxBoundaryGeometryElementFileName = $meshfile . "_fluxBoundaryGeometry.ele";
my $ADCELEFLUXBOUNDARY;
if (not open($ADCELEFLUXBOUNDARY,"<","$adcFluxBoundaryGeometryElementFileName")) {
   ASGSUtil::stderrMessage("ERROR","Failed to open '$adcFluxBoundaryGeometryElementFileName' for writing: $!.",$test);
   die;
}
printf $ADCFLUXBOUNDARY "# ASGS adc2vtk.pl '$meshfile' flux boundary geometry as adcirc mesh\n";
my $numNodes = $adcNodeID - 1;
my $numElements = $adcElementID - 1;
printf $ADCFLUXBOUNDARY "$numElements $numNodes  ! numElements numNodes\n";
my $file_content = do { local $/; <$ADCNODFLUXBOUNDARY> };
print $ADCFLUXBOUNDARY $file_content;
my $file_content = do { local $/; <$ADCELEFLUXBOUNDARY> };
print $ADCFLUXBOUNDARY $file_content;
printf $ADCFLUXBOUNDARY "0 ! number of elevation-specified boundaries\n";
printf $ADCFLUXBOUNDARY "0 ! number of elevation-specified boundary nodes\n";
printf $ADCFLUXBOUNDARY "0 ! number of flux-specified boundaries\n";
printf $ADCFLUXBOUNDARY "0 ! number of flux-specified boundary nodes\n";
close($ADCFLUXBOUNDARY);
close($ADCNODFLUXBOUNDARY);
close($ADCELEFLUXBOUNDARY);
# fulldomain 2dm file of flux boundaries
if ( defined $fluxBoundaries2dm ) {
   close($TWODMNODEFLUXBOUNDARY); # close for writing, open for reading
   my $twodmFluxBoundaryGeometryFileName = $meshfile . "_fluxBoundaryGeometry.2dm";
   my $TWODMFLUXBOUNDARY;
   if (not open($TWODMFLUXBOUNDARY,">","$twodmFluxBoundaryGeometryFileName")) {
      ASGSUtil::stderrMessage("ERROR","Failed to open $twodmFluxBoundaryGeometryFileName for writing: $!.",$test);
      die;
   }
   printf $TWODMFLUXBOUNDARY "MESH2D\n";
   printf $TWODMFLUXBOUNDARY "NUM_MATERIALS_PER_ELEM 1\n";
   my $adcFluxBoundaryGeometryNodeFileName = $meshfile . "_fluxBoundaryGeometry.nd";
   my $TWODMNODEFLUXBOUNDARY;
   if (not open($TWODMNODEFLUXBOUNDARY,"<","$twodmFluxBoundaryGeometryNodeFileName")) {
      ASGSUtil::stderrMessage("ERROR","Failed to open '$twodmFluxBoundaryGeometryNodeFileName' for reading: $!.",$test);
      die;
   }
   my $file_content = do { local $/; <$TWODMNODEFLUXBOUNDARY> };
   print $TWODMFLUXBOUNDARY $file_content;
   close($TWODMNODEFLUXBOUNDARY);
   close($TWODMELEMENTFLUXBOUNDARY); # close for writing, open for reading
   my $adcFluxBoundaryGeometryElementFileName = $meshfile . "_fluxBoundaryGeometry.e4q";
   my $TWODMELEMENTFLUXBOUNDARY;
   if (not open($TWODMELEMENTFLUXBOUNDARY,"<","$twodmFluxBoundaryGeometryElementFileName")) {
      ASGSUtil::stderrMessage("ERROR","Failed to open '$twodmFluxBoundaryGeometryElementFileName' for reading: $!.",$test);
      die;
   }
   my $file_content = do { local $/; <$TWODMELEMENTFLUXBOUNDARY> };
   print $TWODMFLUXBOUNDARY $file_content;
   close($TWODMELEMENTFLUXBOUNDARY);
   close($TWODMFLUXBOUNDARY);
}
#
# write data from adcirc file(s)
foreach my $file (@adcircfiles) {
   ASGSUtil::stderrMessage("INFO","Processing $file.",$test);
   my $num_components = 0; # 1 if scalar, 2 if 2D vector, 3 if 3D vector
   my $num_datasets = 0;   # 0 if unknown
   my $scalars_name = "";
   # set some parameters based on the type of file we are working with
   if ( $file eq "none" || $file eq "fort.14" ) {
      my $outfile = $meshfile . ".vtu";
      # start writing vtk-formatted file
      if (not open(OUT,">","$outfile")) {
         ASGSUtil::stderrMessage("ERROR","Failed to open vtk file $outfile for writing: $!.",$test);
         die;
      }
      $outWriter = newXMLWriter(\*OUT);
      &writeHeader($ne, $np);
      $outWriter->startTag("PointData", Scalars => "BathymetricDepth");
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
   if ( $file eq "elementareas.100" ) {
      $num_components = 1;
      $num_datasets = 1;
      $datacentered = "CellData";
   }
   if ( $file eq "particles_count_peak.200"  ) {
      $num_components = 1;
      $num_datasets = 1;
      $datacentered = "CellData";
      $datatype = "Int32";
   }
   if ( $file eq "particles_perarea_peak.200" || $file eq "particles_pervolume_peak.200" ) {
      $num_components = 1;
      $num_datasets = 1;
      $datacentered = "CellData";
      $datatype = "Float64";
   }
   if ( $file eq "maxele.63" || $file eq "maxwvel.63" || $file eq "minpr.63" || $file eq "ESLNodes.63" || $file eq "minedgelengths.63" || $file eq "edgelengthgradients.63" ) {
      $num_components = 1;
      $num_datasets = 1;
      $datatype = "Float64";
   }
   if ( $file eq "residents.63" || $file eq "ghosts.63" || $file eq "absolutes.63" || $file eq "subdomains.63" || $file eq "psubdomains.63" || $file eq "ghostmem.63" ) {
      $num_components = 1;
      $num_datasets = 1;
      $datatype = "Int32";
   }
   if ( $file eq "noff.100" || $file eq "noffornot.100"  ) {
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
      $datacentered = "Cel`lData";
   }
   # make sure we can actually open the adcirc file before going further
   unless (open(ADCIRCFILE,"<$file")) {
      ASGSUtil::stderrMessage("ERROR",
          "Failed to open ADCIRC file $file for reading: $!.",$test);
      next;
   }

   # for nodal attributes, we read the file entirely differently from an
   # output file
   if ( $file eq "fort.13" ) {
      my $outfile = $file . ".vtu";
      if (not open(OUT,">$outfile")) {
         ASGSUtil::stderrMessage("ERROR","Failed to open vtk file $outfile for writing: $!.",$test);
         die;
      }
      $outWriter = newXMLWriter(\*OUT);
      &writeHeader($ne, $np);
      $outWriter->startTag("PointData", Scalars => "NodalAttributes");

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
               ASGSUtil::stderrMessage("ERROR","Ran out of data: $!.",$test);
               die;
            }
            $attrValues[$fields[0]-1] = $fields[1];
         }
         $scalars_name = "Scalars=\"$attrName\"";
         # write out dataset from ADCIRC file
         my $vtk_components = 1;
         $outWriter->startTag("DataArray",
            Name => $attrName,
            type => "Float64",
            NumberOfComponents => $vtk_components,
            format => "ascii");
         $outWriter->characters(join("\n", @attrValues[0..$np-1]));
         $outWriter->endTag("DataArray");
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
      # than one, so write a separate PVD collection file
      my $outfile = $file . ".pvd";
      unless (open(PVD,">$outfile")) {
         ASGSUtil::stderrMessage("ERROR",
            "Failed to open vtk file $outfile for writing: $!.",$test);
         die;
      }
      $pvdWriter = newXMLWriter(\*PVD);
      $pvdWriter->xmlDecl();
      $pvdWriter->startTag("VTKFile",
         type => "Collection",
         version => "0.1",
         byte_order => "LittleEndian");
      $pvdWriter->startTag("Collection");
   }
   my $dataset = 0;
   my @comp; # components of the dataset
   while (<ADCIRCFILE>) {
      @fields = split(' ',$_);
      $time[$dataset] = $fields[0];
      $timestep[$dataset] = $fields[1];
      my @mag; # for holding vector magnitudes
      my $io_success = "true";
      my $lim=$np; # nodal values are the default
      if ( $datacentered eq "CellData" ) {
         $lim=$ne;
      }

      # read one dataset from adcirc data file
      for (my $i=0; $i<$lim; $i++) {
         $line = <ADCIRCFILE>;
         if ( defined $line ) {
            @fields = split(' ',$line);
         } else {
            ASGSUtil::stderrMessage("ERROR","Ran out of data: $!.",$test);
            die;
         }
         # get rid of the node/element index or node/element ID
         shift(@fields);
         if ( $num_components == 2 ) {
            $mag[$i] = sqrt($fields[0]**2 + $fields[1]**2);
            push(@fields,"0.0"); # vtk expects all vectors to be 3D
         }
         $comp[$i] = join(' ',@fields);
      }

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
         ASGSUtil::stderrMessage("ERROR","Failed to open vtk file $outfile for writing: $!.",$test);
         die;
      }
      if ( $num_datasets == 0 ) {
         $pvdWriter->emptyTag("DataSet",
            timestep => $time[$dataset],
            group => "",
            part => "0",
            file => $outfile);
      }
      $outWriter = newXMLWriter(\*OUT);
      &writeHeader($ne, $np);
      my @centerAttrs;
      if ($scalars_name =~ /Scalars="([^"]*)"/) {
         push @centerAttrs, Scalars => $1;
      }
      if ($vectors_name =~ /Vectors="([^"]*)"/) {
         push @centerAttrs, Vectors => $1;
      }
      $outWriter->startTag($datacentered, @centerAttrs);

      my $vtk_components = $num_components;
      if ( $num_components == 2 ) {
         $vtk_components = $num_components + 1;
      }
      $outWriter->startTag("DataArray",
         Name => $adcirctypes{$file},
         type => $datatype,
         NumberOfComponents => $vtk_components,
         format => "ascii");
      $outWriter->characters(join("\n", @comp[0..$lim-1]));
      $outWriter->endTag("DataArray");

      if ( $num_components > 1 ) {
         $outWriter->startTag("DataArray",
            Name => $adcirctypes{$file} . "Magnitude",
            type => $datatype,
            NumberOfComponents => "1",
            format => "ascii");
         $outWriter->characters(join("\n", @mag[0..$lim-1]));
         $outWriter->endTag("DataArray");
      }
      if ($datacentered eq "CellData") {
         $outWriter->endTag("CellData");
         $outWriter->startTag("PointData");
      }
      writeMesh($ne, $np);  # write out bathymetric depth as a dataset
      writeFooter();
      close(OUT);
      $dataset++;
      # only write the number of datasets as specified according to the filetype
      if ( $num_datasets != 0 && $dataset >= $num_datasets ) {
         last;
      }
   }
   if ( $num_datasets == 0 ) {
      $pvdWriter->endTag("Collection");
      $pvdWriter->endTag("VTKFile");
      $pvdWriter->end();
      close(PVD);
   }
   close(ADCIRCFILE);
}

sub writeHeader () {
   my $ne = shift;
   my $np = shift;
   $outWriter->xmlDecl();
   $outWriter->startTag("VTKFile",
      type => "UnstructuredGrid",
      version => "0.1",
      byte_order => "LittleEndian");
   $outWriter->startTag("UnstructuredGrid");
   $outWriter->startTag("Piece",
      NumberOfPoints => $np,
      NumberOfCells => $ne);
}

sub writeFooter () {
   $outWriter->endTag("Piece");
   $outWriter->endTag("UnstructuredGrid");
   $outWriter->endTag("VTKFile");
   $outWriter->end();
}

sub writeMesh () {
   my $ne = shift;
   my $np = shift;

   # write node IDs if specified
   if ( defined $getNodeIndices ) {
      $outWriter->startTag("DataArray",
         Name => "NodeArrayIndices",
         type => "Int32",
         NumberOfComponents => "1",
         format => "ascii");
      $outWriter->characters(join("\n", @nodeIndices[0..$np-1]));
      $outWriter->endTag("DataArray");
   }

   # write the BathymetricDepth
   $outWriter->startTag("DataArray",
      Name => "BathymetricDepth",
      type => "Float64",
      NumberOfComponents => "1",
      format => "ascii");
   $outWriter->characters(join("\n", @z[0..$np-1]));
   $outWriter->endTag("DataArray");
   $outWriter->endTag("PointData");

   $outWriter->startTag("Points");
   $outWriter->startTag("DataArray",
      type => "Float64",
      NumberOfComponents => "3",
      format => "ascii");
   $outWriter->characters(join("\n", map { "$x[$_] $y[$_] 0.0" } 0..$np-1));
   $outWriter->endTag("DataArray");
   $outWriter->endTag("Points");

   # write element IDs if specified
   if ( defined $getElementIndices ) {
      $outWriter->startTag("CellData", Scalars => "ElementArrayIndices");
      $outWriter->startTag("DataArray",
         Name => "ElementArrayIndices",
         type => "Int32",
         NumberOfComponents => "1",
         format => "ascii");
      $outWriter->characters(join("\n", @elementIndices[0..$ne-1]));
      $outWriter->endTag("DataArray");
      $outWriter->endTag("CellData");
   }

   # write element connectivity indices
   $outWriter->startTag("Cells");
   $outWriter->startTag("DataArray",
      type => "Int32",
      Name => "connectivity",
      format => "ascii");
   $outWriter->characters(join("\n", @conn[0..$ne-1]));
   $outWriter->endTag("DataArray");

   $outWriter->startTag("DataArray",
      type => "Int32",
      Name => "offsets",
      format => "ascii");
   $outWriter->characters(join("\n", map { $_ * 3 + 3 } 0..$ne-1));
   $outWriter->endTag("DataArray");

   $outWriter->startTag("DataArray",
      type => "UInt8",
      Name => "types",
      format => "ascii");
   $outWriter->characters(join("\n", (5) x $ne)); # triangles
   $outWriter->endTag("DataArray");
   $outWriter->endTag("Cells");
}

sub newXMLWriter {
   my ($fh) = @_;
   return XML::Writer->new(
      OUTPUT => $fh,
      DATA_MODE => 1,
      DATA_INDENT => "   "
   );
}