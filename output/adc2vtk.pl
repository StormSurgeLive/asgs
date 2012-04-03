#!/usr/bin/perl
#
# adc2vtk.pl
#
# This script reformats ADCIRC input or output files to vtk xml format for
# visualization and/or analysis.
#
use strict;
$^W++;
use Getopt::Long;
#
my %adcirctypes = ("maxele.63", "MaximumElevation",
                   "maxwvel.63", "MaximumWindSpeed",
                   "minpr.63", "MinimumBarometricPressure",
                   "fort.63", "WaterSurfaceElevation",
                   "fort.64", "WaterCurrentVelocity",
                   "fort.73", "BarometricPressure",
                   "fort.74", "WindVelocity");
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
my $meshfile = "fort.14";
my $cpp;  # 1 to reproject to cpp (carte parallelogrammatique projection)
my $slam0 = 265.5; # longitude at center of projection
my $sfea0 = 29.0;  # latitude at center of projection
my @adcircfiles;
#
GetOptions(
           "meshfile=s" => \$meshfile,
           "cpp" => \$cpp,
           "slam0=s" => \$slam0,
           "sfea0=s" => \$sfea0,
           "adcircfiles=s" => \@adcircfiles
         );
#
unless ( defined @adcircfiles ) {
   $adcircfiles[0] = "none";
} else {
   @adcircfiles = split(/,/,join(',',@adcircfiles)); # get a list w/o commas
}
#
unless (open(MESH,"<$meshfile")) {
   stderrMessage("ERROR","Failed to open mesh file $meshfile for reading: $!.");
   die;
}
# read number of nodes and number of elements from adcirc mesh file
my $line = <MESH>;     # read AGRID (comment line in mesh file)
$line = <MESH>;        # read number of elements and number of points line
my @fields = split(' ',$line);
my $ne = $fields[0];
my $np = $fields[1];
for (my $i=0; $i<$np; $i++) {
   $line = <MESH>;
   @fields = split(' ',$line);
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

for (my $i=0; $i<$ne; $i++) {
   $line = <MESH>;
   @fields = split(' ',$line);
   # have to subtract 1 from the ADCIRC node numbers because vtk is 0 indexed
   my $i1 = $fields[2]-1;
   my $i2 = $fields[3]-1;
   my $i3 = $fields[4]-1;
   $conn[$i] = " $i1 $i2 $i3 ";
}
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
   if ( $file eq "maxele.63" || $file eq "maxwvel.63" || $file eq "minpr.63" ) {
      $num_components = 1;
      $num_datasets = 1;
   }
   if ( $file eq "fort.63" || $file eq "fort.73" ) {
      $num_components = 1;
      $num_datasets = 0;
   }
   if ( $file eq "fort.74" || $file eq "fort.64" ) {
      $num_components = 2;
      $num_datasets = 0;
   }
   # make sure we can actually open the adcirc file before going further
   unless (open(ADCIRCFILE,"<$file")) {
      stderrMessage("ERROR",
          "Failed to open ADCIRC file $file for reading: $!.");
         next;
   }
   # parse down to where the data starts
   $line = <ADCIRCFILE>;  # read comment line (not used)
   $line = <ADCIRCFILE>;  # read header line (not used)
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
      for (my $i=0; $i<$np; $i++) {
         $line = <ADCIRCFILE>;
         if ( defined $line ) {
            @fields = split(' ',$line);
         } else {
            stderrMessage("ERROR","Ran out of data: $!.");
            die;
         }
         # get rid of the node number
         shift(@fields);
         if ( $num_components == 2 ) {
            # calculate vector magnitude
            $mag[$i] = sqrt($fields[0]**2 + $fields[1]**2);
            push(@fields,"0.0"); # vtk expects all vectors to be 3D
         }
         $comp[$i] = join(' ',@fields);
      }
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
      printf OUT "         <PointData $scalars_name $vectors_name>\n"; # set default dataset
      # write out dataset from ADCIRC file
      my $vtk_components = $num_components;
      if ( $num_components == 2 ) {
         $vtk_components = $num_components + 1; # for vtk all vectors are 3D
      }
      printf OUT "            <DataArray Name=\"$adcirctypes{$file}\" type=\"Float64\" NumberOfComponents=\"$vtk_components\" format=\"ascii\">\n";
      for (my $i=0; $i<$np; $i++) {
         printf OUT "$comp[$i]\n";
      }
      printf OUT "            </DataArray>\n";
      # write vector magnitude if this is a vector dataset
      if ( $num_components > 1 ) {
         printf OUT "            <DataArray Name=\"$adcirctypes{$file}Magnitude\" type=\"Float64\" NumberOfComponents=\"1\" format=\"ascii\">\n";
         for (my $i=0; $i<$np; $i++) {
            printf OUT "$mag[$i]\n";
         }
         printf OUT "            </DataArray>\n";
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


