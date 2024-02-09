#!/usr/bin/env perl
#--------------------------------------------------------------------------
# control_file_gen.pl
#

# This script uses the template fort.15 file and the ATCF formatted fort.22
# file as input and produces a fort.15 file as output. The name of the template
# file and the fort.22 file to be used as input must be specified on the
# command line.
#
# It optionally accepts the csdate (YYYYMMDDHH24), that is, the
# calendar time that corresponds to t=0 in simulation time. If it is
# not provided, the first line in the fort.22 file is used as the cold start
# time, and this time is written to stdout.
#
# It optionally accepts the time in a hotstart file in seconds since cold
# start.
#
# If the time of a hotstart file has been supplied, the fort.15 file
# will be set to hotstart.
#
# It optionally accepts the end time (YYYYMMDDHH24) at which the simulation
# should stop (e.g., if it has gone too far inland to continue to be
# of interest).
#
# If the --name option is set to nowcast, the RNDAY will be calculated such
# that the run will end at the nowcast time.
#
# The --dt option can be used to specify the time step size if it is
# different from the default of 3.0 seconds.
#
# The --bladj option can be used to specify the Boundary Layer Adjustment
# parameter for the Holland model (not used by the asymmetric wind vortex
# model, NWS=9.
#
# The NHSINC will be calculated such that a hotstart file is always generated
# on the last time step of the run.
#
# usage:
#   %perl control_file_gen.pl [--cst csdate] [--hst hstime]
#   [--dt timestep] [--nowcast] [--controltemplate templatefile] < storm1_fort.22
#
#--------------------------------------------------------------------------
# Copyright(C) 2006--2024 Jason Fleming
# Copyright(C) 2006, 2007 Brett Estrade
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
#jgf20120124: standalone usage example for making a fort.15 for the
# ec95d mesh. It was for a tides-only run.
#perl ~/asgs/trunk/control_file_gen.pl --controltemplate ~/asgs/trunk/input/ec_95_fort.15_template --gridname fort.14 --cst 2012010100 --endtime 7 --dt 30.0 --nws 0 --hsformat binary --name hindcast --fort63freq 3600.0

use strict;
use warnings;
use Getopt::Long;
use YAML::Tiny qw(Load);
use Date::Calc;
use Cwd;
use ASGSUtil;
#
my $nscreen=-1000; # frequency of time step output to STDOUT(+) or adcirc.log(-)
my %logLevelsNABOUT = ('DEBUG' => -1, 'ECHO' => 0, 'INFO' => 1, 'WARNING' => 2, 'ERROR' => 3 );
my $fort61freq=0; # output frequency in SECONDS
my $fort61append; # if defined, output files will be appended across hotstarts
my $fort62freq=0; # output frequency in SECONDS
my $fort62append; # if defined, output files will be appended across hotstarts
my $fort63freq=0; # output frequency in SECONDS
my $fort63append; # if defined, output files will be appended across hotstarts
my $fort64freq=0; # output frequency in SECONDS
my $fort64append; # if defined, output files will be appended across hotstarts
my $fort7172freq=0; # output frequency in SECONDS
my $fort7172append; # if defined, output files will be appended across hotstart
my $fort7374freq=0; # output frequency in SECONDS
my $fort7374append; # if defined, output files will append across hotstarts
my ($fort61, $fort62, $fort63, $fort64, $fort7172, $fort7374);
our $sparseoutput; # if defined, then fort.63 and fort.64 will be sparse ascii
my $hsformat="binary";  # input param for hotstart format: binary or netcdf
my ($fort61netcdf, $fort62netcdf, $fort63netcdf, $fort64netcdf, $fort7172netcdf, $fort7374netcdf); # for netcdf (not ascii) output
my $hotswan = "on"; # "off" if swan has to be cold started (only on first nowcast)
our $netcdf4;  # if defined, then netcdf files should use netcdf4 formatting
my $output_start = "0.0"; # days after cold start when output should start
my $output_end = "9999.0"; # days after cold start when output should end
#
my @TRACKS = (); # should be few enough to store all in an array for easy access
my $controltemplate;
my $elevstations="null"; # file containing list of adcirc elevation stations
my $velstations="null";  # file with list of adcirc velocity stations
my $metstations="null";  # file with list of adcirc meteorological stations
my $swantemplate;
my $metfile;
my $gridname="nc6b";
our $csdate;
our ($cy, $cm, $cd, $ch, $cmin, $cs); # ADCIRC cold start time
our ($ny, $nm, $nd, $nh, $nmin, $ns); # current ADCIRC time
our ($ey, $em, $ed, $eh, $emin, $es); # ADCIRC end time
our ($oy, $om, $od, $oh, $omin, $os); # OWI start time
my $numelevstations="0"; # number and list of adcirc elevation stations
my $numvelstations="0";  # number and list of adcirc velocity stations
my $nummetstations="0";  # number and list of adcirc meteorological stations
my $startdatetime; # formatted for swan fort.26
my $enddatetime;   # formatted for swan fort.26
my $hstime;      # time, in seconds, of hotstart file (since coldstart)
my $hstime_days; # time, in days, of hotstart file (since coldstart)
our $endtime;    # time at which the run should end (yyyymmddhh24)
our $dt=3.0;      # adcirc time step, in seconds
my $swandt=600.0; # swan time step, in seconds
my $bladj=0.9;
our $enstorm;    # ensemble name of the storm
my $tau=0; # forecast period
my $dir=getcwd();
my $nws=0;
my $advisorynum="0";
my $particles;  # flag to produce fulldomain current velocity files at an
                # increment of 30 minutes
our $NHSINC;    # time step increment at which to write hot start files
our $NHSTAR;    # writing and format of ADCIRC hotstart output file
our $RNDAY;     # total run length from cold start, in days
my $nffr = -1;  # for flux boundaries; -1: top of fort.20 corresponds to hs
my $ihot;       # whether or not ADCIRC should READ a hotstart file
my $fdcv;       # line that controls full domain current velocity output
our $wtiminc_line;   # parameters related to met and wave timing
our $rundesc;   # description of run, 1st line in fort.15
our $scenarioid; # run id, 2nd line in fort.15
our $specifiedRunLength; # time in days for run if there is no externally specified forcing
my ($m2nf, $s2nf, $n2nf, $k2nf, $k1nf, $o1nf, $p1nf, $q1nf); # nodal factors
my ($m2eqarg, $s2eqarg, $n2eqarg, $k2eqarg, $k1eqarg, $o1eqarg, $p1eqarg, $q1eqarg); # equilibrium arguments
my $periodicflux="null";  # the name of a file containing the periodic flux unit discharge data for constant inflow boundaries
my $fluxdata;
my $staticoffset = "null";
my $unitoffsetfile = "null";
our $addHours; # duration of the run (hours)
our $nds;      # number of datasets expected to be placed in a file
# multiples of Rmax for wind blending
my $pureVortex = "3.0";
my $pureBackground = "5.0";
# ASCII ADCIRC OWI file
our $nwset = 1;  # number of wind datasets (basin, region, local)
our $nwbs = 0;   # number of blank snaps
our $dwm = "1.0";
my $parameters;  # YAML document that defines model control options
#
GetOptions("controltemplate=s" => \$controltemplate,
           "swantemplate=s" => \$swantemplate,
           "elevstations=s" => \$elevstations,
           "velstations=s" => \$velstations,
           "metstations=s" => \$metstations,
           "metfile=s" => \$metfile,
           "name=s" => \$enstorm,
           "gridname=s" => \$gridname,
           "cst=s" => \$csdate,
           "endtime=s" => \$endtime,
           "dt=s" => \$dt,
           "swandt=s" => \$swandt,
           "bladj=s" => \$bladj,
           "nws=s" => \$nws,
           "advisorynum=s" => \$advisorynum,
           "hstime=s" => \$hstime,
           "nscreen=s"   => \$nscreen,
           "fort61freq=s" => \$fort61freq,
           "fort62freq=s" => \$fort62freq,
           "fort63freq=s" => \$fort63freq,
           "fort64freq=s" => \$fort64freq,
           "fort7172freq=s" => \$fort7172freq,
           "fort7374freq=s" => \$fort7374freq,
           "fort61append" => \$fort61append,
           "fort62append" => \$fort62append,
           "fort63append" => \$fort63append,
           "fort64append" => \$fort64append,
           "fort7172append" => \$fort7172append,
           "fort7374append" => \$fort7374append,
           "fort61netcdf" => \$fort61netcdf,
           "fort62netcdf" => \$fort62netcdf,
           "fort63netcdf" => \$fort63netcdf,
           "fort64netcdf" => \$fort64netcdf,
           "fort7172netcdf" => \$fort7172netcdf,
           "fort7374netcdf" => \$fort7374netcdf,
           "netcdf4" => \$netcdf4,
           "sparse-output" => \$sparseoutput,
           "hsformat=s" => \$hsformat,
           "hotswan=s" => \$hotswan,
           "periodicflux=s" => \$periodicflux,
           "pureVortex=s" => \$pureVortex,
           "pureBackground=s" => \$pureBackground
           );
#
# load YAML document containing model control parameters from stdin
$parameters = do { local $/; <> };
our $p = Load($parameters); # load parameters into the hashref p
#
# parse out the pieces of the cold start date
$csdate=~ m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
$cy = $1;
$cm = $2;
$cd = $3;
$ch = $4;
$cmin = 0.0;
$cs = 0.0;
#
# initialize "now" to a reasonable value
$ny = $1;
$nm = $2;
$nd = $3;
$nh = $4;
$nmin = $cmin;
$ns = $cs;
#
#
#----------------------------------------------------
#
#  A D C I R C   C O N T R O L   F I L E
#
# open template file for fort.15
unless (open(TEMPLATE,"<$controltemplate")) {
   ASGSUtil::stderrMessage("ERROR","Failed to open the fort.15 template file $controltemplate for reading: $!.");
   die;
}
#
# call subroutine that knows how to fill in the fort.15 for each particular
# type of forcing
if ( abs($nws) == 19 || abs($nws) == 319 || abs($nws) == 20 || abs($nws) == 320 || abs($nws) == 8 || abs($nws) == 308 || abs($nws) == 30 || abs($nws) == 330 ) {
   ASGSUtil::stderrMessage("DEBUG","Setting parameters appropriately for vortex model.");
   vortexModelParameters($nws);
   # for getting the OWI wind time increment for blended winds
   # and appending it to the wtiminc line
   if ( abs($nws) == 30 || abs($nws) == 330 ) {
      $wtiminc_line .= " $p->{meteorology}->{wtiminc} $pureVortex $pureBackground";
   }
} elsif ( abs($nws) == 12 || abs($nws) == 312 ) {
   owiParameters();
   $wtiminc_line = "$p->{meteorology}->{wtiminc}";
} elsif ( defined $specifiedRunLength ) {
   ASGSUtil::stderrMessage("DEBUG","The duration of this $enstorm run is specially defined.");
   customParameters();
} elsif ( $enstorm eq "hindcast" ) {
   ASGSUtil::stderrMessage("DEBUG","This is a model initialization run.");
   initializationParameters();
}
#
# we want a hotstart file if this is a nowcast or model initialization
if ( $enstorm eq "nowcast" || $enstorm eq "hindcast" ) {
   $NHSTAR = 1;
   if ( $hsformat eq "netcdf" || $hsformat eq "netcdf3" ) {
      $NHSTAR = 3;
      if ( defined $netcdf4 && $hsformat ne "netcdf3" ) {
         $NHSTAR = 5;
      }
   }
} else {
   $NHSTAR = 0;
   $NHSINC = 99999;
}
#
# we always look for a fort.68 file, and since we only write one hotstart
# file during the run, we know we will always be left with a fort.67 file.
if ( defined $hstime ) {
   $ihot = 68;
   if ( $hsformat eq "netcdf" || $hsformat eq "netcdf3" ) {
      $ihot = 368;
      if ( defined $netcdf4 && $hsformat ne "netcdf3" ) {
         $ihot = 568;
      }
   }
} else {
   $ihot = 0;
   $nffr = 0;
}
# [de]activate output files with time step increment and with(out) appending.
my $fort61specifier = getSpecifier($fort61freq,$fort61append,$fort61netcdf);
my $incr = getIncrement($fort61freq,$dt);
$fort61 = "$fort61specifier $output_start $output_end $incr";
my $fort62specifier = getSpecifier($fort62freq,$fort62append,$fort62netcdf);
$incr = getIncrement($fort62freq,$dt);
$fort62 = "$fort62specifier $output_start $output_end $incr";
#
my $fort63specifier = getSpecifier($fort63freq,$fort63append,$fort63netcdf);
my $fort64specifier = getSpecifier($fort64freq,$fort64append,$fort64netcdf);
if ( defined $sparseoutput ) {
   unless ( defined $fort63netcdf ) {
      $fort63specifier *= 4;
   }
   unless ( defined $fort64netcdf ) {
      $fort64specifier *= 4;
   }
}
$incr = getIncrement($fort63freq,$dt);
$fort63 = "$fort63specifier $output_start $output_end $incr";
$incr = getIncrement($fort64freq,$dt);
$fort64 = "$fort64specifier $output_start $output_end $incr";
my $fort7172specifier = getSpecifier($fort7172freq,$fort7172append,$fort7172netcdf);
my $fort7374specifier = getSpecifier($fort7374freq,$fort7374append,$fort7374netcdf);

# Casey 121009: Debug for sparse output.
if ( defined $sparseoutput ) {
   unless ( defined $fort7374netcdf ) {
      $fort7374specifier *= 4;
   }
}
$incr = getIncrement($fort7172freq,$dt);
$fort7172 = "$fort7172specifier $output_start $output_end $incr";
$incr = getIncrement($fort7374freq,$dt);
$fort7374 = "$fort7374specifier $output_start $output_end $incr";

if ( $nws eq "0" ) {
   $fort7172 = "NO LINE HERE";
   $fort7374 = "NO LINE HERE";
}
# add swan time step to WTIMINC line if wave coupling was specified
if ( $p->{wave_coupling}->{waves} eq "on"  ) {
   $wtiminc_line .= " $p->{wave_coupling}->{rstiminc}";
}
#
# tides
my $tides = "off";
my $tide_fac_message = `tide_fac.x --length $RNDAY --year $cy --month $cm --day $cd --hour $ch -n 8 m2 s2 n2 k1 k2 o1 q1 p1 --outputformat simple --outputdir .`;
if ( $tide_fac_message =~ /ERROR|WARNING/ ) {
   ASGSUtil::stderrMessage("WARNING","There was an issue when running tide_fac.x: $tide_fac_message.");
} else {
   ASGSUtil::stderrMessage("INFO","Nodal factors and equilibrium arguments were written to the file 'tide_fac.out'.");
   # open data file
   unless (open(TIDEFAC,"<tide_fac.out")) {
      ASGSUtil::stderrMessage("ERROR","Failed to open the file 'tide_fac.out' for reading: $!.");
      die;
   }
   # parse out nodal factors and equilibrium arguments from the
   # various constituents
   $tides = "on";
   ASGSUtil::stderrMessage("INFO","Parsing tidal node factors and equilibrium arguments.");
   while(<TIDEFAC>) {
      my @constituent = split;
      if ( $constituent[0] eq "M2" ) {
         $m2nf = $constituent[1];
         $m2eqarg = $constituent[2];
      } elsif ( $constituent[0] eq "S2" ) {
         $s2nf = $constituent[1];
         $s2eqarg = $constituent[2];
      } elsif  ( $constituent[0] eq "N2" ) {
         $n2nf = $constituent[1];
         $n2eqarg = $constituent[2];
      } elsif ( $constituent[0] eq "K2" ) {
         $k2nf = $constituent[1];
         $k2eqarg = $constituent[2];
      } elsif ( $constituent[0] eq "K1" ) {
         $k1nf = $constituent[1];
         $k1eqarg = $constituent[2];
      } elsif ( $constituent[0] eq "O1" ) {
         $o1nf = $constituent[1];
         $o1eqarg = $constituent[2];
      } elsif ( $constituent[0] eq "P1" ) {
         $p1nf = $constituent[1];
         $p1eqarg = $constituent[2];
      } elsif ( $constituent[0] eq "Q1" ) {
         $q1nf = $constituent[1];
         $q1eqarg = $constituent[2];
      } else {
         ASGSUtil::stderrMessage("WARNING","Tidal constituent named '$constituent[0]' was unrecognized.");
      }
   }
   close(TIDEFAC);
}
#
# load up stations
$numelevstations = getStations($elevstations,"elevation");
$numvelstations = getStations($velstations,"velocity");
if ( $nws eq "0" ) {
   ASGSUtil::stderrMessage("INFO","NWS is zero; meteorological stations will not be written to the fort.15 file.");
   $nummetstations = "NO LINE HERE";
} else {
   $nummetstations = getStations($metstations,"meteorology");
}
#
# load up the periodicflux data
$fluxdata = getPeriodicFlux($periodicflux);
#
# construct metControl namelist line
# &metControl WindDragLimit=floatValue, DragLawString='stringValue', rhoAir=floatValue, outputWindDrag=logicalValue /
my $outputWindDrag = $p->{metControl}->{outputWindDrag} eq "yes" ? "T" : "F";
my $met_control_line ="&metControl WindDragLimit=$p->{metControl}->{WindDragLimit}, DragLawString=\"$p->{metControl}->{DragLawString}\", outputWindDrag=$outputWindDrag /";
#
# construct wetDryControl namelist
# &wetDryControl outputNodeCode=logicalValue, outputNOFF=logicalValue, noffActive=logicalValue /
my $outputNodeCode = $p->{wetDryControl}->{outputNodeCode} eq 'yes' ? 'T' : 'F';
my $outputNOFF = $p->{wetDryControl}->{outputNOFF} eq 'yes' ? 'T' : 'F';
my $noffActive = $p->{wetDryControl}->{noffActive} eq 'on' ? 'T' : 'F';
my $wetdry_control_line = "&wetDryControl outputNodeCode=$outputNodeCode, outputNOFF=$outputNOFF, noffActive=$noffActive /";
#
# construct inundationOutput namelist
# &inundationOutputControl inundationOutput=logicalValue0, inunThresh=floatValue /
my $inundationOutput = $p->{inundationOutputControl}->{inundationOutput} eq 'yes' ? 'T' : 'F';
my $inundation_output_control_line = "&inundationOutputControl inundationOutput=$inundationOutput, inunThresh=$p->{inundationOutputControl}->{inunThresh} /";
#
my $dynamic_water_level_correction_line = 'NO LINE HERE';
#
# LINTER: check for consistency between solver time integration
#         type and time weighting coefficients
# "explict"                   -> "0.00 1.0 0.00"
# "implicit"                  -> "0.35 0.3 0.35"
# "full-gravity-wave-implicit"-> "0.50 0.5 0.00" ! or
#                             -> "0.80 0.2 0.00" ! alternate
# LINTER: Check that (A00+B00+C00)==1.0 in all cases
# LINTER: Check that C00==0.0 if time integration is "full-gravity-wave-implicit" or "explicit"
# LINTER: Check that C00!=0.0 if time integration is "implicit"
# LINTER: Check that A00==0.0 if time integration is "explicit"
#
# time weighting coefficients
my $a00b00c00 = "$p->{time_weighting_coefficients}";
my @tw = split(" ",$p->{time_weighting_coefficients}); # grab original coefficients
#
# specify iterative solver by default (required for implicit time integration)
my $ititer=1;
#
# the first digit of IM specifies both 2D/3D and lateral turbulence representation
my $IMDig1 = "1"; # 2D with constant lateral eddy viscosity
if ( $p->{lateral_turbulence} eq "smagorinsky" ) {
   $IMDig1 = "5";
}
# the sixth digit of IM specifies the time integration formulation
my $IMDig6 = "1";    # implicit time integration
if ( $p->{solver_time_integration} eq "explicit" ) {
   $IMDig6 = "2";
   $ititer = -1;  # explicit "solver"
   $a00b00c00 = "0.00 1.0 0.00"; # overwrite the values specified if necessary
   # check to see if the original was not 0 1 0 and issue warning if so
   if ($tw[0] != 0.0 || $tw[1] != 1.0 || $tw[2] != 0.0 ) {
      ASGSUtil::stderrMessage("WARNING","The time weighting coefficients A00 B00 C00 were reset to 0.0 1.0 0.0 for explicit time integration, replacing the specified time weighting coefficients of $p->{time_weighting_coefficients}");
   }
}
if ( $p->{solver_time_integration} eq "full-gravity-wave-implicit" ) {
   $IMDig6 = "3";
   if ( $tw[2] != 0.0 ) {
      $a00b00c00 = "0.50 0.5 0.00"; # overwrite the values specified with something reasonable
      ASGSUtil::stderrMessage("WARNING","The time weighting coefficients A00 B00 C00 were reset to 0.5 0.5 0.0 for full gravity wave implicit time integration, replacing the specified time weighting coefficients of $p->{time_weighting_coefficients}");
   }
}
my $im="$IMDig1"."1111"."$IMDig6";
#
# lateral turbulence formulation
my $eslm = $p->{eddy_viscosity_coefficient};  # this is the default
if ( $p->{lateral_turbulence} eq "smagorinsky" ) {
   $eslm = -$p->{smagorinsky_coefficient};    # must be set to negative in fort.15
}
#
# advection terms in control file (fort.15), does not affect nodal attributes
my $nolica = 1;
my $nolicat = 1;
if ( $p->{advection} eq "off" ) {
   $nolica = 0;
   $nolicat = 0;
}
#
# nodal_attribute_activate:
#   - "sea_surface_height_above_geoid"
#   - "mannings_n_at_sea_floor"
#
# count the number of activated nodal attributes and form the
# associated list of nodal attributes
my @nodal_attributes_activate = @{$p->{nodal_attributes}->{activate}};
my $nwp = scalar @nodal_attributes_activate;
#
ASGSUtil::stderrMessage("INFO","Filling in ADCIRC control template (fort.15).");
while(<TEMPLATE>) {
    # fill in the ADCIRC version
    s/%ADCIRCVER%/$p->{adcirc_version}/;
    # if we are looking at the first line, fill in the name of the storm
    # and the advisory number, if available
    s/%StormName%/$rundesc/;
    # fill in frequency of time step output to STDOUT or adcirc.log
    s/%NSCREEN%/$nscreen/;
    # non-fatal override (water levels for warnings and fatal errors)
    s/%NFOVER%/$p->{nfover}/;
    # logging levels (debug, echo, info, warning, error)
    s/%NABOUT%/$logLevelsNABOUT{$p->{log_level}}/;
    # set six digit IM according to time integration
    # IM=0 is the same as IM=111111
    s/%IM%/$im/;
    # set time weighting coefficients
    s/%A00B00C00%/$a00b00c00/;
    # if we are looking at the DT line, fill in the time step (seconds)
    s/%DT%/$dt/;
    # if we are looking at the RNDAY line, fill in the total run time (days)
    s/%RNDAY%/$RNDAY/;
    # whether or not to read a hotstart file
    s/%IHOT%/$ihot/;
    # advection terms, on or off
    s/%NOLICA%/$nolica/;
    s/%NOLICAT%/$nolicat/;
    # meteorological forcing type
    s/%NWS%/$nws/;
    # number of nodal attributes
    s/%NWP%/$nwp/;
    # list of nodal attributes
    if ( /%NALIST%/ ) {
       if ( $nwp == 0 ) {
          s/%NALIST%/NO LINE HERE/;
       } else {
          foreach my $nali (@nodal_attributes_activate) {
            printf "$nali\n";
          }
          next;
       }
    }
    # lateral turbulence formulation
    s/%ESLM%/$eslm/;
    # minimum water depth to be categorized wet vs dry
    s/%H0%/$p->{h0}/;
    # minimum pseudovelocity from wet to dry to change state
    s/%VELMIN%/$p->{velmin}/;
    # bottom friction lower limit
    s/%FFACTOR%/$p->{bottom_friction_limit}/;
    # number of forcing frequencies on periodic flux boundaries
    s/%NFFR%/$nffr/;
    # fill in nodal factors and equilibrium arguments
    if ( $tides eq "on" ) {
       s/%M2NF%/$m2nf/; s/%M2EQARG%/$m2eqarg/;
       s/%S2NF%/$s2nf/; s/%S2EQARG%/$s2eqarg/;
       s/%N2NF%/$n2nf/; s/%N2EQARG%/$n2eqarg/;
       s/%K2NF%/$k2nf/; s/%K2EQARG%/$k2eqarg/;
       s/%K1NF%/$k1nf/; s/%K1EQARG%/$k1eqarg/;
       s/%O1NF%/$o1nf/; s/%O1EQARG%/$o1eqarg/;
       s/%P1NF%/$p1nf/; s/%P1EQARG%/$p1eqarg/;
       s/%Q1NF%/$q1nf/; s/%Q1EQARG%/$q1eqarg/;
    }
    # fill in the timestep increment that hotstart files will be written at
    s/%NHSINC%/$NHSINC/;
    # fill in whether or not we want a hotstart file out of this
    s/%NHSTAR%/$NHSTAR/;
    # fill in scenario name -- this is in the comment line
    s/%ScenarioID%/$scenarioid/;
    s/%EnsembleID%/$scenarioid/; # for backward compatibility with old templates
    # may be asymmetric parameters, or wtiminc, rstiminc, etc
    s/%WTIMINC%/$wtiminc_line/;
    # periodic non-zero inflow
    s/%PERIODICFLUX%/$fluxdata/;
    # elevation stations
    s/%NUMELEVSTATIONS%/$numelevstations/;
    # velocity stations
    s/%NUMVELSTATIONS%/$numvelstations/;
    # meteorological stations
    s/%NUMMETSTATIONS%/$nummetstations/;
    # output options
    s/%FORT61%/$fort61/;
    s/%FORT62%/$fort62/;
    s/%FORT63%/$fort63/;
    s/%FORT64%/$fort64/;
    s/%FORT7172%/$fort7172/;
    s/%FORT7374%/$fort7374/;
    # iterative solver specification
    s/%ITITER%/$ititer/;
    # netcdf metadata
    s/%NCPROJ%/$p->{netcdf_metadata}->{NCPROJ}/;
    s/%NCINST%/$p->{netcdf_metadata}->{NCINST}/;
    s/%NCSOUR%/$p->{netcdf_metadata}->{NCSOUR}/;
    s/%NCHIST%/$p->{netcdf_metadata}->{NCHIST}/;
    s/%NCREF%/$p->{netcdf_metadata}->{NCREF}/;
    s/%NCCOM%/$p->{netcdf_metadata}->{NCCOM}/;
    s/%NCHOST%/$p->{netcdf_metadata}->{NCHOST}/;
    s/%NCCONV%/$p->{netcdf_metadata}->{NCCONV}/;
    s/%NCCONT%/$p->{netcdf_metadata}->{NCCONT}/;
    s/%NCDATE%/$p->{netcdf_metadata}->{NCDATE}/;
    # coldstart date by parts instead of NCDATE
    s/%CSYEAR%/$cy/;
    s/%CSMONTH%/$cm/;
    s/%CSDAY%/$cd/;
    s/%CSHOUR%/$ch/;
    s/%CSMIN%/$cmin/;
    s/%CSSEC%/$cs/;
    # remove line IDs from netCDF metadata so it doesn't
    # show up in output files
    s/%NCPROJ-Line%//;
    s/%NCINST-Line%//;
    s/%NCSOUR-Line%//;
    s/%NCHIST-Line%//;
    s/%NCREF-Line%//;
    s/%NCCOM-Line%//;
    s/%NCHOST-Line%//;
    s/%NCCONT-Line%//;
    s/%NCDATE-Line%//;
    # namelists
    s/%met_control_namelist%/$met_control_line/;
    s/%wetdry_control_namelist%/$wetdry_control_line/;
    s/%inundation_output_control_namelist%/$inundation_output_control_line/;
    s/%dynamic_water_level_correction_namelist%/$dynamic_water_level_correction_line/;
    #
    unless (/NO LINE HERE/) {
       print $_;
    }
}
close(TEMPLATE);
#
#
#  A D C I R C   N O D A L   A T T R I B U T E S   F I L E
#
my $nafi;
if (not open($nafi,"<","$p->{nodal_attributes}->{template}") ) {
   ASGSUtil::stderrMessage("ERROR","Failed to open '$p->{nodal_attributes}->{template}': $!.");
   die;
}
my $nafo;
if (not open($nafo,">","fort.13") ) {
   ASGSUtil::stderrMessage("ERROR","Failed to open 'fort.13': $!.");
   die;
}
#
# fill in nodal attribute default values from hash
while(<$nafi>) {
   foreach my $key (keys %{$p->{nodal_attributes}->{default_values}}) {
      my $tag = "%"."$key"."_default"."%";
      my $value = "$p->{nodal_attributes}->{default_values}->{$key}";
      s/$tag/$value/;
   }
   print $nafo $_;
}
#
close($nafi); # nodal attributes file template
close($nafo); # nodal attributes file (filled template)
#
#
#  S W A N   C O N T R O L   F I L E
#
if ( $p->{wave_coupling}->{waves} eq "on" && $p->{wave_coupling}->{wave_model} eq "swan" ) {
   # open swan template file for fort.26
   unless (open(TEMPLATE,"<$swantemplate")) {
      ASGSUtil::stderrMessage("ERROR","Failed to open the swan template file $swantemplate for reading: $!.");
      die;
   }
   #
   # open output fort.26 file
   unless (open(FORT26,">fort.26")) {
      ASGSUtil::stderrMessage("ERROR","Failed to open the swan parameters file 'fort.26': $!.");
      die;
   }
   ASGSUtil::stderrMessage("INFO","The 'fort.26' file will be written.");
   #
   $startdatetime = sprintf("%4d%02d%02d.%02d0000",$ny,$nm,$nd,$nh);
   $enddatetime = sprintf("%4d%02d%02d.%02d0000",$ey,$em,$ed,$eh);
   my $swanhs =  "INIT HOTSTART MULTIPLE 'swan.68'";
   if ( $hotswan eq "off" ) {
      $swanhs = "\$ swan will coldstart";
   }
   #
   ASGSUtil::stderrMessage("INFO","Filling in swan control template (fort.26).");
   while(<TEMPLATE>) {
       # use the year as the run number
       my $ny72 = substr($ny,0,4);                  # 'nr' in SWAN documentation, max 4 characters
       s/%nr%/$ny72/;
       # if we are looking at the first line, fill in the name of the storm
       # and the advisory number, if available
       my $rundesc72 = substr($rundesc,0,72);       # 'title1' in SWAN documentation, max 72 char
       s/%StormName%/$rundesc72/;
       # if we are looking at the DT line, fill in the time step (seconds)
       s/%swandt%/$swandt/;
       # fill in ensemble name -- this is in the comment line
       my $scenarioid72 = substr($scenarioid,0,72); # 'title2' in SWAN documentation, max 72 char
       s/%ScenarioID%|%EnsembleID%/$scenarioid72/;
       #s//$scenarioid72/; # for backward compatibility with old templates
       # fill in the ADCIRC version : jgfdebug does this need to be checked for length?
       s/%ADCIRCVER%/$p->{adcirc_version}/;
       # may be asymmetric parameters, or wtiminc, rstiminc, etc
       my $wtiminc_line_72 = substr($wtiminc_line,0,72);
       s/%WTIMINC%/$wtiminc_line_72/;                     # 'title3' in SWAN documentation, max 72 char
       #
       s/%hotstart%/$swanhs/;
       # swan start time -- corresponds to adcirc hot start time
       s/%startdatetime%/$startdatetime/;
       # swan end time%
       s/%enddatetime%/$enddatetime/;
       # swan max iterations
       s/%MXITNS%/$p->{swan}->{mxitns}/;
       # swan min percent of nodes meeting convergence criteria
       s/%NPNTS%/$p->{swan}->{npnts}/;
       print FORT26 $_;
   }
   close(TEMPLATE);
   close(FORT26);
}
#
# write run-control.properties file
# set components
my $model = "PADCIRC";
my $model_type = "SADC";
my $run_type = "Forecast"; # for the run-control.properties file
my $cycle_hour = sprintf("%02d",$nh);
my $currentdate = substr($ny,2,2) . sprintf("%02d%02d",$nm,$nd); # start time
my $date1 = sprintf("%4d%02d%02dT%02d%02d",$ny,$nm,$nd,$nh,$nmin); # start time
my $date2 = sprintf("%4d%02d%02dT%02d%02d",$ny,$nm,$nd,$nh,$nmin); # 1st output
my $date3 = sprintf("%4d%02d%02dT%02d%02d",$ey,$em,$ed,$eh,$emin); # end time
my $runstarttime = sprintf("%4d%02d%02d%02d",$ny,$nm,$nd,$nh); # start time
my $runendtime = sprintf("%4d%02d%02d%02d",$ey,$em,$ed,$eh); # end time
if ( $p->{wave_coupling}->{waves} eq "on" && $p->{wave_coupling}->{wave_model} eq "swan" ) {
   $model_type = "SPDS";
   $model = "PADCSWAN";
}
if ( abs($nws) == 12 || abs($nws) == 312 ) {
   $cycle_hour = sprintf("%02d",$oh);
   $currentdate = substr($oy,2,2) . sprintf("%02d%02d",$om,$od); # start time
   $date1 = sprintf("%4d%02d%02dT%02d%02d",$oy,$om,$od,$oh,$omin);
}
if ( $enstorm eq "nowcast" ) {
   $run_type = "Nowcast";  # for the run-control.properties file
} elsif ( $enstorm eq "hindcast" ) {
   $run_type = "Hindcast"; # for the run-control.properties file
}
ASGSUtil::stderrMessage("INFO","Opening run-control.properties file for writing.");
unless (open(RUNPROPS,">run-control.properties")) {
   ASGSUtil::stderrMessage("ERROR","Failed to open the run-control.properties file for appending: $!.");
   die;
}
# If we aren't using a vortex met model, we don't have a track
# file, but the CERA web app still needs to have values for these
# properties. In the case of a vortex met model, these values are
# filled in by the storm_track_gen.pl script.
if ( abs($nws) != 19 && abs($nws) != 319 && abs($nws) != 20 && abs($nws) != 320 ) {
   printf RUNPROPS "track_raw_dat : notrack\n";
   printf RUNPROPS "track_raw_fst : notrack\n";
   printf RUNPROPS "track_modified : notrack\n";
}
printf RUNPROPS "year : $ny\n";
printf RUNPROPS "mesh : $gridname\n";
printf RUNPROPS "RunType : $run_type\n";
printf RUNPROPS "ADCIRCgrid : $gridname\n";
printf RUNPROPS "currentcycle : $cycle_hour\n";
printf RUNPROPS "currentdate : $currentdate\n";
printf RUNPROPS "advisory : $advisorynum\n";
if (defined $hstime) {
   printf RUNPROPS "InitialHotStartTime : $hstime\n";
}
printf RUNPROPS "RunStartTime : $runstarttime\n";
printf RUNPROPS "RunEndTime : $runendtime\n";
printf RUNPROPS "ColdStartTime : $csdate\n";
#
printf RUNPROPS "Model : $model\n";
# write the names of the output files to the run-control.properties file
ASGSUtil::stderrMessage("INFO","Writing file names and formats to run-control.properties file.");
writeFileName("fort.61",$fort61specifier);
writeFileName("fort.62",$fort62specifier);
writeFileName("fort.63",$fort63specifier);
writeFileName("fort.64",$fort64specifier);
writeFileName("fort.71",$fort7172specifier);
writeFileName("fort.72",$fort7172specifier);
writeFileName("fort.73",$fort7374specifier);
writeFileName("fort.74",$fort7374specifier);
writeFileName("maxele.63",$fort63specifier);
writeFileName("maxvel.63",$fort64specifier);
writeFileName("maxwvel.63",$fort7374specifier);
writeFileName("minpr.63",$fort7374specifier);
if ( $p->{wave_coupling}->{waves} eq "on" && $p->{wave_coupling}->{wave_model} eq "swan" ) {
   writeFileName("maxrs.63",$fort7374specifier);
   writeFileName("swan_DIR.63",$fort7374specifier);
   writeFileName("swan_DIR_max.63",$fort7374specifier);
   writeFileName("swan_HS.63",$fort7374specifier);
   writeFileName("swan_HS_max.63",$fort7374specifier);
   writeFileName("swan_TMM10.63",$fort7374specifier);
   writeFileName("swan_TMM10_max.63",$fort7374specifier);
   writeFileName("swan_TPS.63",$fort7374specifier);
   writeFileName("swan_TPS_max.63",$fort7374specifier);
}
if ($p->{inundationOutputControl}->{inundationOutput} eq "yes" ) {
   writeFileName("initiallydry.63",$fort63specifier);
   writeFileName("inundationtime.63",$fort63specifier);
   writeFileName("maxinundepth.63",$fort63specifier);
   writeFileName("everdried.63",$fort63specifier);
   writeFileName("endrisinginun.63",$fort63specifier);
}
close(RUNPROPS);
ASGSUtil::stderrMessage("INFO","Wrote run-control.properties file 'run-control.properties'.");
exit;
#
#
#--------------------------------------------------------------------------
#   S U B   W R I T E   F I L E   N A M E
#
# If an output file will be available, its name and type is written
# to the run-control.properties file.
#--------------------------------------------------------------------------
sub writeFileName {
   my $identifier = shift;
   my $specifier = shift;
   #
   my $format = "ascii"; # default output file format
   my $f = $identifier; # default (ascii) name of output file
   #
   # if there won't be any output of this type, just return without
   # writing anything to the run-control.properties file
   if ( $specifier == 0 ) {
      return;
   }
   # create the hash for relating the basic file identifier with the
   # long winded file type description
   my %ids_descs;
   $ids_descs{"fort.61"} = "Water Surface Elevation Stations";
   $ids_descs{"fort.62"} = "Water Current Velocity Stations";
   $ids_descs{"fort.63"} = "Water Surface Elevation";
   $ids_descs{"fort.64"} = "Water Current Velocity";
   $ids_descs{"fort.71"} = "Barometric Pressure Stations";
   $ids_descs{"fort.72"} = "Wind Velocity Stations";
   $ids_descs{"fort.73"} = "Barometric Pressure";
   $ids_descs{"fort.74"} = "Wind Velocity";
   $ids_descs{"maxele.63"} = "Maximum Water Surface Elevation";
   $ids_descs{"maxvel.63"} = "Maximum Current Speed";
   $ids_descs{"maxwvel.63"} = "Maximum Wind Speed";
   $ids_descs{"minpr.63"} = "Minimum Barometric Pressure";
   $ids_descs{"maxrs.63"} = "Maximum Wave Radiation Stress";
   $ids_descs{"swan_DIR.63"} = "Mean Wave Direction";
   $ids_descs{"swan_DIR_max.63"} = "Maximum Mean Wave Direction";
   $ids_descs{"swan_HS.63"} = "Significant Wave Height";
   $ids_descs{"swan_HS_max.63"} = "Maximum Significant Wave Height";
   $ids_descs{"swan_TMM10.63"} = "Mean Wave Period";
   $ids_descs{"swan_TMM10_max.63"} = "Maximum Mean Wave Period";
   $ids_descs{"swan_TPS.63"} = "Peak Wave Period";
   $ids_descs{"swan_TPS_max.63"} = "Maximum Peak Wave Period";
   $ids_descs{"initiallydry.63"} = "Initially Dry";
   $ids_descs{"inundationtime.63"} = "Inundation Time";
   $ids_descs{"maxinundepth.63"} = "Maximum Inundation Depth";
   $ids_descs{"everdried.63"} = "Ever Dried";
   $ids_descs{"endrisinginun.63"} = "End Rising Inundation";

   # number of data sets
   if ( $f eq "fort.61") { $nds = $addHours/($fort61freq/3600.0); }
   elsif ( $f eq "fort.62") { $nds = $addHours/($fort62freq/3600.0); }
   elsif ( $f eq "fort.63") { $nds = $addHours/($fort63freq/3600.0); }
   elsif ( $f eq "fort.64") { $nds = $addHours/($fort64freq/3600.0); }
   elsif ( $f eq "fort.71" || $f eq "fort.72" ) {
      $nds = $addHours/($fort7172freq/3600.0);
   }
   elsif ( $f eq "fort.73" || $f eq "fort.74"
        || $f eq "swan_DIR.63" || $f eq "swan_HS.63" || $f eq "swan_TMM10.63" || $f eq "swan_TPS.63" ) {
      $nds = $addHours/($fort7374freq/3600.0);
   }
   else {
      $nds = 1;
   }

   # format specifier
   if ( abs($specifier) == 3 || abs($specifier) == 5 ) {
      $f = $f . ".nc";
      $format = "netcdf";
   }
   if ( abs($specifier) == 4 ) {
      $format = "sparse-ascii";
   }
   printf RUNPROPS "$ids_descs{$identifier} File Name : $f\n";
   printf RUNPROPS "$ids_descs{$identifier} Format : $format\n";
   printf RUNPROPS "adcirc.file.output.$f.numdatasets : $nds\n";
}
#
#
#--------------------------------------------------------------------------
#   S U B   G E T   S P E C I F I E R
#
# Determines the correct output specifier for output files based on
# the output frequency, whether or not the files should be appended,
# and whether or not the netcdf format is used (ascii is the default).
#--------------------------------------------------------------------------
sub getSpecifier {
   my $freq = shift;
   my $append = shift;
   my $netcdf = shift;
   my $specifier;

   if ( $freq == 0 ) {
      $specifier = "0";
   } else {
      if ( defined $append ) {
         $specifier = "1";
      } else {
         $specifier = "-1";
      }
      if ( defined $netcdf ) {
         if ( defined $netcdf4 ) {
            $specifier *= 5;
         } else {
            $specifier *= 3;
         }
      }
   }
   return $specifier;
}
#
#--------------------------------------------------------------------------
#   S U B   G E T   I N C R E M E N T
#
# Determines the correct time step increment based on the output frequency
# and time step size.
#--------------------------------------------------------------------------
sub getIncrement {
   my $freq = shift;
   my $timestepsize = shift;
   my $increment;
   if ( $freq == 0 ) {
      $increment = "99999";
   } else {
      $increment = int($freq/$timestepsize);
   }
   return $increment;
}
#
#--------------------------------------------------------------------------
#   S U B   G E T   S T A T I O N S
#
# Pulls in the stations from an external file.
#--------------------------------------------------------------------------
sub getStations {
   my $station_file = shift;
   my $station_type = shift;
#
   my $numstations = "";
   my $station_var = "NSTAE";
   if ( $station_type eq "velocity" ) {
      $station_var = "NSTAV";
   }
   if ( $station_type eq "meteorology" ) {
      $station_var = "NSTAM";
   }
   if ( $station_file =~ /null/) {
      $numstations = "0   ! $station_var" ;
      ASGSUtil::stderrMessage("INFO","There are no $station_type stations.");
      return $numstations; # early return
   }
   unless (open(STATIONS,"<$station_file")) {
      ASGSUtil::stderrMessage("ERROR","Failed to open the $station_type stations file $station_file for reading: $!.");
      die;
   }
   my $number=0;
   while (<STATIONS>) {
      $_ =~ s/^\s+//;
      next if (substr($_,0,1) eq '#');  # skip comment lines in the stations file
      next if ($_ eq '');               # skip empty lines in the stations file
      $numstations.=$_;
      $number++;
   }
   close(STATIONS);
   ASGSUtil::stderrMessage("INFO","There are $number $station_type stations in the file '$station_file'.");
   chomp($numstations);
   # need to add this as a sort of comment in the fort.15 for the post
   # processing script station_transpose.pl to find
   if ( $number != 0 ) {
      $numstations = $number . " " . $station_file . " " . $station_var . "\n" . $numstations;
   } else {
      $numstations = $number . " " . $station_file . " " . $station_var . " (file contains no stations)";
   }
   return $numstations;
}
#
#-------------------------------------------------------------------------
#   S U B    G E T  P E R I O D I C  F L U X
#
#  gets data for periodic non-zero inflow boundaries from a separate file
#  for example, a file generated by the riverFlow.pl script during
#  model configuration.
#-------------------------------------------------------------------------
sub getPeriodicFlux {
   my $flux_file=shift;
   if ($flux_file =~ /null/){
      ASGSUtil::stderrMessage("INFO","No periodic inflow boundary data file was specified/");
      return
   }
   unless (open(FLUXFILE,"<$flux_file")) {
      ASGSUtil::stderrMessage("ERROR","Failed to open $flux_file for reading: $!.");
      die;
   }
   my $fluxdata='';
   while (<FLUXFILE>){
       $fluxdata.=$_;
   }
   close(FLUXFILE);
   ASGSUtil::stderrMessage("INFO","Inserting periodic inflow boundary data from $flux_file.");
   chomp $fluxdata;
   return $fluxdata;
}
#
#
#--------------------------------------------------------------------------
#   S U B    I N I T I A L I Z A T I O N   P A R A M E T E R S
#
# Determines parameter values for the control file when running
# ADCIRC during initialization with no met forcing.
#--------------------------------------------------------------------------
sub initializationParameters {
   $rundesc = "cs:$csdate"."0000 cy:initialization";
   $RNDAY = $p->{initialization_length};
   $addHours = $RNDAY*24.0;  # used to calculate number of datasets in files
   $NHSINC = int(($RNDAY*86400.0)/$dt);
    ($ey,$em,$ed,$eh,$emin,$es) =
       Date::Calc::Add_Delta_DHMS($cy,$cm,$cd,$ch,$cmin,$cs,$RNDAY,0,0,0);
   $nws = 0;
   $scenarioid = "$RNDAY day initialization run";
   $wtiminc_line = "NO LINE HERE";
   ASGSUtil::stderrMessage("DEBUG","Finished setting model initialization parameters.");
}
#
#--------------------------------------------------------------------------
#   S U B    C U S T O M   P A R A M E T E R S
#
# Determines parameter values for the control file when running
# ADCIRC without external forcing (e.g., for running ADCIRC tests).
#--------------------------------------------------------------------------
sub customParameters {
    $rundesc = "cs:$csdate"."0000 cy:custom";
    my $alreadyElapsedDays = 0.0;
    if ( defined $hstime && $hstime != 0 ) {
       $alreadyElapsedDays = $hstime / 86400.0;
    }
    $RNDAY = $specifiedRunLength + $alreadyElapsedDays;
    $NHSINC = int(($RNDAY*86400.0)/$dt);
    $nws = 0;
    $scenarioid = "$enstorm $specifiedRunLength day run";
       #
   # determine the relationship between the start of the NAM data and the
   # current time in the ADCIRC run
   if ( defined $hstime && $hstime != 0 ) {
      # now add the hotstart seconds
      ($ny,$nm,$nd,$nh,$nmin,$ns) =
         Date::Calc::Add_Delta_DHMS($cy,$cm,$cd,$ch,0,0,0,0,0,$hstime);
   } else {
      # the hotstart time was not provided, or it was provided and is equal to 0
      # therefore the current ADCIRC time is the cold start time, t=0
      $ny = $cy;
      $nm = $cm;
      $nd = $cd;
      $nh = $ch;
      $nmin = 0;
      $ns = 0;
   }
   ($ey,$em,$ed,$eh,$emin,$es) =
      Date::Calc::Add_Delta_DHMS($ny,$nm,$nd,$nh,0,0,$specifiedRunLength,0,0,0);
   $wtiminc_line = "NO LINE HERE";
   ASGSUtil::stderrMessage("DEBUG","Finished setting specified run length.");
}

#
#--------------------------------------------------------------------------
#   S U B   O W I  P A R A M E T E R S
#
# Determines parameter values for the control file when running
# ADCIRC with OWI formatted meteorological data (NWS12).
#--------------------------------------------------------------------------
sub owiParameters {
   #
   # determine the relationship between the start of the NAM data and the
   # current time in the ADCIRC hotstart file
   if ( defined $hstime && $hstime != 0 ) {
      # now add the hotstart seconds
      ($ny,$nm,$nd,$nh,$nmin,$ns) =
         Date::Calc::Add_Delta_DHMS($cy,$cm,$cd,$ch,0,0,0,0,0,$hstime);
   } else {
      # the hotstart time was not provided, or it was provided and is equal to 0
      # therefore the current ADCIRC time is the cold start time, t=0
      $ny = $cy;
      $nm = $cm;
      $nd = $cd;
      $nh = $ch;
      $nmin = 0;
      $ns = 0;
   }
   # date time of the start and end of the OWI file(s)
   my $owistart = $p->{meteorology}->{owi_win_pre}->{startdatetime};
   my $owiend = $p->{meteorology}->{owi_win_pre}->{enddatetime};
   # create run description
   $rundesc = "cs:$csdate"."0000 cy:$owistart end:$owiend OWI ASCII ";
   print "cs:'$csdate' cy:'$owistart'";
   # compute RNDAY and NHSINC
   $owiend =~ m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
   $ey = $1;
   $em = $2;
   $ed = $3;
   $eh = $4;
   $emin = 0;
   $es = 0;
   # get difference
   ( my $ddays, my $dhrs, my $dmin, my $dsec )
           = Date::Calc::Delta_DHMS(
                $cy,$cm,$cd,$ch,0,0,
                $ey,$em,$ed,$eh,0,0);
   # find the new total run length in days
   $RNDAY = $ddays + $dhrs/24.0 + $dmin/1440.0 + $dsec/86400.0;
   # determine the number of hours of this run, from hotstart to end
   ( $ddays, $dhrs, $dmin, $dsec)
           = Date::Calc::Delta_DHMS(
                $ny,$nm,$nd,$nh,0,0,
                $ey,$em,$ed,$eh,0,0);
   $addHours = $ddays*24.0 + $dhrs + $dmin/60.0 + $dsec/3600.0;
   $scenarioid = $addHours . " hour " . $enstorm . " run";
   $NHSINC = int(($RNDAY*86400.0)/$dt);
}
#
#--------------------------------------------------------------------------
#   S U B   V O R T E X   M O D E L   P A R A M E T E R S
#
# Determines parameter values for the control file when running
# the with tropical cyclone vortex models (asymmetric, nws=19; or
# generalized asymmetric holland model, nws=20).
#--------------------------------------------------------------------------
sub vortexModelParameters {
   my $nws = shift;
   my $geofactor = 1; # turns on Coriolis for GAHM; this is the default
   #
   # convert hotstart time (in days since coldstart) if necessary
   if ( $hstime ) {
      $hstime_days = $hstime/86400.0;
   }
   # get end time
   my $end = $endtime; # yyyymmddhh
   ASGSUtil::stderrMessage("INFO","New $enstorm time is $end.");
   if ( defined $hstime && $hstime != 0 ) {
      # now add the hotstart seconds
      ($ny,$nm,$nd,$nh,$nmin,$ns) =
         Date::Calc::Add_Delta_DHMS($cy,$cm,$cd,$ch,$cmin,$cs,0,0,0,$hstime);
   } else {
      # the hotstart time was not provided, or it was provided and is equal to 0
      # therefore the current ADCIRC time is the cold start time, t=0
      $ny = $cy;
      $nm = $cm;
      $nd = $cd;
      $nh = $ch;
      $nmin = 0;
      $ns = 0;
   }
   #
   $end =~ m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
   $ey = $1;
   $em = $2;
   $ed = $3;
   $eh = $4;
   $emin = 0.0;
   $es = 0.0;
   #
   # get total difference btw cold start time and end time ... this is RNDAY
   my ($days,$hours,$minutes,$seconds)
      = Date::Calc::Delta_DHMS(
         $cy,$cm,$cd,$ch,$cmin,$cs,
         $ey,$em,$ed,$eh,$emin,$es);
   # RNDAY is diff btw cold start time and end time
   # For a forecast, RNDAY is one time step short of the total time to ensure
   # that we won't run out of storm data at the end of the fort.22
   # For a nowcast, RNDAY will be one time step long, so that we end at
   # the nowcast time, even if ADCIRC rounds down the number of timesteps
   # jgf20110629: Lets see if we can get the nowcast to stop at the exact time
   # that we want
   $RNDAY = $days + $hours/24.0 + $minutes/1440.0 + $seconds/86400.0;
   my $RNDAY_orig = $RNDAY;

   # If RNDAY is less than two timesteps, make sure it is at least
   #  two timesteps.
   # This can happen if we start up from a fort.22 that has only one BEST line,
   # i.e., it starts at the nowcast. RNDAY would be zero in this case, except
   # our algorithm actually stops one ts short of the full time, so RNDAY is
   # actually negative in this case. ADCIRC needs at least two timesteps from
   # coldstart to create a valid hotstart file.
   my $runlength_seconds = $RNDAY*86400.0;
   if ( $hstime ) {
      $runlength_seconds-=$hstime;
   }
   my $min_runlength = 2*$dt;
   # if we coldstart at the nowcast, we may not have calculated a runlength
   # longer than the minimum
   my $goodRunlength = 1;
   if ( $runlength_seconds < $min_runlength ) {
      if ( $enstorm eq "nowcast" ) {
         $goodRunlength = 0;
      }
      ASGSUtil::stderrMessage("INFO","Runlength was calculated as $runlength_seconds seconds, which is less than the minimum runlength of $min_runlength seconds. The RNDAY will be adjusted so that it ADCIRC runs for the minimum length of simulation time.");
      # recalculate the RNDAY as the hotstart time plus the minimal runlength
      if ( $hstime ) {
         $RNDAY=$hstime_days + ($min_runlength/86400.0);
      } else {
         $RNDAY=$min_runlength/86400.0;
      }
      $runlength_seconds = $min_runlength;
   }
   #
   # if this is an update from model initialization to nowcast, calculate the hotstart
   # increment so that we only write a single hotstart file at the end of
   # the run. If this is a forecast, don't write a hotstart file at all.
   $NHSINC = int(($RNDAY*86400.0)/$dt);
   #
   # If we have swan coupling, we may need to add some run time, after
   # the adcirc hotstart file was written, to give swan a chance to write
   # its hotstart file. After adcirc has written its hotstart file,
   # swan has to run its time own time step, and then write
   # the swan hotstart file.
   if ( $p->{wave_coupling}->{waves} eq "on" ) {
      my $total_time = $RNDAY*86400.0; # in seconds
      # unusual but possible for the total run time to be less than the swan
      # time step
      if ( $total_time < $swandt ) {
         $total_time = $swandt; # run for at least one swan time step
         $RNDAY = $total_time / 86400.0; # convert to days
         $NHSINC = int(($RNDAY*86400.0)/$dt);
      }
   }
   # check to see if the RNDAY had to be modified from the value calculated
   # purely from the met file ... if so, modify the ending time accordingly
   if ( $RNDAY != $RNDAY_orig ) {
      ($ey,$em,$ed,$eh,$emin,$es) =
       Date::Calc::Add_Delta_DHMS($cy,$cm,$cd,$ch,$cmin,$cs,$RNDAY,0,0,0);
   }
   $addHours=$RNDAY*24.0; # for reporting the predicted number of datasets in each file
   if ( $hstime ) {
      $addHours-=$hstime/3600.0;
   }
   #
   # create run description
   $rundesc = "cs:$csdate"."0000 cy:$p->{meteorology}->{tropical_cyclone}->{storm_name}$advisorynum";
   # create the RUNID
   $scenarioid = $addHours . " hour " . $enstorm . " run";
   # create the WTIMINC line
   $wtiminc_line = $cy." ".$cm." ".$cd." ".$ch." 1 ".$bladj;
   if ( abs($nws) == 20 || abs($nws) == 320 || abs($nws) == 30 || abs($nws) == 330 ) {
      $wtiminc_line .= " $geofactor";
   }
}


