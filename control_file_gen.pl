#!/usr/bin/env perl
#--------------------------------------------------------------------------
# control_file_gen.pl : generate fort.15 file and write to stdout based on
# a specification provided via a yaml file on stdin
#--------------------------------------------------------------------------
# Copyright(C) 2006--2025 Jason Fleming
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
# The following table maps between the name of an adcirc version in ASGS
# and the output from "adcirc -v"
#   ASGS Name    |    adcirc -v
# ---------------+----------------
# v53release     | v53.05-modified
# v55.01-5bc04d6 | v55.00-20-g5bc04d6
# v55.02         | v55.02
# v56.0.2        | v56.0.2
#--------------------------------------------------------------------------
use strict;
use warnings;
use YAML::Tiny qw(Load);
use Date::Calc;
use ASGSUtil;
#
my %logLevelsNABOUT = ('DEBUG' => -1, 'ECHO' => 0, 'INFO' => 1, 'WARNING' => 2, 'ERROR' => 3 );
#
my $nafile="fort.13";
our ($cy, $cm, $cd, $ch, $cmin, $cs); # ADCIRC cold start time
our ($ny, $nm, $nd, $nh, $nmin, $ns); # current ADCIRC time
our ($ey, $em, $ed, $eh, $emin, $es); # ADCIRC end time
our ($oy, $om, $od, $oh, $omin, $os); # OWI start time
my $numelevstations="-99"; # number and list of adcirc elevation stations
my $numvelstations="-99";  # number and list of adcirc velocity stations
my $nummetstations="-99";  # number and list of adcirc meteorological stations
my $startdatetime; # formatted for swan fort.26
my $enddatetime;   # formatted for swan fort.26
my $hstime_days; # time, in days, of hotstart file (since coldstart)
our $dt=-99;      # adcirc time step, in seconds
our $NHSINC;    # time step increment at which to write hot start files
our $NHSTAR;    # writing and format of ADCIRC hotstart output file
our $RNDAY;     # total run length from cold start, in days
my $ihot;       # whether or not ADCIRC should READ a hotstart file
our $wtiminc_line;   # parameters related to met and wave timing
our $rundesc;   # description of run, 1st line in fort.15
our $scenarioid; # run id, 2nd line in fort.15
our $specifiedRunLength; # time in days for run if there is no externally specified forcing
my ($m2nf, $s2nf, $n2nf, $k2nf, $k1nf, $o1nf, $p1nf, $q1nf); # nodal factors
my ($m2eqarg, $s2eqarg, $n2eqarg, $k2eqarg, $k1eqarg, $o1eqarg, $p1eqarg, $q1eqarg); # equilibrium arguments
# flux boundary conditions
my $fluxdata = "NO LINE HERE";  # data from the $periodic_flux file
my $nffr = "NO LINE HERE";      # for flux boundaries; -1: top of fort.20 corresponds to hs
#
our $addHours; # duration of the run (hours)
my $parameters;  # YAML document that defines model control options
#
# load YAML document containing model control parameters from stdin
$parameters = do { local $/; <> };
our $p = Load($parameters); # load parameters into the hashref p
#
# parse out the pieces of the cold start date
$p->{coldstartdate}=~ m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
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
#----------------------------------------------------
#
#  A D C I R C   C O N T R O L   F I L E
#
# open template file for fort.15
unless (open(TEMPLATE,"<$p->{controltemplate}")) {
   ASGSUtil::stderrMessage("ERROR","Failed to open the fort.15 template file '$p->{controltemplate}' for reading: $!.");
   die;
}
# ICS
my $ics = 2;
if ( $p->{coordinate_system}->{projection} eq "cartesian" ) {
   $ics = 1;
   # the only input projections for ADCIRC are cartesian and geographic; all of the below
   # assume the input projection is geographic (lon/lat)
} else {
   if ( $p->{coordinate_system}->{reprojection} eq "equal-area" ) {
      $ics = 20;
   } elsif ($p->{coordinate_system}->{reprojection} eq "CPP" && $p->{coordinate_system}->{earthCurvature} eq "no" ) {
      $ics = 2; # ICS=2 is the most common setting
   } elsif ($p->{coordinate_system}->{reprojection} eq "CPP" && $p->{coordinate_system}->{earthCurvature} eq "yes" ) {
      $ics = 21;
   } elsif ( $p->{coordinate_system}->{reprojection} eq "mercator" ) {
      $ics = 22;
   } elsif ( $p->{coordinate_system}->{reprojection} eq "miller" ) {
      $ics = 23;
   } elsif ( $p->{coordinate_system}->{reprojection} eq "gall-stereographic" ) {
      $ics = 24;
   } else {
      ASGSUtil::stderrMessage("ERROR","Coordinate reprojection '$p->{coordinate_system}->{reprojection}' was not recognized.");
   }
}
my $zNorth = "northpole";
if ( $ics >=20 && $ics <=24 && $p->{coordinate_system}->{rotation} ne "northpole" ) {
   if ( $p->{coordinate_system}->{rotation} eq "greenland-antarctica" ) {
      $zNorth = "-42.8906  72.3200  ! Greenland-Antarctica";
   } elsif ( $p->{coordinate_system}->{rotation} eq "china-argentina" ) {
      $zNorth = "112.8516  40.3289  ! China-Argentina";
   } elsif ( $p->{coordinate_system}->{rotation} eq "borneo-brazil" ) {
      $zNorth = "114.16991  0.77432 ! Borneo-Brazil";
   } else {
      ASGSUtil::stderrMessage("ERROR","Coordinate rotation '$p->{coordinate_system}->{rotation}' was not recognized.");
   }
}
if ( $zNorth ne "northpole" ) {
   $ics *= -1;
   my $rotm; # rotation file
   unless (open($rotm,">fort.rotm")) {
      ASGSUtil::stderrMessage("ERROR","Failed to open the coordinate rotation file fort.rotm for writing: $!.");
      die;
   }
   print $rotm "$zNorth";
   close($rotm);
}
#
my $nws = $p->{meteorology}->{nws};
my $basenws = $p->{meteorology}->{basenws};
my $thisNWS = $p->{meteorology}->{nws};
if ( $p->{output}->{inventory} eq "metonly" ) {
   $thisNWS = $p->{meteorology}->{basenws};
}
#
# set the model time step, depending on whether shallow water
# equations will be solved
$dt = $p->{timestepsize};
if ( $p->{output}->{inventory} eq "metonly" ) {
   $dt = $p->{output}->{metonly_dt};
}
#
# call subroutine that knows how to fill in the fort.15 for each particular
# type of forcing
if ( abs($basenws) == 19 || abs($basenws) == 20 || abs($basenws) == 8 || abs($basenws) == 30 ) {
   ASGSUtil::stderrMessage("DEBUG","Setting parameters appropriately for vortex model.");
   vortexModelParameters($nws);
}
# for getting the OWI wind time increment for blended winds
# and appending it to the wtiminc line
if ( abs($nws) == 308 || abs($nws) == 319 || abs($nws) == 320 ) {
      $wtiminc_line .= " $p->{wave_coupling}->{rstiminc}";
} elsif ( abs($nws) == 30 ) {
   $wtiminc_line .= " $p->{meteorology}->{wtiminc} $p->{meteorology}->{blending}->{pureVortex} $p->{meteorology}->{blending}->{pureBackground}";
} elsif ( abs($nws) == 330 ) {
   $wtiminc_line .= " $p->{meteorology}->{wtiminc} $p->{wave_coupling}->{rstiminc} $p->{meteorology}->{blending}->{pureVortex} $p->{meteorology}->{blending}->{pureBackground}";
}
#
# for straight OWI WIN/PRE ASCII files
if ( abs($nws) == 12 ) {
   owiParameters();
   $wtiminc_line = "$p->{meteorology}->{wtiminc}";
} elsif ( abs($nws) == 312 ) {
   owiParameters();
   $wtiminc_line = "$p->{meteorology}->{wtiminc} $p->{wave_coupling}->{rstiminc}";
}
#
# tidal initialization or "other"
if ( defined $specifiedRunLength ) {
   ASGSUtil::stderrMessage("DEBUG","The duration of this $enstorm run is specially defined.");
   customParameters();
} elsif ( $p->{scenario} eq "hindcast" ) {
   ASGSUtil::stderrMessage("DEBUG","This is a model initialization run.");
   initializationParameters();
}
#
# we want a hotstart file if this is a nowcast or model initialization
# and this is not a meteorology-only scenario/layer
$NHSTAR = 0;
$NHSINC = 99999;
if ( $p->{output}->{inventory} eq "full" ) {
   if ( $p->{scenario} eq "nowcast" || $p->{scenario} eq "hindcast" ) {
      $NHSTAR = 5;
      if ( $p->{hotstart}->{output_format} eq "netcdf3" ) {
         $NHSTAR = 3;
      }
      if ( $p->{hotstart}->{output_format} eq "binary" ) {
         $NHSTAR = 1;
      }
   }
}
#
# we always look for a fort.68 file, and since we only write one hotstart
# file during the run, we know we will always be left with a fort.67 file.
$ihot = 0;
if ( $p->{hotstart}->{time} != 0.0 ) {
   $ihot = 568; # netcdf4
   if ( $p->{hotstart}->{input_format} eq "netcdf3" ) {
      $ihot = 368;
   }
   if ( $p->{hotstart}->{input_format} eq "binary" ) {
      $ihot = 68;
   }
}
#
# [de]activate output files with time step increment and with(out) appending.
my $fort61 = getOutputParameters($p->{output}->{stations}->{fort61},$p->{output}->{inventory},$dt,"elev");
my $fort62 = getOutputParameters($p->{output}->{stations}->{fort62},$p->{output}->{inventory},$dt,"vel");
my $fort7172 = getOutputParameters($p->{output}->{stations}->{fort7172},$p->{output}->{inventory},$dt,"met");
my $fort63 = getOutputParameters($p->{output}->{fulldomain}->{fort63},$p->{output}->{inventory},$dt,"elev");
my $fort64 = getOutputParameters($p->{output}->{fulldomain}->{fort64},$p->{output}->{inventory},$dt,"vel");
my $fort7374 = getOutputParameters($p->{output}->{fulldomain}->{fort7374},$p->{output}->{inventory},$dt,"met");
#
# if the code should be run in meteorology-only mode, then turn
# off all output except meteorology (including hotstart output)
if ( $p->{output}->{inventory} eq "metonly" ) {
   $NHSTAR = "0";
   $NHSINC = "99999"
}
#
if ( $nws eq "0" ) {
   $fort7172 = "NO LINE HERE";
   $fort7374 = "NO LINE HERE";
}
#
# tides
if ( $p->{tides}->{tidal_forcing} eq "on" ) {
   # open data file
   unless (open(TIDEFAC,"<$p->{tides}->{tidefac_file}")) {
      ASGSUtil::stderrMessage("ERROR","Failed to open the file '$p->{tides}->{tidefac_file}' for reading: $!.");
      die;
   }
   # parse out nodal factors and equilibrium arguments from the
   # various constituents
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
$numelevstations = getStations($p->{output}->{stations}->{fort61}->{elevstations_file},"elevation");
$numvelstations = getStations($p->{output}->{stations}->{fort62}->{velstations_file},"velocity");
if ( $nws eq "0" ) {
   ASGSUtil::stderrMessage("INFO","NWS is zero; meteorological stations will not be written to the fort.15 file.");
   $nummetstations = "NO LINE HERE";
} else {
   $nummetstations = getStations($p->{output}->{stations}->{fort7172}->{metstations_file},"meteorology");
}
#
# load up the periodicflux data
if ( $p->{flux}->{periodicity} eq "periodic" ) {
   # use the name of a file containing the periodic flux unit discharge
   # data for constant inflow boundaries
   $fluxdata = getPeriodicFlux($p->{flux}->{file});
   $nffr = 1;
}
if ( $p->{flux}->{periodicity} eq "aperiodic") {
   if ( $ihot == 0 ) {
      $nffr = 0;
   } else {
      $nffr = -1;
   }
}
#
# construct &metControl namelist line %met_control_namelist%
# &metControl WindDragLimit=floatValue, DragLawString='stringValue', rhoAir=floatValue, outputWindDrag=logicalValue, invertedBarometerOnElevationBoundary=logicalValue /
# ADCIRC defaults for these namelist parameters, if they are not included in
# the namelist, are as follows:
# WindDragLimit = 0.0035d0    ! wind.F ! this seems way too high
# DragLawString = "garratt"   ! wind.F
# outputWindDrag = .false.    ! global.F
# rhoAir = 1.293D0            ! constants.F
# invertedBarometerOnElevationBoundary = .false. ! gwce.F
# nPowellSearchDomains = -1   ! owiwind.F (the -1 indicates that all domains should be searched for the minimum pressure)
#
my $writeMetControlLine = 0;  # only write this namelist if any specified value is not the default value
my $outputWindDrag = $p->{meteorology}->{wind_drag}->{outputWindDrag} eq "yes" ? "T" : "F";
my $invertedBarometerOnElevationBoundary = $p->{meteorology}->{invertedBarometerOnElevationBoundary} eq "yes" ? "T" : "F";
my $met_control_line ="&metControl \n";
if ( $p->{meteorology}->{wind_drag}->{WindDragLimit} != "0.0035" ) {
   $met_control_line   .="            WindDragLimit=$p->{meteorology}->{wind_drag}->{WindDragLimit}, \n";
   $writeMetControlLine = 1;
}
if ( $p->{meteorology}->{wind_drag}->{DragLawString} ne "garratt" ) {
   $met_control_line   .="            DragLawString=\"$p->{meteorology}->{wind_drag}->{DragLawString}\",\n";
   $writeMetControlLine = 1;
}
if ( $outputWindDrag eq "T" ) {
   $met_control_line   .="            outputWindDrag=$outputWindDrag,\n";
   $writeMetControlLine = 1;
}
if ( $p->{meteorology}->{rhoAir} != 1.293 ) {
   $met_control_line   .="            rhoAir=$p->{meteorology}->{rhoAir},\n";
   $writeMetControlLine = 1;
}
if ( $invertedBarometerOnElevationBoundary eq "T" ) {
   $met_control_line   .="            invertedBarometerOnElevationBoundary=$invertedBarometerOnElevationBoundary,\n";
   $writeMetControlLine = 1;
}
# nPowellSearch domains requires ADCIRC version v55relase or later
if ( $p->{adcirc_version} ne "v53.05-modified" ) {
   if ( $p->{meteorology}->{wind_drag}->{nPowellSearchDomains} != -1 ) {
      $met_control_line.="            nPowellSearchDomains=$p->{meteorology}->{wind_drag}->{nPowellSearchDomains},\n";
      $writeMetControlLine = 1;
   }
}
$met_control_line   .="            /\n";
if ( $writeMetControlLine == 0 ) {
   $met_control_line = "NO LINE HERE";
}
#
# construct &wetDryControl namelist %wetdry_control_namelist%
# &wetDryControl outputNodeCode=logicalValue, outputNOFF=logicalValue, noffActive=logicalValue
#        slim=floatValue, windlim=logicalValue, directvelWD=logicalValue, useHF=logicalValue /
# available in v53release (and later)
my $outputNodeCode = $p->{wetDryControl}->{outputNodeCode} eq 'yes' ? 'T' : 'F';
my $outputNOFF = $p->{wetDryControl}->{outputNOFF} eq 'yes' ? 'T' : 'F';
my $noffActive = $p->{wetDryControl}->{noffActive} eq 'on' ? 'T' : 'F';
# available in v55release and later; defaults if not set:
# StatPartWetFix = .false ! global.F
# How2FixStatPartWet = 0  ! global.F
my $StatPartWetFix = $p->{output}->{stations}->{wetdry}->{StatPartWetFix} eq 'on' ? 'T' : 'F';
my $How2FixStatPartWet = $p->{output}->{stations}->{wetdry}->{How2FixStatPartWet};  # integer
# available in v56.0.3; defaults if not set:
# slim = 1.de9            ! global.F ! Large value on slim effectively assures limiter is not applied anywhere.
# windlim = .false        ! global.F
# directvelWD = .false.   ! global.F
# useHF = .false.         ! global.F
my $slim = $p->{wetDryControl}->{windlim} eq 'on' ? 'T' : 'F';
my $windlim = $p->{wetDryControl}->{windlim} eq 'on' ? 'T' : 'F';
my $directvelWD = $p->{wetDryControl}->{directvelWD} eq 'on' ? 'T' : 'F';
my $useHF = $p->{wetDryControl}->{useHF} eq 'on' ? 'T' : 'F';
#
my $wetdry_control_line = "&wetDryControl \n";
# the following are available in any ADCIRC version supported by ASGS
$wetdry_control_line    .= "outputNodeCode=$outputNodeCode,\n";
$wetdry_control_line    .= "outputNOFF=$outputNOFF,\n";
$wetdry_control_line    .= "noffActive=$noffActive,\n";
# the following are availble in v55 and later
if ( $p->{adcirc_version} ne "v53.05-modified" ) {
   $wetdry_control_line .= "StatPartWetFix=$StatPartWetFix\n";
   $wetdry_control_line .= "How2FixStatPartWet=$How2FixStatPartWet\n";
}
# the following are only available in v56
if ( $p->{adcirc_version} eq "v56.0.3" ) {
   $wetdry_control_line .= "slim=$slim\n";
   $wetdry_control_line .= "windlim=$windlim\n";
   $wetdry_control_line .= "directvelWD=$directvelWD\n";
   $wetdry_control_line .= "useHF=$useHF\n";
}
$wetdry_control_line    .= "/\n";
#
# construct &inundationOutput namelist %inundation_output_control_namelist%
# &inundationOutputControl inundationOutput=logicalValue0, inunThresh=floatValue /
my $inundationOutput = $p->{output}->{inundationOutputControl}->{inundationOutput} eq 'yes' ? 'T' : 'F';
my $inundation_output_control_line = "&inundationOutputControl inundationOutput=$inundationOutput, inunThresh=$p->{output}->{inundationOutputControl}->{inunThresh} /";
#
my $dynamic_water_level_correction_line = 'NO LINE HERE';
#
# construct &SWANOutputControl name list %swan_output_control_namelist%
my $SWAN_OutputTPS = $p->{swan}->{SWANOutputControl}->{SWAN_OutputTPS} eq 'yes' ? 'T' : 'F';
my $SWAN_OutputTM01 = $p->{swan}->{SWANOutputControl}->{SWAN_OutputTM01} eq 'yes' ? 'T' : 'F';
my $SWAN_OutputHS = $p->{swan}->{SWANOutputControl}->{SWAN_OutputHS} eq 'yes' ? 'T' : 'F';
my $SWAN_OutputDIR = $p->{swan}->{SWANOutputControl}->{SWAN_OutputDIR} eq 'yes' ? 'T' : 'F';
my $SWAN_OutputTMM10 = $p->{swan}->{SWANOutputControl}->{SWAN_OutputTMM10} eq 'yes' ? 'T' : 'F';
my $SWAN_OutputTM02 = $p->{swan}->{SWANOutputControl}->{SWAN_OutputTM02} eq 'yes' ? 'T' : 'F';
my $swan_output_control_line = "&SWANOutputControl SWAN_OutputTPS=$SWAN_OutputTPS, SWAN_OutputTM01=$SWAN_OutputTM01, SWAN_OutputHS=$SWAN_OutputHS, SWAN_OutputDIR=$SWAN_OutputDIR, SWAN_OutputTMM10=$SWAN_OutputTMM10, SWAN_OutputTM02=$SWAN_OutputTM02 /";
#
# contstruct &Smag_control namelist %smag_control_namelist% ; defaults as follows if not set
my $smag_comp_flag = $p->{lateral_turbulence}->{smag_comp_flag} eq 'on' ? 'T' : 'F';
my $smag_upper_lim = $p->{lateral_turbulence}->{smag_upper_lim};
my $smag_lower_lim = $p->{lateral_turbulence}->{smag_lower_lim};
my $smag_control_line = "&Smag_Control \n";
# the following are available in any ADCIRC version supported by ASGS
$smag_control_line    .= "smag_comp_flag=$smag_comp_flag,\n";
$smag_control_line    .= "smag_upper_lim=$smag_upper_lim,\n";
$smag_control_line    .= "smag_comp_flag=$smag_lower_lim\n";
$smag_control_line    .= "/ \n";
#
# construct &WarnElevControl namelist %warnelevcontrol_namelist%
# &WarnElevControl WarnElev=floatValue, WarnElevDump=logicalValue, WarnElevDumpLimit=integerValue, ErrorElev=floatValue /
#
# the following values are specified on the NFOVER line in v53release and
# both on the NFOVER line as well as the WarnElevControl namelist in later
# versions (through v56); default values (if they are not set in the
# fort.15):
#  WarnElev = 20.0         ! default
# iWarnElevDump = 0       ! init
# WarnElevDump = .False.  ! default
#     WarnElevDumpLimit = 50  ! default
#     WarnElevDumpCounter = 0 ! init
#     ErrorElev = 1000.0      ! default
my $warnElev = $p->{output}->{non_fatal_override}->{WarnElev};
my $iWarnElevDump = $p->{output}->{non_fatal_override}->{iWarnElevDump};
my $warnElevDump = $iWarnElevDump eq '1' ? 'T' : 'F' ;
my $warnElevDumpLimit = $p->{output}->{non_fatal_override}->{WarnElevDumpLimit};
my $errorElev = $p->{output}->{non_fatal_override}->{ErrorElev};
my $warnelevcontrol_line = "&WarnElevControl\n";
$warnelevcontrol_line .= "WarnElev=$warnElev,\n";
$warnelevcontrol_line .= "WarnElevDump=$warnElevDump,\n";
$warnelevcontrol_line .= "WarnElevDumpLimit=$warnElevDumpLimit,\n";
$warnelevcontrol_line .= "ErrorElev=$errorElev\n";
$warnelevcontrol_line .= "/\n";
#
# construct &WaveCoupling namelist %wavecoupling_namelist%
# &waveCoupling WaveWindMultiplier=floatValue, Limit_WaveStressGrad=logicalValue, WaveStressGrad_Cap=floatValue /
my $waveWindMultiplier = $p->{wave_coupling}->{WaveWindMultiplier};
my $limitWaveStressGrad = $p->{wave_coupling}->{Limit_WaveStressGrad} eq 'yes' ? 'T' : 'F' ;
my $waveStressGradCap = $p->{wave_coupling}->{WaveStressGrad_Cap};
my $wavecoupling_line = "&WaveCoupling\n";
$wavecoupling_line .= "WaveWindMultiplier=$waveWindMultiplier,\n";
$wavecoupling_line .= "Limit_WaveStressGrad=$limitWaveStressGrad,\n";
$wavecoupling_line .= "WaveStressGradCap=$waveStressGradCap\n";
$wavecoupling_line .= "/\n";
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
my $eslm = $p->{lateral_turbulence}->{eddy_viscosity_coefficient};  # this is the default
if ( $p->{lateral_turbulence}->{formulation} eq "smagorinsky" ) {
   $eslm = -$p->{lateral_turbulence}->{smagorinsky_coefficient};    # must be set to negative in fort.15
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
my $nwp = 0;
my @nodal_attributes_activate;
my @nodal_attributes_deactivate;
if ( defined $p->{nodal_attributes}->{activate} ) {
   foreach my $na (@{$p->{nodal_attributes}->{activate}}) {
      if ( $p->{meteorology}->{windExposure} eq "10m" ) {
         # deactivate nodal attributes associated with land interaction
         if ( $na eq "surface_directional_effective_roughness_length" || $na eq "surface_canopy_coefficient" ) {
            push(@nodal_attributes_deactivate,$na);
            next;
         }
      }
      if ( $p->{output}->{inventory} eq "metonly" ) {
         # prevent write_output.F in adcirc from trying to write ESLNodes.63
         if ( $na eq "elemental_slope_limiter" ) {
            push(@nodal_attributes_deactivate,$na);
            next;
         }
      }
      push(@nodal_attributes_activate,$na);
   }
   $nwp = scalar @nodal_attributes_activate;
   if ( (scalar @nodal_attributes_deactivate) > 0 ) {
      $nwp.=" ! deactivated: " . join(" ", @nodal_attributes_deactivate);
   }
}
#
ASGSUtil::stderrMessage("INFO","Filling in ADCIRC control template (fort.15).");
while(<TEMPLATE>) {
    # name of the mesh
    s/%MeshName%/$p->{mesh}/;
    # fill in the ADCIRC version
    s/%ADCIRCVER%/$p->{adcirc_version}/;
    # if we are looking at the first line, fill in the name of the storm
    # and the advisory number, if available
    s/%StormName%/$rundesc/;
    # fill in frequency of time step output to STDOUT or adcirc.log
    s/%NSCREEN%/$p->{output}->{nscreen}/;
    # non-fatal override (water levels for warnings and fatal errors)
    s/%NFOVER%/$p->{output}->{non_fatal_override}->{nfover}/;
    s/%WarnElev%/$p->{output}->{non_fatal_override}->{WarnElev}/;
    s/%iWarnElevDump%/$p->{output}->{non_fatal_override}->{iWarnElevDump}/;
    s/%WarnElevDumpLimit%/$p->{output}->{non_fatal_override}->{WarnElevDumpLimit}/;
    s/%ErrorElev%/$p->{output}->{non_fatal_override}->{ErrorElev}/;
    # logging levels (debug, echo, info, warning, error)
    s/%NABOUT%/$logLevelsNABOUT{$p->{output}->{log_level}}/;
    # coordinate system
    s/%ICS%/$ics/;
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
    # meteorological forcing type (depending on full inventory or metonly mode)
    s/%NWS%/$thisNWS/;
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
    # number of forcing frequencies (or indicator to look to a separate
    # input file) for flux boundaries
    s/%NFFR%/$nffr/;
    # fill in nodal factors and equilibrium arguments
    if ( $p->{tides}->{tidal_forcing} eq "on" ) {
       s/%NTIFCOMMENT%/$p->{tides}->{tidal_potential_comment}/;
       s/%NBFRCOMMENT%/$p->{tides}->{tidal_boundary_comment}/;
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
    s/%NCPROJ%/$p->{netcdf_metadata}->{ncproj}/;
    s/%NCINST%/$p->{netcdf_metadata}->{ncinst}/;
    s/%NCSOUR%/$p->{netcdf_metadata}->{ncsour}/;
    s/%NCHIST%/$p->{netcdf_metadata}->{nchist}/;
    s/%NCREF%/$p->{netcdf_metadata}->{ncref}/;
    s/%NCCOM%/$p->{netcdf_metadata}->{nccom}/;
    s/%NCHOST%/$p->{netcdf_metadata}->{nchost}/;
    s/%NCCONV%/$p->{netcdf_metadata}->{ncconv}/;
    s/%NCCONT%/$p->{netcdf_metadata}->{nccont}/;
    s/%NCDATE%/$p->{netcdf_metadata}->{ncdate}/;
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
    s/%NCCONV-Line%//;
    s/%NCCONT-Line%//;
    s/%NCDATE-Line%//;
    # namelists
    s/%met_control_namelist%/$met_control_line/;
    s/%wetdry_control_namelist%/$wetdry_control_line/;
    s/%inundation_output_control_namelist%/$inundation_output_control_line/;
    s/%dynamic_water_level_correction_namelist%/$dynamic_water_level_correction_line/;
    s/%swan_output_control_namelist%/$swan_output_control_line/;
    s/%smag_control_namelist%/$smag_control_line/;
    s/%warnelevcontrol_namelist%/$warnelevcontrol_line/;
    s/%wavecoupling_namelist%/$wavecoupling_line/;
    # individual namelist parameters
    s/%WindDragLimit%/$p->{metControl}->{WindDragLimit}/;          # &metControl
    s/%DragLawString%/\"$p->{metControl}->{DragLawString}\"/;
    s/%outputWindDrag%/$outputWindDrag/;
    s/%invertedBarometerOnElevationBoundary%/$invertedBarometerOnElevationBoundary/;
    s/%outputNodeCode%/$outputNodeCode/;                           # &wetDryControl
    s/%outputNOFF%/$outputNOFF/;
    s/%noffActive%/$noffActive/;
    s/%slim%/$p->{wetDryControl}->{slim}/;
    s/%windlim%/$windlim/;
    s/%directvelWD%/$directvelWD/;
    s/%useHF%/$useHF/;
    s/%inundationOutput%/$inundationOutput/;                       # &inundationOutputControl
    s/%inunThresh%/$p->{inundationOutputControl}->{inunThresh}/;
    s/%SWAN_OutputTPS%/$SWAN_OutputTPS/;                           #  &SWANOutputControl
    s/%SWAN_OutputTM01%/$SWAN_OutputTM01/;
    s/%SWAN_OutputHS%/$SWAN_OutputHS/;
    s/%SWAN_OutputDIR%/$SWAN_OutputDIR/;
    s/%SWAN_OutputTMM10%/$SWAN_OutputTMM10/;
    s/%SWAN_OutputTM02%/$SWAN_OutputTM02/;
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
if ( $p->{nodal_attributes}->{template} =~ /.*null$/ || $p->{nodal_attributes}->{template} =~ /.*notset$/ ) {
   ASGSUtil::stderrMessage("INFO","There is no nodal attributes (fort.13) template '$p->{nodal_attributes}->{template}'; the nodal attributes file '$nafile' will not be written.");
} else {
   ASGSUtil::stderrMessage("INFO","Reading nodal attributes (fort.13) template '$p->{nodal_attributes}->{template}' and writing nodal attributes file '$nafile'.");
   my $nafi;
   if (not open($nafi,"<","$p->{nodal_attributes}->{template}") ) {
      ASGSUtil::stderrMessage("ERROR","Failed to open '$p->{nodal_attributes}->{template}': $!.");
      die;
   }
   my $nafo;
   if (not open($nafo,">",$nafile) ) {
      ASGSUtil::stderrMessage("ERROR","Failed to open '$nafile' for writing: $!.");
      die;
   }
   my $numLines = 0;
   my $numNodalAttr = 0;
   for my $line (1 .. 3) {
      my $headerLine = <$nafi>;
      print $nafo $headerLine;
      $numLines++;
      # parse out the number of nodal attributes from the 3rd line
      if ( $line == 3 ) {
         $headerLine =~ /^\s*(\d)*/;
         $numNodalAttr = $1;
         ASGSUtil::stderrMessage("INFO","There are '$numNodalAttr' nodal attributes in the fort.13 file.");
      }
   }
   # accumulate the nodal attributes file header into a single string
   my $headerLines;
   for my $line (1 .. ($numNodalAttr*4)) {
      $headerLines .= <$nafi>;
      $numLines++;
   }
   # s/// on header as a block
   foreach my $key (keys %{$p->{nodal_attributes}->{default_values}}) {
      my $tag   = "%"."$key"."_default"."%";
      my $value = $p->{nodal_attributes}->{default_values}->{$key};
      $headerLines =~ s/$tag/$value/g;
   }
   # write header to the file
   print $nafo $headerLines;
   # now append nodal attributes body
   foreach my $line (<$nafi>) {
      print $nafo $line;
      $numLines++;
   }
   #
   close($nafi); # nodal attributes file template
   close($nafo); # nodal attributes file (filled template)
   ASGSUtil::stderrMessage("INFO","Wrote '$numLines' lines to the nodal attributes file '$nafile'.");
}
#
#
#  S W A N   C O N T R O L   F I L E
#
if ( $nws ne "0" && $p->{wave_coupling}->{waves} eq "on" && $p->{wave_coupling}->{wave_model} eq "SWAN" && $p->{output}->{inventory} ne "metonly" ) {
   # open swan template file for fort.26
   unless (open(TEMPLATE,"<$p->{swan}->{template}")) {
      ASGSUtil::stderrMessage("ERROR","Failed to open the swan template file '$p->{swan}->{template}' for reading: $!.");
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
   if ( $p->{swan}->{hot} eq "off" ) {
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
       s/%swandt%/$p->{wave_coupling}->{rstiminc}/;
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
if ( $nws ne "0" && $p->{wave_coupling}->{waves} eq "on" && $p->{wave_coupling}->{wave_model} eq "SWAN" && $p->{output}->{inventory} ne "metonly" ) {
   $model_type = "SPDS";
   $model = "PADCSWAN";
}
if ( abs($basenws) == 12 ) {
   $cycle_hour = sprintf("%02d",$oh);
   $currentdate = substr($oy,2,2) . sprintf("%02d%02d",$om,$od); # start time
   $date1 = sprintf("%4d%02d%02dT%02d%02d",$oy,$om,$od,$oh,$omin);
}
if ( $p->{scenario} eq "nowcast" ) {
   $run_type = "Nowcast";  # for the run-control.properties file
} elsif ( $p->{scenario} eq "hindcast" ) {
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
if ( abs($basenws) != 19 && abs($basenws) != 20 ) {
   printf RUNPROPS "track_raw_dat : notrack\n";
   printf RUNPROPS "track_raw_fst : notrack\n";
   printf RUNPROPS "track_modified : notrack\n";
}
printf RUNPROPS "year : $ny\n";
printf RUNPROPS "mesh : $p->{mesh}\n";
printf RUNPROPS "RunType : $run_type\n";
printf RUNPROPS "ADCIRCgrid : $p->{mesh}\n";
printf RUNPROPS "currentcycle : $cycle_hour\n";
printf RUNPROPS "currentdate : $currentdate\n";
printf RUNPROPS "advisory : $p->{cycle}\n";
if ( $p->{hotstart}->{time} != 0 ) {
   printf RUNPROPS "InitialHotStartTime : $p->{hotstart}->{time}\n";
}
printf RUNPROPS "RunStartTime : $runstarttime\n";
printf RUNPROPS "RunEndTime : $runendtime\n";
printf RUNPROPS "ColdStartTime : $p->{coldstartdate}\n";
#
printf RUNPROPS "Model : $model\n";
# model parameters
printf RUNPROPS "adcirc.control.numerics.im : $im\n";
printf RUNPROPS "adcirc.control.numerics.a00b00c00 : ( $a00b00c00 )\n";
printf RUNPROPS "adcirc.control.physics.nolica : $nolica\n";
printf RUNPROPS "adcirc.control.physics.nolicat : $nolicat\n";
printf RUNPROPS "adcirc.control.monitoring.nscreen : $p->{output}->{nscreen}\n";
printf RUNPROPS "adcirc.control.monitoring.nabout : $logLevelsNABOUT{$p->{output}->{log_level}}\n";
printf RUNPROPS "adcirc.control.physics.rnday : $RNDAY\n";
printf RUNPROPS "adcirc.control.numerics.input.ihot : $ihot\n";
printf RUNPROPS "adcirc.control.physics.nwp : $nwp\n";
printf RUNPROPS "adcirc.control.physics.eslm : $eslm\n";
printf RUNPROPS "adcirc.control.numerics.nffr : $nffr\n";
printf RUNPROPS "adcirc.control.numerics.output.nhsinc : $NHSINC\n";
printf RUNPROPS "adcirc.control.numerics.output.nhstar : $NHSTAR\n";
my @noute = split(" ",$numelevstations);
printf RUNPROPS "adcirc.control.physics.output.noute : $noute[0]\n";
my @noutv = split(" ",$numvelstations);
printf RUNPROPS "adcirc.control.physics.output.noutv : $noutv[0]\n";
printf RUNPROPS "adcirc.control.numerics.ititer : $ititer\n";
if ( $nws ne "0" ) {
   printf RUNPROPS "adcirc.control.numerics.meteorology.wtiminc_line : ( $wtiminc_line )\n";
   my @noutm = split(" ",$nummetstations);
   printf RUNPROPS "adcirc.control.physics.output.noutm : $noutm[0]\n";
   if ( abs($nws) == 19 || abs($nws) == 319 || abs($nws) == 20 || abs($nws) == 320 || abs($nws) == 8 || abs($nws) == 308 || abs($nws) == 30 || abs($nws) == 330 ) {
      printf RUNPROPS "adcirc.control.physics.meteorology.bladj : $p->{meteorology}->{boundary_layer_adjustment}\n";
      if ( abs($nws) == 30 || abs($nws) == 330 ) {
         printf RUNPROPS "adcirc.control.numerics.meteorology.purevortex : $p->{meteorology}->{blending}->{pureVortex}\n";
         printf RUNPROPS "adcirc.control.numerics.meteorology.purebackground : $p->{meteorology}->{blending}->{pureBackground}\n";
      }
   }
}
# write the names of the output files to the run-control.properties file
ASGSUtil::stderrMessage("INFO","Writing file names and formats to run-control.properties file.");
writeFileName("fort.61",(split(' ',$fort61))[0],$addHours/(split(' ',$fort61))[3]/3600.0);
writeFileName("fort.62",(split(' ',$fort62))[0],$addHours/(split(' ',$fort62))[3]/3600.0);
writeFileName("fort.63",(split(' ',$fort63))[0],$addHours/(split(' ',$fort63))[3]/3600.0);
writeFileName("fort.64",(split(' ',$fort64))[0],$addHours/(split(' ',$fort64))[3]/3600.0);
writeFileName("fort.71",(split(' ',$fort7172))[0],$addHours/(split(' ',$fort7172))[3]/3600.0);
writeFileName("fort.72",(split(' ',$fort7172))[0],$addHours/(split(' ',$fort7172))[3]/3600.0);
writeFileName("fort.73",(split(' ',$fort7374))[0],$addHours/(split(' ',$fort7374))[3]/3600.0);
writeFileName("fort.74",(split(' ',$fort7374))[0],$addHours/(split(' ',$fort7374))[3]/3600.0);
writeFileName("maxele.63",(split(' ',$fort63))[0],1);
writeFileName("maxvel.63",(split(' ',$fort64))[0],1);
writeFileName("maxwvel.63",(split(' ',$fort7374))[0],1);
writeFileName("minpr.63",(split(' ',$fort7374))[0],1);
if ( $nws ne "0" && $p->{wave_coupling}->{waves} eq "on" && $p->{wave_coupling}->{wave_model} eq "SWAN" && $p->{output}->{inventory} ne "metonly" ) {
   writeFileName("maxrs.63",(split(' ',$fort7374))[0],1);
   writeFileName("swan_DIR.63",(split(' ',$fort7374))[0],$addHours/(split(' ',$fort7374))[3]/3600.0);
   writeFileName("swan_DIR_max.63",(split(' ',$fort7374))[0],1);
   writeFileName("swan_HS.63",(split(' ',$fort7374))[0],$addHours/(split(' ',$fort7374))[3]/3600.0);
   writeFileName("swan_HS_max.63",(split(' ',$fort7374))[0],1);
   writeFileName("swan_TMM10.63",(split(' ',$fort7374))[0],$addHours/(split(' ',$fort7374))[3]/3600.0);
   writeFileName("swan_TMM10_max.63",(split(' ',$fort7374))[0],1);
   writeFileName("swan_TPS.63",(split(' ',$fort7374))[0],$addHours/(split(' ',$fort7374))[3]/3600.0);
   writeFileName("swan_TPS_max.63",(split(' ',$fort7374))[0],1);
}
if ( $p->{output}->{inundationOutputControl}->{inundationOutput} eq "yes" ) {
   writeFileName("initiallydry.63",(split(' ',$fort63))[0],1);
   writeFileName("inundationtime.63",(split(' ',$fort63))[0],1);
   writeFileName("maxinundepth.63",(split(' ',$fort63))[0],1);
   writeFileName("everdried.63",(split(' ',$fort63))[0],1);
   writeFileName("endrisinginun.63",(split(' ',$fort63))[0],1);
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
   my ( $id, $sp, $nds ) = @_;
   #
   my $format = "ascii"; # default output file format
   my $f = $id; # default (ascii) name of output file
   #
   # if there won't be any output of this type, just return without
   # writing anything to the run-control.properties file
   if ( $sp == 0 ) {
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

   # format specifier
   if ( abs($sp) == 3 || abs($sp) == 5 ) {
      $f = $f . ".nc";
      $format = "netcdf";
   }
   if ( abs($sp) == 4 ) {
      $format = "sparse-ascii";
   }
   printf RUNPROPS "$ids_descs{$id} File Name : $f\n";
   printf RUNPROPS "$ids_descs{$id} Format : $format\n";
   printf RUNPROPS "adcirc.file.output.$f.numdatasets : $nds\n";
}
#
#
#--------------------------------------------------------------------------
#      S U B   G E T   O U T P U T   P A R A M E T E R S
#
# Determines the correct output specifier for output files based on
# the output frequency, whether or not the files should be appended,
# and whether or not the netcdf format is used (ascii is the default).
#--------------------------------------------------------------------------
sub getOutputParameters {
   my ($f, $inv, $timestep_seconds, $data_type ) = @_;
   my $specifier; # output format
   my $increment; # time step increment between outputs

   if ( $f->{incr_seconds} == 0 || ( $p->{output}->{inventory} eq "metonly" && $data_type ne "met" ) ) {
      $specifier = "0";
      $increment = "99999";
   } else {
      if ( $f->{append} eq "yes" ) {
         $specifier = "1";
      } else {
         $specifier = "-1";
      }
      if ( $f->{format} eq "netcdf4" ) {
         $specifier *= 5;
      }
      if ($f->{format} eq "netcdf3" ) {
         $specifier *= 3;
      }
      $increment = int($f->{incr_seconds}/$timestep_seconds);
   }
   return "$specifier 0.0 999.0 $increment";
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
      ASGSUtil::stderrMessage("INFO","No periodic inflow boundary data file was specified.");
      return
   }
   unless (open(FLUXFILE,"<$flux_file")) {
      ASGSUtil::stderrMessage("ERROR","Failed to open '$flux_file' for reading: $!.");
      die;
   }
   my $fluxdata='';
   while (<FLUXFILE>){
       $fluxdata.=$_;
   }
   close(FLUXFILE);
   ASGSUtil::stderrMessage("INFO","Inserting periodic inflow boundary data from '$flux_file'.");
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
   $rundesc = "cs:$p->{coldstartdate}"."0000 cy:initialization";
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
    $rundesc = "cs:$p->{coldstartdate}"."0000 cy:custom";
    my $alreadyElapsedDays = 0.0;
    if ( $p->{hotstart}->{time} != 0.0 ) {
       $alreadyElapsedDays = $p->{hotstart}->{time} / 86400.0;
    }
    $RNDAY = $specifiedRunLength + $alreadyElapsedDays;
    $NHSINC = int(($RNDAY*86400.0)/$dt);
    $nws = 0;
    $scenarioid = "$p->{scenario} $specifiedRunLength day run";
       #
   # determine the relationship between the start of the NAM data and the
   # current time in the ADCIRC run
   if ( $p->{hotstart}->{time} != 0 ) {
      # now add the hotstart seconds
      ($ny,$nm,$nd,$nh,$nmin,$ns) =
         Date::Calc::Add_Delta_DHMS($cy,$cm,$cd,$ch,0,0,0,0,0,$p->{hotstart}->{time});
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
   if ( $p->{hotstart}->{time} != 0 ) {
      # now add the hotstart seconds
      ($ny,$nm,$nd,$nh,$nmin,$ns) =
         Date::Calc::Add_Delta_DHMS($cy,$cm,$cd,$ch,0,0,0,0,0,$p->{hotstart}->{time});
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
   $owistart =~ m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
   $oy = $1;
   $om = $2;
   $od = $3;
   $oh = $4;
   $omin = 0;
   $os = 0;
   my $owiend = $p->{meteorology}->{owi_win_pre}->{enddatetime};
   # create run description
   $rundesc = "cs:$p->{coldstartdate}"."0000 cy:$owistart end:$owiend OWI ASCII ";
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
   $scenarioid = $addHours . " hour " . $p->{scenario} . " run";
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
   if ( $p->{hotstart}->{time} != 0 ) {
      $hstime_days = $p->{hotstart}->{time}/86400.0;
   }
   # get end time
   my $end = $p->{endtime}; # yyyymmddhh
   ASGSUtil::stderrMessage("INFO","New $p->{scenario} time is $end.");
   if ( $p->{hotstart}->{time} != 0 ) {
      # now add the hotstart seconds
      ($ny,$nm,$nd,$nh,$nmin,$ns) =
         Date::Calc::Add_Delta_DHMS($cy,$cm,$cd,$ch,$cmin,$cs,0,0,0,$p->{hotstart}->{time});
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
   if ( $p->{hotstart}->{time} != 0 ) {
      $runlength_seconds-=$p->{hotstart}->{time};
   }
   my $min_runlength = 2*$dt;
   # if we coldstart at the nowcast, we may not have calculated a runlength
   # longer than the minimum
   my $goodRunlength = 1;
   if ( $runlength_seconds < $min_runlength ) {
      if ( $p->{scenario} eq "nowcast" ) {
         $goodRunlength = 0;
      }
      ASGSUtil::stderrMessage("INFO","Runlength was calculated as $runlength_seconds seconds, which is less than the minimum runlength of $min_runlength seconds. The RNDAY will be adjusted so that it ADCIRC runs for the minimum length of simulation time.");
      # recalculate the RNDAY as the hotstart time plus the minimal runlength
      if ( $p->{hotstart}->{time} != 0 ) {
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
   if ( $nws ne "0" && $p->{wave_coupling}->{waves} eq "on" ) {
      my $total_time = $RNDAY*86400.0; # in seconds
      # unusual but possible for the total run time to be less than the swan
      # time step
      if ( $total_time < $p->{wave_coupling}->{rstiminc} ) {
         $total_time = $p->{wave_coupling}->{rstiminc}; # run for at least one swan time step
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
   if ( $p->{hotstart}->{time} != 0 ) {
      $addHours-=$p->{hotstart}->{time}/3600.0;
   }
   #
   # create run description
   $rundesc = "cs:$p->{coldstartdate}"."0000 cy:$p->{meteorology}->{tropical_cyclone}->{storm_name}$p->{cycle}";
   # create the RUNID
   $scenarioid = $addHours . " hour " . $p->{scenario} . " run";
   # create the WTIMINC line
   $wtiminc_line = $cy." ".$cm." ".$cd." ".$ch." 1 ".$p->{meteorology}->{tropical_cyclone}->{boundary_layer_adjustment};
   if ( abs($basenws) == 20 || abs($basenws) == 30 ) {
      $wtiminc_line .= " $geofactor";
   }
}


