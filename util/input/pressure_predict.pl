#!/usr/bin/env perl
#---------------------------------------------------------------------
# pressure_predict.pl
#
# This script is used to assess various methods for predicting the
# central pressure based on max wind speed.
#
# It reads a BEST track file, then at each point in time in the file,
# it attempts to predict central pressure based on max wind speed
# for the next 120 hours (5 days). It writes out a data file that can
# be read in and plotted with gnuplot.
#
# It then creates a gnuplot script to plot the data.
#
#---------------------------------------------------------------------
#
# Copyright(C) 2011 Jason Fleming
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
#
#---------------------------------------------------------------------
#
use strict;
use Getopt::Long;
use Date::Calc;
$^W++;
#
my $dir;                            # path to raw ATCF hindcast and forecast
my $storm;                          # number, e.g., 05 or 12
my $year;                           # YYYY
my $pi=3.141592653589793;
my $method = "twoslope";
my $forecast_length = 120;
my $hindcastATCF = "null";
my $trackfile_type = "best";
#
#
GetOptions(
           "method=s" => \$method,
           "trackfile=s" => \$hindcastATCF,
           "trackfiletype=s" => \$trackfile_type,
           "forecastlength=s", \$forecast_length,
           "storm=s" => \$storm,
           "year=s" => \$year,
           );
#
if ( $hindcastATCF eq "null" ) {
    $hindcastATCF = "bal$storm$year.dat";
}
unless (open(HCST,"<$hindcastATCF")) {
   stderrMessage("ERROR","Failed to open hindcast ATCF file $hindcastATCF: $!.");
   die;
}
if ( $method eq "courtneyknaff2009" && $trackfile_type ne "nws19" ) {
    stderrMessage("ERROR","The courtneyknaff2009 method requires an nws19 trackfile.");
    die;
}
#---------------------------------------------------------------------
# P R O C E S S I N G   H I N D C A S T   F I L E
#---------------------------------------------------------------------
my @date_strings; # date/time values, yyyymmddhh
my @lats;         # latitude, degrees
my @c;            # storm translation speed, kts
my @avgr34;       # average size of the nonzero 34kt isotach radii
my @vmaxes;       # max wind speed values, kts
my @rmaxes;       # not needed, nautical miles
my @pinfs;        # barometric pressure at last closed isobar, mb
my @pns;          # derived environmental pressure using dvorak, mb
my @pcs;          # central pressure values, mb
my @pcf;          # forecasted central pressure values, mb
my @rmse;         # root mean square error at each forecast increment, mb
my @num_obs;      # number of predictions at each forecast increment
my @time_difference; # between start of forecast and each forecast incr, hours
my @error;        # string of values at each incr: forecasted - observed, mb
my @mean_error;   # avg for a time incr: forecasted - observed, mb
#
# read the BEST track file and load up all the dates, Vmaxes, and central
# pressure values
my $last_date="0000000000"; # used to see if the data correspond to new date
while(<HCST>) {
    my @fields = split(',',$_);
    # if this is not a new time point, just go to the next line
    if ( $last_date eq $fields[2] ) {
       next;
    }
    $last_date = $fields[2];
    # reported pressure at last closed isobar (environmental pressure)
    if ( length($_) < 101 ) {
       push(@pinfs,"-99999");
    } else {
       my $pinf = substr($_,97,4);
       # work around some BEST track files that have typos
       if ( $pinf < 950 ) {
          $pinf = "-99999";
       }
       push(@pinfs,$pinf);
    }
    # latitude
    $fields[6] =~ /(\d+)/;
    push(@lats,$1/10.0);
    # date
    push(@date_strings,$fields[2]);
    # central pressure
    my $this_pc = substr($_,53,4);
    push(@pcs,$this_pc);
    # max wind speed
    my $this_vmax = substr($_,48,3);
    push(@vmaxes,$this_vmax);
    # derived environmental pressure
    my $this_pn = $this_pc + ($this_vmax*0.51444444/2.3)**(1/0.76);
    push(@pns,$this_pn);
    # radius to maximum winds
    if ( length($_) < 109 ) {
       push(@rmaxes,"-99999");
    } else {
       push(@rmaxes,substr($_,109,3));
    }
    # storm translation speed
    if ( $trackfile_type eq "nws19" ) {
        $fields[26] =~ /(\d+)/;
        push(@c,$1);
    }
    # average of the nonzero isotach radii at 34kts
    my $num_nonzero_radii = 0;
    my $avg = 0.0;
    for (my $i=13; $i<17; $i++ ) {
       $fields[$i] =~ /(\d+)/;
       if ( $1 != 0 ) {
          $num_nonzero_radii++;
          $avg += $1;
       }
    }
    if ( $num_nonzero_radii != 0 ) {
       push(@avgr34,$avg/$num_nonzero_radii);
    } else {
       push(@avgr34,"-99999");
    }
}
close(HCST);
#
# we now have arrays of real data ... produce successive sets of predicted
# data ... the first set of predicted data uses the first real value as its
# starting point ... the 2nd set of predicted data uses the 2nd real value
# as its starting point, etc.
my $num_real = @date_strings;
my @output_rows;
for (my $i=0; $i<$num_real; $i++ ) {
   $output_rows[$i] =
      "$date_strings[$i]  $pinfs[$i]  $vmaxes[$i]  $rmaxes[$i]  $pcs[$i]  ";
}
my $tsflag=0;  # set to 1 when the storm reaches TS force
for (my $i=1; $i<$num_real; $i++ ) {
   # fill in prior values as "missing"
   for ( my $j=0; $j<$i; $j++ ) {
      $pcf[$j] = "-99999";
   }
   # set a flag if this storm as achieved TS force at some point so far
   if ( $tsflag == 0 ) {
      if ( $vmaxes[$i] > 39.0 ) {
         $tsflag = 1;
      }
   }
   my $pp = $pcs[$i-1];       # pressure at previous time
   my $vmaxp = $vmaxes[$i-1]; # vmax at previous time
   my $nowcast_time = $date_strings[$i-1]; # date/time at previous time
   $nowcast_time =~ /(\d{4})(\d{2})(\d{2})(\d{2})/;
   my $ny = $1;
   my $nm = $2;
   my $nd = $3;
   my $nh = $4;
   #
   # loop through the remainder of the array and  execute the algorithm
   # to predict the central pressure from the given vmax
   my $forecast_increment=0;
   for ( my $j=$i; $j<$num_real; $j++ ) {
      # check to see if we are more than 120 hours from the nowcast time,
      # and if so, we don't need to make predictions, and will therefore
      # just put down the missing value
      $date_strings[$j] =~ /(\d{4})(\d{2})(\d{2})(\d{2})/;
      my $fy = $1;
      my $fm = $2;
      my $fd = $3;
      my $fh = $4;
      (my $ddays,my $dhrs, my $dsec) = Date::Calc::Delta_DHMS($ny,$nm,$nd,$nh,0,0,$fy,$fm,$fd,$fh,0,0);
      $time_difference[$forecast_increment] = $ddays*24 + $dhrs; # in hours
      if ( $time_difference[$forecast_increment] > $forecast_length ) {
         $pcf[$j] = "-99999";
         next;
      }
      if ( $method eq "twoslope" ) {
         # same as last time by default
         $pcf[$j] = sprintf("%4d",$pp);
         # if stronger max wind
         if ( $vmaxes[$j] > $vmaxp ) {
            $pcf[$j] = sprintf("%4d",(1040.0-0.877*$vmaxes[$j]));
            # the resulting pressure should be lower than the last
            # ... if it isn't, just use the slope
            if ( $pcf[$j] > $pp ) {
               $pcf[$j] = sprintf("%4d",($pp - 0.877*($vmaxes[$j]-$vmaxp)));

            }
         }
         # if weaker max wind
         if ( $vmaxes[$j] < $vmaxp ) {
            $pcf[$j] = sprintf("%4d",(1000.0-0.65*$vmaxes[$j]));
            # the resulting pressure should be higher than the last
            # ... if it isn't, just use the slope
            if ($pcf[$j] < $pp ) {
               $pcf[$j] = sprintf("%4d",($pp + 0.65*($vmaxp-$vmaxes[$j])));
            }
         }
         # slower windspeeds can be strange ... just use the last pressure
         if ( $vmaxes[$j] <= 30 ) {
            $pcf[$j] = sprintf("%4d",$pp);

         }
         $pp = $pcf[$j];
         $vmaxp = $vmaxes[$j];

      } elsif ( $method eq "asgs2012" ) {
         # same as last time by default
         $pcf[$j] = sprintf("%4d",$pp);
         # if stronger max wind
         if ( $vmaxes[$j] > $vmaxp ) {
            $pcf[$j] = sprintf("%4d",(1040.0-0.877*$vmaxes[$j]));
            # the resulting pressure should be lower than the last
            # ... if it isn't, just use the slope
            if ( $pcf[$j] > $pp ) {
               $pcf[$j] = sprintf("%4d",($pp - 0.877*($vmaxes[$j]-$vmaxp)));
            }
         }
         # if weaker max wind
         if ( $vmaxes[$j] < $vmaxp ) {
            $pcf[$j] = sprintf("%4d",(1000.0-0.65*$vmaxes[$j]));
            # the resulting pressure should be higher than the last
            # ... if it isn't, just use the slope
            if ($pcf[$j] < $pp ) {
               $pcf[$j] = sprintf("%4d",($pp + 0.65*($vmaxp-$vmaxes[$j])));
            }
         }
         # slower windspeeds can be strange ... use Dvorak if the storm is
         # early in its history, or use ah77 if it is late in its history
         if ( $vmaxes[$j] <= 35 ) {
            if ( $tsflag == 0 ) {
               # use Dvorak
               $pcf[$j] = 1015 - ($vmaxes[$j]/3.92*0.51444444)**(1.0/0.644);
            } else {
               # its later in the storm's history -- use AH77
               $pcf[$j] = 1010 - ($vmaxes[$j]/3.4*0.51444444)**(1.0/0.644);
            }
         }
         $pp = $pcf[$j];
         $vmaxp = $vmaxes[$j];

      } elsif ( $method eq "courtneyknaff2009" ) {
         # need an estimate of storm size for this to work
         if ( $avgr34[$j] != -99999 ) {

            my $vsrm = $vmaxes[$j] - 1.5*($c[$j]**0.63);
            my $rmax = 66.785 - 0.09102*$vmaxes[$j] + 1.0619*($lats[$j]-25.0);
            my $x = 0.1147 + 0.0055*$vmaxes[$j] - 0.001*($lats[$j]-25);
            my $v500c = $vmaxes[$j]*($rmax/500.0)**$x;
            my $v500 = $avgr34[$j] / 9.0 - 3.0;
            my $S = $v500/$v500c;
            if ( $S < 0.4 ) {
                $S = 0.4;
            }
            my $penv = 1013.0;
            if ( $lats[$j] < 18.0 ) {
               $pcf[$j] = $penv + 5.962 - 0.267*$vsrm - ($vsrm/18.26)**2 - 6.8*$S;
            } else {
                # latitude greater than 18.0 degrees
                $pcf[$j] = $penv + 23.286 - 0.483*$vsrm - ($vsrm/24.254)**2 - 12.587*$S - 0.483*$lats[$j];
            }
         } else {
            # if we couldn't get a size estimate, just use KnaffZehr2007
            $pcf[$j] = 1010 - ($vmaxes[$j]/2.3*0.51444444)**(1.0/0.76);
         }
      } elsif ( $method eq "ah77" ) {
         # Atkinson and Holliday, as described in Holland (2007)
         # must convert from knots to m/s to use the formula
         $pcf[$j] = 1010 - ($vmaxes[$j]/3.4*0.51444444)**(1.0/0.644);

      } elsif ( $method eq "dvorak" ) {
         # Dvorak, as described in Holland (2007)
         # must convert from knots to m/s to use the formula
         $pcf[$j] = 1015 - ($vmaxes[$j]/3.92*0.51444444)**(1.0/0.644);

      } elsif ( $method eq "koba" ) {
         # Koba, as described in KnaffZehr (2007)
         # must convert from knots to m/s to use the formula
         $pcf[$j] = 1013 + 6.22 - 0.58 * $vmaxes[$j] * 0.514444444
            - ($vmaxes[$j]*0.514444444/31.62)**2.0;

      } elsif ( $method eq "knaffzehr" ) {
         # Knaff and Zehr, as described in Holland (2007)
         # must convert from knots to m/s to use the formula
         $pcf[$j] = 1010 - ($vmaxes[$j]/2.3*0.51444444)**(1.0/0.76);
         #$pcf[$j] = 1010 - ($vmaxes[$j]/4.4*0.51444444)**(1.0/0.76);
      }
      # accumulate the error and the square of the errors at
      # this forecast increment
      my $diff = $pcf[$j]-$pcs[$j];
      $mean_error[$forecast_increment] += $diff;
      $rmse[$forecast_increment] += $diff**2;
      $num_obs[$forecast_increment]++;
      $error[$forecast_increment] .= "$diff ";
      $forecast_increment++;
   }
   # write this set of predictions to the output rows
   for (my $i=0; $i<$num_real; $i++ ) {
      $output_rows[$i] .= " $pcf[$i] ";
   }
}
# use the sum of the squared errors at each time increment to calculate
# the RMSE at each forecast increment; write the output to a file
my $rmsedatafile = $hindcastATCF . "_" . $method . "_rmse.gpd";
unless (open(RMSEDATA,">$rmsedatafile")) {
   stderrMessage("ERROR","Failed to open data file $rmsedatafile for writing: $!.");
   die;
}
# iterate over forecast increments
for (my $i=0; $i<@rmse; $i++) {
   $rmse[$i] = $rmse[$i]/$num_obs[$i];
   $rmse[$i] = sqrt($rmse[$i]);
   $mean_error[$i] = $mean_error[$i]/$num_obs[$i];
   # calculate standard deviation
   my @error_list = split(' ',$error[$i]);
   my $stddev_error = 0;
   # iterate over observations in this forecast increment
   for (my $j=0; $j<@error_list; $j++) {
      $stddev_error += ($error_list[$j] - $mean_error[$i])**2;
   }
   $stddev_error = sqrt($stddev_error/@error_list);
   printf RMSEDATA "$time_difference[$i] $rmse[$i] $mean_error[$i] $stddev_error\n";
}
close(RMSEDATA);
#
# write the output to a file
#
my $gpdatafile = $hindcastATCF . "_" . $method . ".gpd";
unless (open(GNUPLOTDATA,">$gpdatafile")) {
   stderrMessage("ERROR","Failed to open data file $gpdatafile for writing: $!.");
   die;
}
for (my $i=0; $i<$num_real; $i++ ) {
   printf GNUPLOTDATA "$output_rows[$i]\n";
}
close(GNUPLOTDATA);
#
# write the gnuplot script that can be used to plot the data
#
my $gpfile = $hindcastATCF . "_" . $method . ".gp";
unless (open(GNUPLOT,">$gpfile")) {
   stderrMessage("ERROR","Failed to open file $gpfile for writing: $!.");
   die;
}
print GNUPLOT "set terminal postscript color \"Times-Roman\" 14\n";
#print GNUPLOT "set grid\n";
print GNUPLOT "set xlabel \"Six-hour Cycles\"\n";
print GNUPLOT "set key bottom left box\n";
print GNUPLOT "set datafile missing \"-99999\"\n";
print GNUPLOT "#\n";
print GNUPLOT "set ylabel \"Pc (mb)\"\n";
#print GNUPLOT "set y2range [1000:1013]\n";
#print GNUPLOT "set y2label \"Pinf (mb)\"\n";
#print GNUPLOT "set y2tics\n";
print GNUPLOT "set title  \"$hindcastATCF Predicted Central Pressure Every 6 Hours\"\n";
for ( my $column=6; $column<$num_real; $column++ ) {
   my $pseudo_advisory = $column-5;
   my $gpframe = sprintf("%03d",$pseudo_advisory);
   print GNUPLOT "#\n";
   print GNUPLOT "set output \"pcf_all_$hindcastATCF\_$method\_$gpframe.ps\"\n";
   print GNUPLOT "plot \"$hindcastATCF\_ah77.gpd\" every 1 using 6 title \"ah77\" with linespoints 3,\\\n";
   print GNUPLOT "\"$hindcastATCF\_dvorak.gpd\" every 1 using 6 title \"dvorak\" with linespoints 2,\\\n";
   print GNUPLOT "\"$hindcastATCF\_knaffzehr.gpd\" every 1 using 6 title \"knaffzehr\" with linespoints 4,\\\n";
   print GNUPLOT "\"$hindcastATCF\_courtneyknaff2009.gpd\" every 1 using 6 title \"courtneyknaff2009\" with linespoints 5,\\\n";
   print GNUPLOT "\"$hindcastATCF\_$method.gpd\" every 1 using 5 title \"best\" with linespoints 1,\\\n";
   printf GNUPLOT "\"$gpdatafile\" every 1 using $column title \"$method $pseudo_advisory\" with linespoints 7\n";
}
close(GNUPLOT);

sub stderrMessage () {
   my $level = shift;
   my $message = shift;
   my @months = qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);
   (my $second, my $minute, my $hour, my $dayOfMonth, my $month, my $yearOffset, my $dayOfWeek, my $dayOfYear, my $daylightSavings) = localtime();
   my $year = 1900 + $yearOffset;
   my $hms = sprintf("%02d:%02d:%02d",$hour, $minute, $second);
   my $theTime = "[$year-$months[$month]-$dayOfMonth-T$hms]";
   printf STDERR "$theTime $level: pressure_predict.pl: $message\n";
   if ($level eq "ERROR") {
      sleep 60
   }
}


