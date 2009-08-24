#!/usr/bin/env perl


# run in the LPFS
# RJW:08/2008

        $linenum = 0;
        $count   = 0;
       open(RUN_INFO,"< fort.22" ) || die " No info file, fort.22, for trackline and storm data \n";
       open(TACK_line_DATA,"> track_line.dat") || die " cannot open file to write track data \n" ;
       open(TACK_point_DATA,"> track_point.dat") || die " cannot open file to write track data \n" ;
  #    open(CycleTime,"> cycletime_file.dat") || die " cannot open file to write track data \n" ;
       while(<RUN_INFO>) {
              chomp;
         $storm_data_string= $_ ;
           $linenum++;
            my @array = split(/,/,$storm_data_string);
   #      foreach my $value (@array)
   #        {
   #       print $value . "\n" ;
   #        }
     $cycletime[$linenum]=$array[2];
             $x[$linenum]=$array[7];
             $y[$linenum]=$array[6];
          $size1[$linenum]=$array[8];
          $size2[$linenum]=$array[9];
         $x[$linenum]= $x[$linenum]/-10.0;
         $y[$linenum]= $y[$linenum]/10.0;
         $size[$linenum]= $size1[$linenum]/100.0;
  #     print " try to write out data", "\n";
  #       print $x[$linenum]," ",$y[$linenum], "\n";
          if ($x[$linenum] == $x[$linenum-1] and $y[$linenum] == $y[$linenum-1]) {
  #                 print " same position of storm \n";
          }
          else {
             $Lon[$count] = $x[$linenum];
             $Lat[$count] = $y[$linenum];
             $Size[$count]= $size[$linenum];
           print TACK_point_DATA $Lat[$count]," ", $Lon[$count]," ",$Size[$count],"\n";
           print TACK_line_DATA $Lat[$count]," ", $Lon[$count],"\n";
             $count++;
         }
  #      print $array[6]," ",$array[7], "\n";
      }
          close TACK_point_DATA;
          close TACK_line_DATA;
 # write out cycletime to file
  #        $cycletime[$linenum] = substr($cycletime[$linenum],1,11);
  #        print CycleTime  $cycletime[$linenum];
  #        close CycleTime


