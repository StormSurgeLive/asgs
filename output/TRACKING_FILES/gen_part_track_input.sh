#!/bin/bash
#--------------------------------------------------------------

# Need 3 things
#  1) from fort.64 file need:
#       run length, in hours
#       starting time  (must convert from seconds to hours)
#       number of data records in hours

#  2) need number of drogues 
#       wc -l drog initial position file the minus 1 for the header

#  3) drogue positions from the file
#       this is easy the file is set up to go as is

#  with all three in place then cat 1,2,3 > input_deepwater.din
#  done


   CONFIG=$1
   VELINPUTFILE=$2
   TRACKDIR=$3
   PARTICLEFILE=$4


   
    NumRecords=$(head -2 ${VELINPUTFILE} | tail -1 | awk '{print $1}')  # 
    OutputFreq=$(head -2 ${VELINPUTFILE} | tail -1 | awk '{print $3}')  # in Seconds

    RunStartTime=$(head -3 ${VELINPUTFILE} | tail -1 | awk '{print $1}') # in Seconds
       
  # convert seconds to hours
       echo $NumRecords $OutputFreq $RunStartTime > numberstoconvert
       ln -fs $TRACKDIR/convert_numbers.exe
             ./convert_numbers.exe
    RunStartTimeHr=$(head -1 ./convertednumbers | tail -1 | awk '{print $1}') 
    RUNLENGTH=$(head -1 ./convertednumbers | tail -1 | awk '{print $2}') 
    
  
 # get initial locations
#    Initial_Loc_File=$(ls -tr1 /corral/hurricane/mthoward/*1000m* | tail -1  | awk '{print $1}')
#    Initial_Loc_File_base=`basename $Initial_Loc_File .txt`
      Initial_Loc_File=${PARTICLEFILE}
      Initial_Loc_File_base=`basename $Initial_Loc_File .txt`

  InitPartTime=${Initial_Loc_File_base:1:8}
   echo $Initial_Loc_File_base > InitialParticleTime.txt
   echo $InitPartTime >> InitialParticleTime.txt
   

#     echo $NumRecords $OutputFreq $RunStartTime $Initial_Loc_File
#  Get the number of particles form the initial positino file
     NumParticles=$(wc -l ${Initial_Loc_File} | awk '{print $1}')
     NumParticles=`expr $NumParticles - 1`

echo "ASGS Part Track $ADVISORY ${ENSTORM}: $RUNLENGTH hours      "  >     input_deepwater_header
echo "1  ${RUNLENGTH}  ${RunStartTimeHr} ${NumRecords} 1 "  >>    input_deepwater_header
echo "fort.64                              "  >>    input_deepwater_header
echo "1  1.0  1.0  1.0  1.0                "  >>    input_deepwater_header
echo "END OF LEG INFORMATION               "  >>    input_deepwater_header
echo "Specify iprint (# of time steps between outputs)   "  >>    input_deepwater_header
echo "1                                    "  >>    input_deepwater_header
echo "Scaling info for test case grid (MKS -> 1.0)       "  >>    input_deepwater_header
echo "1.00  1.00                           "  >>    input_deepwater_header
echo "Horizontal error, and minimum time step            "  >>    input_deepwater_header
echo "1.00  0.01                           "  >>    input_deepwater_header
echo "Scaling factors for drogue coordinates in x,y directions   "  >>    input_deepwater_header
echo "1.00  1.00                           "  >>    input_deepwater_header
echo "Number of starting drogues (ndr)     "  >>    input_deepwater_header
echo "${NumParticles}      ${Initial_Loc_File_base}      "  >>    input_deepwater_header

        cat input_deepwater_header  ${Initial_Loc_File} > input_deepwater.din

         rm -f ./convert_numbers.exe ./numberstoconvert ./convertednumbers ./input_deepwater_header 
