#!/bin/bash

# This script captures the general information in the adjacent README on how to archive storms
# from the NHC archive site

# 1. if a directory for the year of the storm has not been created, do so under asgs/sample_input
# 2. cd in the $YEAR
# 3. run this script, for example to capture all of the advisories for Dorian (Storm 05, year 2019):
#   cd 2019
#   ../get_all.sh 5 2019 1 64
# NOTE: there is an attempt to guess the storm name using some imperfect sed/awk magic, but you will
#       get a chance to correct this right before it moves all fetched files into the storm directory

if [ -z "$4" ]; then
  echo script requires 4 arguments: stormNum fullYear firstAdvisoryNum lastAdvisoryNum
  exit 1
fi

storm=$1        # does not expect a leading 0 for single digit storm numbers
year=$2
advisoryMin=$3
advisoryMax=$4

# get all advisories for $storm

../get_advisories.sh $advisoryMin $advisoryMax $storm $year al

# get BEST track (this URL is for storms from previous years, not the current year)
fstorm=$(printf "%02d" $storm)
bestName=bal${fstorm}${year}.dat
echo "wget https://ftp.nhc.noaa.gov/atcf/archive/${year}/${bestName}.gz"
wget https://ftp.nhc.noaa.gov/atcf/archive/${year}/${bestName}.gz
gunzip ${bestName}.gz

advisory=$advisoryMin
while [[ $advisory -le $advisoryMax ]]; do
    advisoryNum=$(printf "%03d\n" $advisory)
    echo "perl ../advisory_xml_filler.pl --input al${fstorm}${year}.fstadv.$advisoryNum.shtml --template ../template.index.xml --best bal${fstorm}${year}.dat"
    perl ../advisory_xml_filler.pl --input al${fstorm}${year}.fstadv.$advisoryNum.shtml --template ../template.index.xml --best bal${fstorm}${year}.dat 2> /dev/null
    advisory=$(expr $advisory + 1)
done

tmpName=$(grep --no-filename "Forecast\/Advisory Number" *.xml | sed 's/<title>//g' | sed 's/<\/title>//g' | awk '{for(i=NF;i>0;--i)printf "%s%s",$i,(i>1?OFS:ORS)}' | awk '{print $4}' | tail -n 1)
read -p "Directory name? [$tmpName]" dirName
if [ -z "$dirName" ]; then
  dirName=$tmpName
fi
mkdir ./$dirName
mv *.dat *.xml ./$dirName
echo cleaning up...
rm *.shtml
echo "to save to repo, run 'git add ./$dirName && git commit -a -m \"added advisories for $dirName $storm $year\"'"