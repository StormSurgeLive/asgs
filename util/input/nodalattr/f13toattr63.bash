#!/bin/bash
#f13toattr63.bash
#
#Taylor Asher 2012-05-15

#input file name and file identifier
#filin=fort.13
#nanam="surface_directional_effective_roughness_length"
#filoutpat=SDERL
filin=$1                               #input file name
filoutpat=$2                              #output file name
nanam=$3                               #nodal attribute name
fidin=100                              #file ID assigned to input file
tmpfil=tmpf13toattr63.tmp              #name of temporary file to be used during run

echo "Begin $0"

#Open file
echo "Opening input file $filin"
eval "exec $fidin<$filin"

#Read in file info, number of nodes, and number of nodal attributes
#The "dum" part of the read is to ensure any additional info on the line is 
#written to a dummy variable
read <&$fidin filinfo
read <&$fidin nn dum
read <&$fidin na dum


#Loop to read in header areas of fort.13 file and identify number of values 
#per line in target attribute and default value
echo "Identifying target nodal attribute $nanam"
for ctr1 in $(seq 1 $na) ; do
   read <&$fidin curnanam                        #current nodal attribute name
   read <&$fidin curunits                        #current units
   read <&$fidin curnvals                        #current number of attribute values per node
   read <&$fidin curdefvals                      #current default value(s)

   #Assign "current" values to "permanent" variables if the target nodal
   #attribute is found
   if [[ "$curnanam" == $nanam ]] ; then
      units=$curunits
      nvals=$curnvals
      defvals=$curdefvals
   fi
done


#Turn single-line of default values into an array of default values
#This needs to be done if $nvals is greater than 1
ctr1=1
for i in $defvals ; do
   arr[$ctr1]=$i
   ctr1=$((ctr1+1))
done
defvals=("${arr[@]}")




#Get rid of $tmpfil if it exists, as we don't want anything to already be inside
if [ -f $tmpfil ] ; then
   echo "Removing pre-existing temp file $tmpfil"
   rm $tmpfil
fi
#Read through file until you find parameter of interest, then export nodal 
#attribute contents to a new temporary file, $tmpfil, for further processing
echo "Reading through input file $filin"
for ctr1 in $(seq 1 $na) ; do
   read <&$fidin curnanam                           #current nodal attribute name
   read <&$fidin curndifnod                         #current number of attribute that aren't the default value
   #If the nodal attribute name isn't the one you want, read through it to get 
   #to the next one.  If it is what you want, then read it and dump it to a 
   #temporary file $tmpfil, keeping the $ndifnod variable for later, then 
   #break out of the for loop since there's no need to keep reading the file
   if [[ "$curnanam" != $nanam ]] ; then
      for ctr2 in $(seq 1 $curndifnod) ; do
         read <&$fidin dum
      done
   else
      ndifnod=$curndifnod
      for ctr2 in $(seq 1 $ndifnod) ; do
#         read <&$fidin >>$tmpfil
         read <&$fidin dum
         echo $dum >>$tmpfil
      done
      break
   fi
done


#Open up output file(s), $nvals files total, and put fort.63-style header info 
#into each one.  Then, cut columns out of $tmpfil and put them into the target 
#sparse output file.  Lastly, run the inflate.x FORTRAN routine to inflate the
#sparse versions of the output files.  Sparse files are then deleted.  
for ctr1 in $(seq 1 $nvals) ; do
   sparsefilout[$ctr1]="sparse$filoutpat$ctr1.63"                              #name of sparse output file
   filout[$ctr1]="$filoutpat$ctr1.63"                                          #name of full output file
   #Write 3 fort.63-style header lines to files
   echo $filinfo $nanam > "${sparsefilout[$ctr1]}"
   echo "1 $nn 1 1 1" >> "${sparsefilout[$ctr1]}"
   echo "1 1 $ndifnod ${defvals[$((ctr1-1))]}" >> "${sparsefilout[$ctr1]}"
   #Grab 1st and $ctr1-th columns from the temporary nodal attribute file and 
   #spit them out to the sparse output file
   cut -d ' ' -f "1 $((ctr1+1))" $tmpfil >>"${sparsefilout[$ctr1]}"
   ./inflate.x ${sparsefilout[$ctr1]} ${filout[$ctr1]}                         #inflate the sparse output file to the full one
   rm ${sparsefilout[$ctr1]}
   echo ${sparsefilout[$ctr1]}
done
rm $tmpfil
echo "Done $0"
