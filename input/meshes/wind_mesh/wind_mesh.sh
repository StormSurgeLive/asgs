#!/bin/bash
#
#
# put together a mesh for a state
makeStateMesh() {
   # make shifted node table for hsofs
   echo "Making shifted node table."
   awk -v slon=$shiftLon -v slat=$shiftLat 'NR>2 && NR<1813446 { printf "%d   %.8f   %.8f   %.8f\n",$1,$2+slon,$3+slat,$4 } NR<3 { print $0 }' hsofs.14 > hsofs_${stateName}_node_table.txt
   #
   # combine into fort.14 file
   echo "Combining into ADCIRC mesh file."
   cat hsofs_${stateName}_node_table.txt hsofs_element_table.txt boundary_table_none.txt > hsofs_${stateName}.14
   #
   # polygon for state outline
   awk 'BEGIN { FS="," } NR>1 { print $1"  "$2 }' ${stateName}_20m.csv > ${stateName}_20m.poly
   #
   # cut out state-shaped mesh
   echo "Cutting out state-shaped mesh."
   ~/asgs/jasonfleming/wlo/output/resultScope.x --meshfile hsofs_${stateName}.14 --resultmeshfilename ${stateName}.14  --meshonly --resultshape polygon ${stateName}_20m.poly
   #
   # convert to netcdf and generate XDMF for visualization
   echo "Converting for visualization."
   ~/asgs/jasonfleming/wlo/output/adcirc2netcdf.x --meshfile ${stateName}.14 --meshonly
   ~/asgs/jasonfleming/wlo/output/generateXDMF.x --datafile ${stateName}.14.nc --meshonly
}
#
# copy element table
#echo "Copying element table."
#awk 'NR>=1813446 && NR<5377550 { print $0}' hsofs.14 > hsofs_element_table.txt
#
# make boundary table (specifying that there are no boundaries)
printf "0\n0\n0\n0\n" > boundary_table_none.txt
#
#
# command used to reformat data extracted from kml (after clean up 
# to remove xml tags)
# awk '{ for(i=1;i<=NF;i++){printf "%s\n",$i} }' texas_20m.dat > texas_20m.csv
#
# make texas state mesh
stateName="texas"
shiftLon="-34.0"
shiftLat="0.0"
makeStateMesh
#
# make florida state mesh
stateName="florida"
shiftLon="2.0"
shiftLat="2.0"
makeStateMesh
#
# make louisiana state mesh
stateName="louisiana"
shiftLon="0.0"
shiftLat="3.0"
makeStateMesh
#
# make north carolina state mesh
stateName="northcarolina"
shiftLon="-10.0"
shiftLat="0.0"
makeStateMesh
#
# make agglomerated mesh
echo "# wind mesh" > wind_mesh.14
agglomeratedNodes=0
agglomeratedElements=0
if [[ -e agglomerated_node_table.txt ]]; then
   rm agglomerated_node_table.txt
fi
if [[ -e agglomerated_element_table.txt ]]; then
   rm agglomerated_element_table.txt
fi
#
for stateName in texas louisiana florida northcarolina ; do
   #
   # pull out node table
   stateNodes=`head -n 2 ${stateName}.14 | awk 'NR==2 { print $2 }'`
   awk -v nodes=$stateNodes -v allnodes=$agglomeratedNodes 'NR>2 && NR<(nodes+3) { printf "%d   %.8f   %.8f   %.8f\n",$1+allnodes,$2,$3,$4 }' ${stateName}.14 >> agglomerated_node_table.txt
   #
   # pull out element table
   stateElements=`head -n 2 ${stateName}.14 | awk 'NR==2 { print $1 }'`
   awk -v elements=$stateElements -v allelements=$agglomeratedElements -v nodes=$stateNodes -v allnodes=$agglomeratedNodes 'NR>=(nodes+3) && NR<(nodes+3+elements) { printf "%d   %d   %d   %d   %d\n",$1+allelements,$2,$3+allnodes,$4+allnodes,$5+allnodes }' ${stateName}.14 >> agglomerated_element_table.txt
   #
   # increment total number of nodes and elements
   agglomeratedNodes=`expr $agglomeratedNodes + $stateNodes`
   agglomeratedElements=`expr $agglomeratedElements + $stateElements`

done
#
# write out agglomerated mesh file
echo "$agglomeratedElements  $agglomeratedNodes" >> wind_mesh.14
cat agglomerated_node_table.txt >> wind_mesh.14
cat agglomerated_element_table.txt >> wind_mesh.14
cat boundary_table_none.txt >> wind_mesh.14
#
# convert to netcdf and generate XDMF for visualization
echo "Converting for visualization."
~/asgs/jasonfleming/wlo/output/adcirc2netcdf.x --meshfile wind_mesh.14 --meshonly
~/asgs/jasonfleming/wlo/output/generateXDMF.x --datafile wind_mesh.14.nc --meshonly 
 
