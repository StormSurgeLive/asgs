#!/usr/bin/bash

NAME=${1:-NOMESH}
WCPU=${2:-128}   # number of CPUs running computations

PPN=128          # platform specified number of cores per node (Procs Per Node)
NCPU=$((WCPU+1)) # + 1 global writer
NODES=1          # default
if [ $NCPU -gt $PPN ]; then
  NODES=$((NCPU / PPN))          # numbr of total compute nodes needed, need to compute this value NCPU/PPN
fi 
RUNDIR=$(pwd)/${NAME}-${NCPU}

mkdir -p $RUNDIR 

cp -v input/fort.* input/*.slurm $RUNDIR/

# gymnastics to use sed for updating file path:
SAFEPATH=$(echo "$RUNDIR" | sed "s/\//|/g")     # 1. turn / to | in path
sed -i "s/%RUNDIR%/$SAFEPATH/g" $RUNDIR/*.slurm # 2. do sed command to replace path with pipes
sed -i "s/|/\//g" $RUNDIR/*.slurm               # 3. replaces | with /

# update placeholders (queue template), in places
sed -i "s/%NCPU%/$NCPU/g" $RUNDIR/*.slurm
sed -i "s/%WCPU%/$WCPU/g" $RUNDIR/*.slurm
sed -i "s/%PPN%/$PPN/g" $RUNDIR/*.slurm
sed -i "s/%NODES%/$NODES/g" $RUNDIR/*.slurm

sbatch $RUNDIR/adcprep.slurm

while [ ! -e $RUNDIR/adcprep.done ]
do
  echo Waiting for adcprep phase to complete ...
  sleep 30
done

echo submitting padcirc.slurm

sbatch $RUNDIR/padcirc.slurm

while [ ! -e $RUNDIR/padcirc.done ]
do
  echo Waiting padcirc run to complete ...
  if [ -e $RUNDIR/padcirc.out ]; then
    tail -n 1 $RUNDIR/padcirc.out
  fi
  sleep 30
done

echo run finished
tail -n 3 $RUNDIR/padcirc.out
