#!/bin/bash
#
#----------------------------------------------------------------------------
#                    S L U R M   D I R E C T I V E S
#----------------------------------------------------------------------------
#SBATCH -J cera_contour
#SBATCH -t 20
#SBATCH -n 1
#SBATCH --partition=ncfs
#SBATCH --reservation=ncfs
#SBATCH --array=1-168
#SBATCH -o /projects/ncfs/data/tests/cera_contour/cera_contour.out
#
#----------------------------------------------------------------------------
#        L O G   M E S S A G E S   T O   S T A R T   T H E   J O B
#----------------------------------------------------------------------------
case $SLURM_ARRAY_TASK_ID in
   1)
   DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
   echo "[${DATETIME}] INFO: SLURM Job ID ${SLURM_JOBID}; SLURM submit directory ${SLURM_SUBMIT_DIR}; and SLURM submit host ${SLURM_SUBMIT_HOST}." 
   #
   hostname | tee CONTROL.TXT
   echo "Job Run on Nodes"  | tee --append CONTROL.TXT
   echo "----------------"  | tee --append CONTROL.TXT
   echo $SLURM_JOB_NODELIST | tee --append CONTROL.TXT
   echo "----------------"  | tee --append CONTROL.TXT
   ;;
   *)
   # all other tasks don't need to write these log messages
   ;;
esac
#
#----------------------------------------------------------------------------
#                      L O A D   M O D U L E S
#----------------------------------------------------------------------------
module load intelc/14.0.3  
module load intelfort/14.0.3
module load netcdf/4.1.3_intel-14.0.3
module load mvapich2/2.0_intel-14.0.3_ch3_ofed-3.1  
# for cera_contour.py
module load gdal/1.11.1_gcc
source /projects/ncfs/apps/kalpana/env/bin/activate
# 
#----------------------------------------------------------------------------
#                  E X E C U T E   T H E   J O B
#----------------------------------------------------------------------------
cd /projects/ncfs/data/tests/cera_contour
DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
taskID=`printf "%03d" $SLURM_ARRAY_TASK_ID` 
echo "$DATETIME start" > task${taskID}.log
#
case $SLURM_ARRAY_TASK_ID in
   1 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 1 -s -o wvel001
   ;;
   2 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 2 -s -o wvel002
   ;;
   3 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 3 -s -o wvel003
   ;;
   4 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 4 -s -o wvel004
   ;;
   5 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 5 -s -o wvel005
   ;;
   6 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 6 -s -o wvel006
   ;;
   7 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 7 -s -o wvel007
   ;;
   8 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 8 -s -o wvel008
   ;;
   9 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 9 -s -o wvel009
   ;;
   10 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 10 -s -o wvel010
   ;;
   11 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 11 -s -o wvel011
   ;;
   12 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 12 -s -o wvel012
   ;;
   13 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 13 -s -o wvel013
   ;;
   14 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 14 -s -o wvel014
   ;;
   15 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 15 -s -o wvel015
   ;;
   16 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 16 -s -o wvel016
   ;;
   17 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 17 -s -o wvel017
   ;;
   18 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 18 -s -o wvel018
   ;;
   19 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 19 -s -o wvel019
   ;;
   20 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 20 -s -o wvel020
   ;;
   21 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 21 -s -o wvel021
   ;;
   22 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 22 -s -o wvel022
   ;;
   23 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 23 -s -o wvel023
   ;;
   24 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 24 -s -o wvel024
   ;;
   25 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 25 -s -o wvel025
   ;;
   26 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 26 -s -o wvel026
   ;;
   27 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 27 -s -o wvel027
   ;;
   28 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 28 -s -o wvel028
   ;;
   29 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 29 -s -o wvel029
   ;;
   30 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 30 -s -o wvel030
   ;;
   31 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 31 -s -o wvel031
   ;;
   32 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 32 -s -o wvel032
   ;;
   33 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 33 -s -o wvel033
   ;;
   34 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 34 -s -o wvel034
   ;;
   35 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 35 -s -o wvel035
   ;;
   36 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 36 -s -o wvel036
   ;;
   37 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 37 -s -o wvel037
   ;;
   38 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 38 -s -o wvel038
   ;;
   39 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 39 -s -o wvel039
   ;;
   40 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 40 -s -o wvel040
   ;;
   41 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 41 -s -o wvel041
   ;;
   42 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 42 -s -o wvel042
   ;;
   43 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 43 -s -o wvel043
   ;;
   44 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 44 -s -o wvel044
   ;;
   45 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 45 -s -o wvel045
   ;;
   46 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 46 -s -o wvel046
   ;;
   47 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 47 -s -o wvel047
   ;;
   48 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 48 -s -o wvel048
   ;;
   49 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 49 -s -o wvel049
   ;;
   50 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 50 -s -o wvel050
   ;;
   51 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 51 -s -o wvel051
   ;;
   52 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 52 -s -o wvel052
   ;;
   53 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 53 -s -o wvel053
   ;;
   54 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 54 -s -o wvel054
   ;;
   55 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 55 -s -o wvel055
   ;;
   56 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 56 -s -o wvel056
   ;;
   57 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 57 -s -o wvel057
   ;;
   58 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 58 -s -o wvel058
   ;;
   59 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 59 -s -o wvel059
   ;;
   60 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 60 -s -o wvel060
   ;;
   61 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 61 -s -o wvel061
   ;;
   62 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 62 -s -o wvel062
   ;;
   63 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 63 -s -o wvel063
   ;;
   64 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 64 -s -o wvel064
   ;;
   65 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 65 -s -o wvel065
   ;;
   66 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 66 -s -o wvel066
   ;;
   67 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 67 -s -o wvel067
   ;;
   68 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 68 -s -o wvel068
   ;;
   69 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 69 -s -o wvel069
   ;;
   70 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 70 -s -o wvel070
   ;;
   71 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 71 -s -o wvel071
   ;;
   72 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 72 -s -o wvel072
   ;;
   73 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 73 -s -o wvel073
   ;;
   74 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 74 -s -o wvel074
   ;;
   75 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 75 -s -o wvel075
   ;;
   76 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 76 -s -o wvel076
   ;;
   77 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 77 -s -o wvel077
   ;;
   78 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 78 -s -o wvel078
   ;;
   79 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 79 -s -o wvel079
   ;;
   80 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 80 -s -o wvel080
   ;;
   81 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 81 -s -o wvel081
   ;;
   82 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 82 -s -o wvel082
   ;;
   83 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 83 -s -o wvel083
   ;;
   84 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.74.nc -t 84 -s -o wvel084
   ;;
   85 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 1 -s -o elev001
   ;;
   86 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 2 -s -o elev002
   ;;
   87 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 3 -s -o elev003
   ;;
   88 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 4 -s -o elev004
   ;;
   89 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 5 -s -o elev005
   ;;
   90 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 6 -s -o elev006
   ;;
   91 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 7 -s -o elev007
   ;;
   92 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 8 -s -o elev008
   ;;
   93 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 9 -s -o elev009
   ;;
   94 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 10 -s -o elev010
   ;;
   95 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 11 -s -o elev011
   ;;
   96 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 12 -s -o elev012
   ;;
   97 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 13 -s -o elev013
   ;;
   98 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 14 -s -o elev014
   ;;
   99 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 15 -s -o elev015
   ;;
   100 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 16 -s -o elev016
   ;;
   101 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 17 -s -o elev017
   ;;
   102 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 18 -s -o elev018
   ;;
   103 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 19 -s -o elev019
   ;;
   104 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 20 -s -o elev020
   ;;
   105 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 21 -s -o elev021
   ;;
   106 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 22 -s -o elev022
   ;;
   107 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 23 -s -o elev023
   ;;
   108 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 24 -s -o elev024
   ;;
   109 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 25 -s -o elev025
   ;;
   110 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 26 -s -o elev026
   ;;
   111 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 27 -s -o elev027
   ;;
   112 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 28 -s -o elev028
   ;;
   113 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 29 -s -o elev029
   ;;
   114 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 30 -s -o elev030
   ;;
   115 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 31 -s -o elev031
   ;;
   116 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 32 -s -o elev032
   ;;
   117 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 33 -s -o elev033
   ;;
   118 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 34 -s -o elev034
   ;;
   119 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 35 -s -o elev035
   ;;
   120 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 36 -s -o elev036
   ;;
   121 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 37 -s -o elev037
   ;;
   122 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 38 -s -o elev038
   ;;
   123 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 39 -s -o elev039
   ;;
   124 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 40 -s -o elev040
   ;;
   125 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 41 -s -o elev041
   ;;
   126 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 42 -s -o elev042
   ;;
   127 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 43 -s -o elev043
   ;;
   128 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 44 -s -o elev044
   ;;
   129 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 45 -s -o elev045
   ;;
   130 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 46 -s -o elev046
   ;;
   131 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 47 -s -o elev047
   ;;
   132 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 48 -s -o elev048
   ;;
   133 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 49 -s -o elev049
   ;;
   134 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 50 -s -o elev050
   ;;
   135 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 51 -s -o elev051
   ;;
   136 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 52 -s -o elev052
   ;;
   137 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 53 -s -o elev053
   ;;
   138 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 54 -s -o elev054
   ;;
   139 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 55 -s -o elev055
   ;;
   140 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 56 -s -o elev056
   ;;
   141 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 57 -s -o elev057
   ;;
   142 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 58 -s -o elev058
   ;;
   143 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 59 -s -o elev059
   ;;
   144 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 60 -s -o elev060
   ;;
   145 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 61 -s -o elev061
   ;;
   146 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 62 -s -o elev062
   ;;
   147 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 63 -s -o elev063
   ;;
   148 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 64 -s -o elev064
   ;;
   149 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 65 -s -o elev065
   ;;
   150 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 66 -s -o elev066
   ;;
   151 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 67 -s -o elev067
   ;;
   152 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 68 -s -o elev068
   ;;
   153 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 69 -s -o elev069
   ;;
   154 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 70 -s -o elev070
   ;;
   155 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 71 -s -o elev071
   ;;
   156 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 72 -s -o elev072
   ;;
   157 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 73 -s -o elev073
   ;;
   158 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 74 -s -o elev074
   ;;
   159 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 75 -s -o elev075
   ;;
   160 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 76 -s -o elev076
   ;;
   161 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 77 -s -o elev077
   ;;
   162 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 78 -s -o elev078
   ;;
   163 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 79 -s -o elev079
   ;;
   164 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 80 -s -o elev080
   ;;
   165 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 81 -s -o elev081
   ;;
   166 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 82 -s -o elev082
   ;;
   167 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 83 -s -o elev083
   ;;
   168 )
python /home/ncfs/asgs/contrib/CarolaKaiser/CERA_ASGS/contouring/cera_contour.py -i fort.63.nc -t 84 -s -o elev084
   ;;
   * )
   echo nothingtodo
   ;;
esac

DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
echo "$DATETIME end" >> task${taskID}.log
#
if [[ $SLURM_ARRAY_TASK_ID -eq 170 ]]; then
  DATETIME=`date +'%Y-%h-%d-T%H:%M:%S'`
  echo "[${DATETIME}] INFO: cera_contour.py finished in /projects/ncfs/data/tests/cera_contour with return value = $ERROVALUE" 
fi
