Kickstarting from a nowcast hotstart

Kickstarting ASGS means running a forecast simulation from a hotstart file NOT computed by 
the current instance of ASGS.  In other words, you want to (e.g.) start a forecast cycle on 
a cluster, but the needed hotstart (presumably a nowcast from ASGS on a different cluster 
or from a different instance of ASGS on the local cluster) is not available in the current ASGS instance.  

The process is as follows: 
1. on the target machine, create a directory called kickstart (actual name and location is irrelevant). But let''s pretend it is /scratch/bblanton/kickstart.
2. in kickstart, create nowcast/PE0000
3. put the fort.67 from another ASGS instance in this directory
4. in an $ASGS/config/2018/<ThisConfig>.sh file, set:
    1. HOTCOLDSTART='hotstart'
    2. COLDSTARTDATE=<coldstartdate of the kickstart run>
    3. LASTSUBDIR= /scratch/bblanton/kickstart

That''s it.  ASGS will look for a hotstart file in  /scratch/bblanton/kickstart/nowcast/PE0000.   

You can see that it could be useful to have an ASGS instance always updating a nowcast, and that various forecasts could be started using the same hotstart but in different ASGS instances or on different compute resources. 
