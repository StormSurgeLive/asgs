# Resolution Enhancement

This directory contains files and bash scripts for running the GRASS GIS enhanced resolution scripts in real-time. It is designed to automatically check for and download ADCIRC output from the RENCI archives as they are posted. The following is an overview of the purpose and functionality of each file. Details of this workflow can be found at https://ccht.ccee.ncsu.edu/kalpana/.

## ncsu-viz-config.sh

This file contains two important lines of information: startDateTime and addressList. startDateTime is the date-time string with format YYYYMMDDHHMMSS (i.e. June 22, 2018 at 2:30 PM would be 20180622143000) that is used to specify which ADCIRC output to begin with. The current date-time would be specified for all output archived in the future, but an earlier date-time can be used to begin processing earlier advisories/ensembles. The addressList is the list of email recipients of the guidance. It is best to put your own email in the addressList line, and all other recipients in the bccAddressList line.

## ncsu-viz-mailer.sh

This is the main file used to run the script. Once invoked, it will run continuously until it is canceled or encounters an error. It should be run by opening a linux screen using the `screen` command, and is invoked via the following:
```
bash ncsu-viz-mailer.sh -c ncsu-viz-config.sh
```
To kill the process, use `Ctrl+c`. To leave the screen but keep it active, use `Ctrl+a+d`. To return to an existing screen, type `screen -r`. Job progress output is written to a log file in the logs directory.

The bash script as currently configured requires several files to be present in the working directory:

* `GRASS_LOCATION.zip` - contains GRASS directory

* `*something*-enhanced.zip` - this is the final result that is output after each run and emailed to recipients. However, it is deleted during the next run, so there needs to be some file in the directory ending in "-enhanced.zip" before submitting the bash script so there is something to delete.

* `rgrow_pll.started` `rgrow_pll.finished` - files that are created and overwritten during each run. These files also need to be present at the start of each run for the same reason as above.

* `weights-*mesh*-north_carolina.txt` - file of precomputed IDWs. Necessary now, hopefully not in the future. It is archived and compressed right now, will have to be unpacked using tar command.

In Pyscripts directory:

* `interpolate.py` - script to interpolate maxele.63.nc from points to raster.

* `import.py` - script to import raster into GRASS and set geographic region.

* `r.grow.modified.py` - the actual r.grow GRASS module that has been modified to accept DEM and perform elevation check.

* `grow_process.py` - script that removes isolated, non-hydraulically-connected cells from raster.

* `export.py` - copies subregion, parallel rasters into a new directory and patches them together to create the final result raster, then exports it from GRASS.

The parallel job is managed by a Fortran code, given in `rgrow_pll.f90`, which automatically ditributes the job on the number of cores specified in the `rgrow_pll.csh` file. The Python scripts above are called from this script and submitted in parallel on smaller subregions. The r.grow radius is specified in this file. If changes are made to the f90 file, it needs to be recompiled.

## rgrow_pll.inp
The rgrow_pll.inp file is created automatically and consists of the following list of strings:
```
*region* *storm* *advisory* *mesh* *ensemble* maxele *subtractionKey* *datumConv*
```
where subtractionKey is 'with' or 'without' and is specified in ncsu-viz-mailer.sh and datumConv should always be 'no'.

The `rgrow_pll.hist` file contains a list of all completed jobs, indicating storm, advisory, mesh and ensemble info.

# References

CA Rucker, N Tull, JC Dietrich, TE Langan, H Mitasova, BO Blanton, JG Fleming, RA Luettich Jr (2021). “Downscaling of Real-Time Coastal Flooding Predictions for Decision Support.” Natural Hazards, DOI: 10.1007/s11069-021-04634-8.

CA Rucker (2020). “Improving the Accuracy of a Real-Time ADCIRC Storm Surge Downscaling Model,” North Carolina State University.

N Tull (2018). “Improving Accuracy of Real-Time Storm Surge Inundation Predictions,” North Carolina State University.
