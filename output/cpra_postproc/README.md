# Slide Deck Generation for Louisiana CPRA – Operator Guide
Document vintage 20210517

## Matthew Bilskie^1 and Peter Bacopoulos^2

1 University of Georgia | School of Environmental, Civil, Agriculture and Mechanical Engineering
2 Louisiana State University | Center for Coastal Resiliency

## Summary: This document provides a guide for Operators generating a slide deck for a tropical cyclone advisory, in support of Louisiana CPRA.

## Credits
The slide deck generation software was developed by Matthew Bilskie in coordination with Jason Fleming.

## Computing requirements
1. Matlab 2017
2. Python 2.7, including python-pptx
3. Bourne Again Shell (bash), including parse22.sh
4. FigureGen (dependencies on GMT, ghostscript, Fortran compiler)
5. PowerPoint

## Main programs
1. get_files_fortytwo_gahm.m
2. cpra_hydrograph_plotter.m
3. parse22.sh
4. buildPPT.py

## Preamble
The following instruction set is for an Operator running the software entirely within a Linux environment. The only Windows interaction in the entire process is with display and final (minor) update of the PowerPoint slide deck.

It is best practice to make a full copy of the directory from the previous cycle, delete the simulation files associated with the previous cycle (except for the consensus files of `*.fort.61.nc` and `*.run.properties`), and update and execute the scripts for the current cycle.

1. Identify the scenarios, ASGS instance and HPC for the current cycle. The slide deck Operator will check email for ADCIRC POSTED results corresponding to the current cycle.  At minimum, the slide deck Operator will require consensus scenarios for current and previous cycle.  In addition, more scenarios may be required, including: veerLeft (e.g., 50, 100, etc.); and veerRight (e.g., 50, 100, etc.).
2. Set the environmental variables on the local machine. Assuming that Anaconda has been built on the local machine (with the name py2 for python=2.7), activate Python 2.7, i.e., at the command line: conda activate py2. Load the module for Matlab 2017, i.e., at the command line: module load matlab/r2017a. (Pre-establish pathways and conda initialization in the Operator’s bashrc file.)
3. Update and execute `get_files_fortytwo_gahm.m`. Pre-step: Locate the mesh in the present working directory, naming the mesh according to the mesh variable within the script.  As well, generate an active link of the mesh file (`*.grd`), i.e., at the command line: `ln -fs [mesh file] fort.14`. Update script internals for adv (cycle), asgs_instance, hpc and en (scenarios by name).  Execute the script by typing at the command line: `matlab -nosplash -nodisplay -nodesktop -r “run ./get_files_fortytwo_gahm.m”`. Upon completion, the present working directory will fill up with simulation files for the scenarios associated with the current cycle, as specified within the script.
4. Update and execute `cpra_hydrograph_plotter.m`. Update script internals for `numEns` (number of scenarios), `ensFileNames` (file names of the `*.fort.61.nc` files) and `propFile` (file names of the `*.run.properties` files). When updating the `ensFileNames` and `propFile arrays` in the script, be sure that the scenarios are listed starting with the consensus scenario from previous cycle and ending with consensus scenario for the current cycle. Also, the Operator can modify the lower and upper limits of the y-axis of the plots, viz. refer to `minWL` and `maxWL`, respectively, in the general range of lines 600–650 of the script. Finally, execute the script by typing at the command line: `matlab -nosplash -nodisplay -nodesktop -r “run ./cpra_hydrograph_plotter.m”`. Upon completion, the present working directory will fill up with graphic (png) files for the stations associated with the storm surge analysis.
5. Generate 2D contour plots of maxele files with FigureGen. Work each scenario in sequence, ultimately carrying out the following sub-steps for all scenarios (an alternative approach employs `automate.sh [AdvisoryNumber]` to automate the sequential procedure):
    1. Generate storm track file from input meteorological forcing. Make an active link of the meteorological forcing file (`*.fort.22`), i.e., at the command line: `ln -fs [meteorological forcing file] fort.22`.  Execute the script by typing at the command line: `parse22.sh` (pre-establish pathways and script execution in the user’s bashrc file). The output is a storm track file (`fort.22.trk`) that is used as input to FigureGen.
    2. Update and execute FigureGen. Update the FigureGen input files for the given scenario:
        - Line 7: Alphanumeric label for these plots
        - Line 19: Name(s) of file(s) to use for contours
        - Execute FigureGen for the given input file.
        - The output is a graphic (jpg) file of the maxele associated with the given scenario.
6. Generate slide deck in PowerPoint format. Update the `*.run.properties` file associated with the consensus scenario for the current cycle by adding a line to the end of the file: `time.forecast.valid.cdt : 202009132200006` (adjust the date and time to correspond with the current advisory in GMT: YYYYMMDDHHmmss). Update the `buildPPT.py` script to open (read) the `*.run.properties` file associated with the current advisory. Execute the script by typing at the command line: `python buildPPT.py [FigureGenFilename] [nhcConsensusRunPropertiesFilename]`. The output is a PowerPoint (pptx) file of the slide deck.
7. Update the slide deck to include maxele graphics of other scenarios. By default, the slide deck contains one slide for the maxele of the consensus scenario for the current advisory. It is necessary to manually add slides for the other scenarios, e.g., veerLeft (e.g., 50, 100, etc.); and veerRight (e.g., 50, 100, etc.). Copy and paste the slide for consensus scenario, rename the title to associate with the other scenario (e.g., veerLeft100) and change the picture with the jpg maxele of the other scenario (e.g., veerLeft100). Repeat as necessary until all scenario maxele files are displayed in the slide deck. Save the PowerPoint file, overwriting with the same filename.

Running notes
```
https://repo.anaconda.com/archive/Anaconda3-2020.07-Linux-x86_64.sh
https://problemsolvingwithpython.com/01-Orientation/01.05-Installing-Anaconda-on-Linux/
easy_install-a.b pip
qsub -I walltime=02:00: nodes=1:ppn=20
alias idev=""
conda create --name py2 python=2.7
conda activate py2
conda config --set auto_activate_base false
conda install -c conda-forge python-pptx
https://anaconda.org/conda-forge/python-pptx
conda install -c anaconda pytz
/project/mbilskie
matlab -nosplash -nodisplay -nodesktop -r "run ./MyScript.m"
```