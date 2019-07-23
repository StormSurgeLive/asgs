!----------------------------------------------------------------------!
!
!                            FIGUREGEN
!                       By: Casey Dietrich
!
!  FigureGen is a Fortran program that creates images for ADCIRC files. 
!  It reads mesh files (fort.14, etc.), nodal attributes files
!  (fort.13, etc.) and output files (fort.63, fort.64, maxele.63, etc.). 
!  It plots contours, contour lines, and vectors. Using FigureGen, you 
!  can go directly from the ADCIRC input and output files to a 
!  presentation-quality figure, for one or multiple time snaps.
!
!  This program started from a script written by Brian Blanton, 
!  and I converted it to Fortran because I am more familiar with that 
!  language. It now contains code written by John Atkinson, Zach Cobell, 
!  Howard Lander, Chris Szpilka, Matthieu Vitse, Matthew Bilskie, and others.  
!  But, at its core, FigureGen behaves like a script, and it uses system 
!  calls to tell other software how to generate the figure(s).
!
!----------------------------------------------------------------------!
!
!------------------------------------------------------------------!
!...This file is used to control the options used when compiling
!   FigureGen. Instead of adding options at the command line such 
!   as -DNETCDF, you can define options here and only include
!   the option -DHAVE_CONFIG. Options may be included by removing
!   the '!' in front of each #define line.
!------------------------------------------------------------------!
!------------------------------------------------------------------!
!
!
!------------------------------------------------------------------!
! P L A I N  M A P  S T Y L I N G
!...Generate a more plain looking map using GMT than the
!   traditional "fancy" border using GMT.
!#define PLAIN
!------------------------------------------------------------------!
!
!
!------------------------------------------------------------------!
! C O L O R  B A R  T R I A N G L E S
!...Add triangles on the top and bottom of the color scale to 
!   signal that the plotted value is outside of the range that is
!   displaced by the color bar.
!#define CBARLIMIT
!------------------------------------------------------------------!
!
!
!------------------------------------------------------------------!
! D E P T H  D I F E R E N C E S 
!...Instead of plotting a value outside of the range of the plot
!   when differencing the inundation limits, use the difference in
!   depth for newly inundated areas
!#define DEPTHDIFF
!------------------------------------------------------------------!
!
!
!------------------------------------------------------------------!
! N E T C D F
!...Compile using netCDF libraries to enable ADCIRC netCDF
!   formatted files for use
!#define NETCDF
!------------------------------------------------------------------!
!
!
!------------------------------------------------------------------!
! N E T C D F  D E B U G G I N G
!...Compile with options to help debug netCDF. This option will 
!   intentionally cause a segmentation fault to find where the
!   code goes wrong. This is usful when combined with options for 
!   traceback (-traceback,-fbacktrace) and symbolic debugging (-g).
!#define NETCDF_DEBUG
!------------------------------------------------------------------!
!
!
!------------------------------------------------------------------!
! D R Y  D I F F E R E N C E S
!...When differencing a set of files, two dry values will normally
!   not be recognized and treated in a special way, and the
!   difference will show as a zero value. This option will plot
!   two identical values of "dry" as the "dry" color in the plot
!#define DRYDIFF
!
!------------------------------------------------------------------!
!------------------------------------------------------------------!
! D R Y  V E L O C I T Y  V A L U E S
!...Consider a velocity of zero to be a dry value. ADCIRC
!   does not write dry values in velocity files, so for 
!   plotting water velocities, this should be enabled for
!   nicer plots
!#define DRYZEROVEL
!------------------------------------------------------------------!
!
!
!------------------------------------------------------------------!
! M P I  M E S S A G E  P A S S I N G  ( P A R A L L E L )
!...Use Parallel message passing to use multiple processors to
!   generate plots on more than one processor at a time. The
!   parallel scheme uses n-1 processors to generate plots, so the 
!   minimum number of processors that can be used with this option
!   is 2. Using 2 processors will replicate a serial run, so using
!   more than two processors is recommended.
!#define CMPI
!------------------------------------------------------------------!
!
!
!------------------------------------------------------------------!
! S L O W R E A D
!...Read the ADCIRC ASCII output files explicitly instead of
!   attempting to shortcut through the file using more generic
!   reads. This option is generally not used.
!#define SLOWREAD
!------------------------------------------------------------------!
