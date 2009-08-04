/*
 * Copyright (c) 2008  Renaissance Computing Institute. All rights reserved.
 *
 * This software is open source. See the bottom of this file for the license.
 * 
 * Renaissance Computing Institute, 
 * (A Joint Institute between the University of North Carolina at Chapel Hill,
 * North Carolina State University, and Duke University)
 * http://www.renci.org
 * 
 * For questions, comments please contact software@renci.org
 *
 */

/*
   This code optionally uses the netcdf header file and library.  Assuming your
   netcdf install is in /opt/netcdf/3.6.2, you can use this command line
   for compilation:

    gcc -Wall -DUSE_NETCDF -o FindMax FindMax.c -I/opt/netcdf/3.6.2/include -L/opt/netcdf/3.6.2/lib -lnetcdf

    You can compile the code to emit some debugging information like so:

    gcc -Wall -DUSE_NETCDF -DDEBUG -o FindMax FindMax.c -I/opt/netcdf/3.6.2/include -L/opt/netcdf/3.6.2/lib -lnetcdf
    
    To build without the netcdf library:

    gcc -Wall -o FindMax FindMax.c
*/


#include <stdio.h>
#include <string.h>
#include <getopt.h>
#include <stdlib.h>
#ifdef USE_NETCDF
#include <netcdf.h>
#endif

#define MAX_LENGTH 256
#ifndef TRUE
# define TRUE    1
# define FALSE   0
#endif

void Usage (char *progName) {
   char *message;
   message  = 
"Usage:\n\
\n\
FindMax  --north=north --south=south --east=east --west=west \n\
         --dataFile=dataFile --highClip=value --lowClip=value\n\
         --gridFile=gridFile --fort63Mode=mode [--scale=scale] \n\
\n\
FindMax find the minimim and maximum values for an ADCIRC fort.63\n\
data set. The minimum and maximum values are calculated only in the \n\
portion of the data set inside the bounding box formed by the north, \n\
south, east and west values. The high and low clip values eliminate \n\
from consideration values outside a specified range. The optional scale \n\
parameter is used to scale the min and max values returned: The default \n\
scale value is 1. The min and max values are printed on standard output\n\
delimited by a space.\n\
\n\
The data set can be provided in either netCDF or ascii format.\n\
The data set must include both the values at each point and the grid \n\
coordinates of each point. If the data set is provided in netCDF, then \n\
the netCDF file (specified by --dataFile) must contain the grid info and \n\
the data. Otherwise the grid info should be specified with --gridFile and \n\
the data file should be specified with --dataFile. The --fort63mode  \n\
parameter controls how the data is specified.  The following modes are \n\
supported:\n\
\n\
\tnetCDFScoop: the grid and value data are in a netCDF file that conforms \n\
\t\tto that used in the ADCIRC implementation of the SURA Coastal \n\
\t\tOcean Observing and Prediction (SCOOP) Program.\n\
\n\
\tcompact: The data values are contained in the fort.63 formatted file\n\
\t\tspecified by the --dataFile parameter.  Nodes without valid data\n\
\t\tare not represented in the file. The nodes composing the grid\n\
\t\tare contained in the file specified by the --gridFile parameter.\n\
\n\
\tfull: The data values are contained in the fort.63 formatted file\n\
\t\tspecified by the --dataFile parameter.  All nodes are\n\
\t\trepresented in the file. The nodes composing the grid \n\
\t\tare contained in the file specified by the\n\
\t\t--gridFile parameter.\n\
\n\
The arguments are:\n\
\n\
\t--help: Print this message.\n\
\t--north: The northern extent of the bounding box.\n\
\t--south: The southern extent of the bounding box.\n\
\t--east: The eastern extent of the bounding box.\n\
\t--west: The western extent of the bounding box.\n\
\t--dataFile: The file containing the data values on which\n\
\t\tto calculate the minimum and maximum.\n\
\t--highClip: The upper bound of values considered in the max calculation.\n\
\t--lowClip: The lower bound of values considered in the min calculation.\n\
\t--gridFile: The file containing the grid on which to calculate the\n\
\t\tthe minimum and maximum.\n\
\t--fort63Mode: The mode for the input data, as discussed above.\n\
\t--scale: A scale factor (default 1.0) applied to the min and max before\n\
\t\tthey are printed.";
   printf ("%s\n", message);
}

int buildCliplist(int **clipList, 
                  char *gridFile, 
                  char *gridFileMode,
                  float north,
                  float south,
                  float east,
                  float west) {
    FILE  *GRID_FILE;
    char   line[MAX_LENGTH];
    int    lineCtr = 0;
    int    i;
    double thisLat;
    double thisLon;
    int    nTrue = 0;
#ifdef USE_NETCDF
    int    latid;
    int    lonid;
    int    ncid;
    int    dimid;
    int    error;
    long    count = 1;
    long    start;
#endif

    if (!strcasecmp(gridFileMode, "netCDFScoop")) {
#ifdef USE_NETCDF
       // The file is an adcirc netCDF file following the "SCOOP" format.
       error = nc_open(gridFile, NC_NOCLOBBER, &ncid);
       if (error != NC_NOERR) {
          printf("could not open %s\n", gridFile);
          exit (1);
       }

       // The number of points in the grid is in a dimension called "node"
       error = nc_inq_dimid(ncid, "node", &dimid);
       if (error != NC_NOERR) {
          printf("could not retrieve dimension for node\n");
          exit (1);
       }

       error = nc_inq_dimlen(ncid, dimid, (size_t *)&lineCtr);
       if (error != NC_NOERR) {
          printf("could not retrieve length for node\n");
          exit (1);
       }

       // Get the lat and long var ids
       error = nc_inq_varid(ncid, "lat", &latid);
       if (error != NC_NOERR) {
          printf("could not retrieve dimension for lat\n");
          exit (1);
       }

       error = nc_inq_varid(ncid, "lon", &lonid);
       if (error != NC_NOERR) {
          printf("could not retrieve dimension for lon\n");
          exit (1);
       }
#endif      
    } else {
       GRID_FILE = fopen(gridFile, "r");
       if (GRID_FILE == NULL) {
          printf("Can't open file %s\n", gridFile);
          exit (1);
       }
       // Let's start by counting the number of lines in the file, so we can
       // malloc clipList.
       while ( fgets(line, MAX_LENGTH, GRID_FILE) != NULL) {
          lineCtr++;
       }
        // Now back to the beginning of the file...
       rewind(GRID_FILE);
    }

#if defined DEBUG
    printf("Count of lines in %s is %d\n", gridFile, lineCtr);
#endif


    *clipList = (int *) malloc(sizeof(int) * lineCtr);
    for (i = 0; i < lineCtr; i ++) {
       // Read each line of the grid file to see if it is in the
       // bounding box the user provided.  If so, set the corresponding
       // entry in clipList to TRUE, otherwise set it to FALSE.
       // If the file is a netCDF file get the data that way.
       if (!strcasecmp(gridFileMode, "netCDFScoop")) {
#ifdef USE_NETCDF
          // The grid is in variables called lat and long
          start = i;
          error = nc_get_vara_double(ncid, lonid, &start, &count, &thisLon);
          if (error != NC_NOERR) {
              printf("could not retrieve lon\n");
              exit (1);
          }
          error = nc_get_vara_double(ncid, latid, &start, &count, &thisLat);
          if (error != NC_NOERR) {
              printf("could not retrieve lat\n");
              exit (1);
          }
#endif
      } else {
         fscanf(GRID_FILE, "%lf %lf\n", &thisLon, &thisLat);
      }
#if defined DEBUG
      if (i == 10) {
         printf("thisLon %d %.16lf thisLat %.16lf\n", i, thisLon, thisLat);
      }
#endif

      if ((thisLat <= north) && (thisLat >= south) &&
          (thisLon >= west)  && (thisLon <= east)) {
         (*clipList)[i] = TRUE;
         nTrue++;
      } else {
         (*clipList)[i] = FALSE;
      }  
    }
#if defined DEBUG
    printf("Found %d nodes in north %f, south %f, east %f west %f\n",
            nTrue, north, south, east, west);
    printf("clipList of 100 is %d\n", (*clipList)[100]);
#endif
    if (!strcasecmp(gridFileMode, "netCDFScoop")) {
#ifdef USE_NETCDF
       // Close the file
       nc_close(ncid);
#endif
    }
    // Return the number of nodes in the grid.
    return (lineCtr);    
}
int main (int argc, char *argv[]) {

    float  north;
    float  south;
    float  east;
    float  west;
    float  highClip = 100;
    float  lowClip = -100;
    char   gridFile[MAX_LENGTH];
    char   dataFile[MAX_LENGTH];
    char   line[MAX_LENGTH];
    char   fort63Mode[MAX_LENGTH];
    char   progName[MAX_LENGTH];
    FILE  *DATA_FILE;
    int   *clipList;
    int    c = 0;
    int    option_index = 0;
    int    nDataThisRecord;
    float  timeFloat;
    int    timeInt;
    float  noData;
    int    lineCtr;
    int    nSteps;
    int    i,j;
    float  value;
    int    thisNode;
    float  min, max;
#ifdef USE_NETCDF
    int    ncid;
    int    dimid;
    int    zetaid;
    int    error;
    long   startDims[2];
    long   countDims[2];
#endif
    float  scale=1.0;

    static struct option long_options[] =
    {
       {"north", required_argument, 0, 'n'},
       {"south", required_argument, 0, 's'},
       {"east", required_argument, 0, 'e'},
       {"west", required_argument, 0, 'w'},
       {"dataFile", required_argument, 0, 'd'},
       {"fort63Mode", required_argument, 0, 'f'},
       {"highClip", required_argument, 0, 'h'},
       {"lowClip", required_argument, 0, 'l'},
       {"scale", required_argument, 0, 'a'},
       {"help", no_argument, 0, 'p'},
       {"gridFile", required_argument, 0, 'g'}
    };

    while (c != -1) {
       c = getopt_long (argc, argv, "n:s:e:w:g:d:h:l:f:a:p",
                        long_options, &option_index);
       switch (c) {
          case 'p':
          strcpy(progName, argv[0]); 
          Usage(progName);
          exit (1);

          case 'n':
          north = atof(optarg);
          break;

          case 's':
          south = atof(optarg);
          break;

          case 'e':
          east = atof(optarg);
          break;

          case 'w':
          west = atof(optarg);
          break;

          case 'h':
          highClip = atof(optarg);
          min = highClip;
          break;

          case 'l':
          lowClip = atof(optarg);
          max = lowClip;
          break;

          case 'g':
          strcpy(gridFile, optarg); 
          break;

          case 'd':
          strcpy(dataFile, optarg); 
          break;

          case 'f':
          strcpy(fort63Mode, optarg); 
          break;

          case 'a':
          scale = atof(optarg);
          break;
       }

    }

    lineCtr = buildCliplist(&clipList, gridFile, fort63Mode, north, south, east, west);
#if defined DEBUG
    printf("clipList of 100 is %d\n", clipList[100]);
#endif
    /* Now that we have built the clip list, the algorithm proceeds
       as follows: We go line by line through the fort.63 file. We have
       to discard the three header lines and the one line delimiting each
       time step. If the file is a "full" file, then there is one line for
       every node.  If the file is compact then the third field in the third
       line tells us how many data points are in the timestep. Also for a 
       compact file, the third field of the delimiting line tells us how
       many data points are in the next timestep.For each data point, there
       is a node number and value.  If the node number is set to true in the
       cliplist and the value is between the high and low clip point, we 
       compare to the existing min and max. We handle NetCDF files in the
       same basic way, we just don't have the issues with the headers.
     */

     if ((!strcasecmp(fort63Mode, "compact")) ||
         (!strcasecmp(fort63Mode, "full"))) {
        DATA_FILE = fopen(dataFile, "r");
        if (DATA_FILE == NULL) {
           printf("Can't open file %s\n", dataFile);
           exit (1);
        }
      
        // Skip the first line
        fgets(line, MAX_LENGTH, DATA_FILE);

        fscanf(DATA_FILE, "%d %*d %*f %*d %*d %*s %*d \n", &nSteps);

        // Get the number of elements depending on the fort 63 mode.
        if (!strcasecmp(fort63Mode, "compact")) {
           // A compact file: the third line has 4 fields.
           fscanf(DATA_FILE, "%f %d %d %f\n", 
              &timeFloat, &timeInt, &nDataThisRecord, &noData);
        } else {
           // A full file: the third line has 2 fields.
           fscanf(DATA_FILE, "%f %d\n", &timeFloat, &timeInt);
           nDataThisRecord = lineCtr;
           noData = -9999.0; // unused in both cases.
        }
    } else if (!strcasecmp(fort63Mode, "netCDFScoop")) {
#ifdef USE_NETCDF
       nDataThisRecord = lineCtr;
       timeFloat = 0;
       timeInt = 0;
       // The file is an adcirc netCDF file following the "SCOOP" format.
       error = nc_open(dataFile, NC_NOCLOBBER, &ncid);
       if (error != NC_NOERR) {
          printf("could not open %s\n", dataFile);
          exit (1);
       }

       // The number of time steps in the file in in the time dimension
       error = nc_inq_dimid(ncid, "time", &dimid);
       if (error != NC_NOERR) {
          printf("could not retrieve dimension for time\n");
          exit (1);
       }
       error = nc_inq_dimlen(ncid, dimid, (size_t *)&nSteps);
       if (error != NC_NOERR) {
          printf("could not retrieve length for time\n");
          exit (1);
       }
       // The actual data is in a var called zeta
       error = nc_inq_varid(ncid, "zeta", &zetaid);
       if (error != NC_NOERR) {
          printf("could not retrieve id for zeta\n");
          exit (1);
       }

       // Read one record at a time.
       countDims[0] = 1;
       countDims[1] = 1;
#endif
    }
#if defined DEBUG
    printf("nSteps is %d\n", nSteps);
    printf("timeFloat is %f\n", timeFloat);
    printf("timeInt is %d\n", timeInt);
    printf("nDataThisRecord is %d\n", nDataThisRecord);
    printf("noData is %f\n", noData);
    printf("min %f max %f\n", min, max);
#endif
    for (i = 0; i < nSteps; i++) {
       for (j = 0; j < nDataThisRecord; j++) {
          if ((!strcasecmp(fort63Mode, "compact")) ||
            (!strcasecmp(fort63Mode, "full"))) {
               fscanf(DATA_FILE, "%d %f\n", &thisNode, &value);

               // IMPORTANT: the node numbers in the fort.63 file
               // start with 1.  The clip list is 0 based c array
               // so we have to decrement thisNode.
               thisNode --;
          } else if (!strcasecmp(fort63Mode, "netCDFScoop")) {
#ifdef USE_NETCDF
             startDims[0] = i;
             startDims[1] = j;
             thisNode = j;
             error = 
                nc_get_vara_float(ncid, zetaid, startDims, countDims, &value);
             if (error != NC_NOERR) {
                printf("could not retrieve zeta: error %d\n", error);
                exit (1);
             }
#if defined EXTREME_DEBUG
             if (i == 0) {
                printf("%f\n", value);
             }
#endif
#endif
          }
          if (clipList[thisNode]) {
             // This node is in the bounding box.
             if ((value < highClip) && (value > max)) {
                max = value;
             }
             if ((value > lowClip) && (value < min)) {
                min = value;
             }
          }
       } // end of loop for this timestep.
#if defined DEBUG
       printf("end of step %d\n", i);
       printf("min %f max %f\n", min, max);
#endif
       // process the delimiting line.
       if (!strcasecmp(fort63Mode, "compact")) {
          // A compact file: the delimiting line has 4 fields and
          // we need to reset nDataThisRecord.
          fscanf(DATA_FILE, "%f %d %d %f\n", 
                 &timeFloat, &timeInt, &nDataThisRecord, &noData);
        } else if (!strcasecmp(fort63Mode, "full")){
          // A full file: the third line has 2 fields, but we don't need to
          // reset anything because, by definition all timesteps are the same.
          fgets(line, MAX_LENGTH, DATA_FILE);
       }
    }
    
    // multiply by the scale factor.
    min *=scale;
    max *=scale;
    printf("%f %f\n", min, max);
    exit (0);
}
/*****************************************************************************
RENCI Open Source Software License
The University of North Carolina at Chapel Hill

The University of North Carolina at Chapel Hill (the "Licensor") through 
its Renaissance Computing Institute (RENCI) is making an original work of 
authorship (the "Software") available through RENCI upon the terms set 
forth in this Open Source Software License (this "License").  This License 
applies to any Software that has placed the following notice immediately 
following the copyright notice for the Software:  Licensed under the RENCI 
Open Source Software License v. 1.0.

Licensor grants You, free of charge, a world-wide, royalty-free, 
non-exclusive, perpetual, sublicenseable license to do the following to 
deal in the Software without restriction, including without limitation the 
rights to use, copy, modify, merge, publish, distribute, sublicense, 
and/or sell copies of the Software, and to permit persons to whom the 
Software is furnished to do so, subject to the following conditions:

. Redistributions of source code must retain the above copyright notice, 
this list of conditions and the following disclaimers.

. Redistributions in binary form must reproduce the above copyright 
notice, this list of conditions and the following disclaimers in the 
documentation and/or other materials provided with the distribution.

. Neither You nor any sublicensor of the Software may use the names of 
Licensor (or any derivative thereof), of RENCI, or of contributors to the 
Software without explicit prior written permission.  Nothing in this 
License shall be deemed to grant any rights to trademarks, copyrights, 
patents, trade secrets or any other intellectual property of Licensor 
except as expressly stated herein.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
THE CONTIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR 
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, 
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR 
OTHER DEALINGS IN THE SOFTWARE.

You may use the Software in all ways not otherwise restricted or 
conditioned by this License or by law, and Licensor promises not to 
interfere with or be responsible for such uses by You.  This Software may 
be subject to U.S. law dealing with export controls.  If you are in the 
U.S., please do not mirror this Software unless you fully understand the 
U.S. export regulations.  Licensees in other countries may face similar 
restrictions.  In all cases, it is licensee's responsibility to comply 
with any export regulations applicable in licensee's jurisdiction.

****************************************************************************/
