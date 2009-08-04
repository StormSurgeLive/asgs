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
   This code requires the netcdf header file and library.  Assuming your
   netcdf install is in /opt/netcdf/3.6.2, you can use this command line
   for compilation:

    gcc -Wall -o BuildContourFile BuildContourFile.c -I/opt/netcdf/3.6.2/include -L/opt/netcdf/3.6.2/lib -lnetcdf

    You can compile the code to emit some debugging information like so:

    gcc -DDEBUG -o BuildContourFile BuildContourFile.c -I/opt/netcdf/3.6.2/include -L/opt/netcdf/3.6.2/lib -lnetcdf
    
*/


#include <stdio.h>
#include <string.h>
#include <getopt.h>
#include <stdlib.h>
#include <netcdf.h>

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
BuildContourFile  --dataFile=dataFile  --fort63Mode=mode \n\
         --outFile=outFile --timeStep=timeStep  [--scale=scale] \n\
\n\
BuildContourFile reads an ADCIRC fort.63 data set and writes a triplet of\n\
{lon, lat, zeta} for each node of the specified time step into the\n\
requested output file in a format suitable for use with the GMT pscontour\n\
command.  The optional scale parameter is used to scale the zeta values\n\
written into the file The default scale value is 1.\n\
\n\
The data set must be provided in netCDF format and include both the\n\
values at each point and the grid coordinates of each point.  The\n\
fort63mode parameter controls how the data is specified.  The following\n\
modes are currently supported:\n\
\n\
\tnetCDFScoop: the grid and value data are in a netCDF file that conforms \n\
\t\tto that used in the ADCIRC implementation of the SURA Coastal \n\
\t\tOcean Observing and Prediction (SCOOP) Program.\n\
\n\
The arguments are:\n\
\n\
\t--help: Print this message.\n\
\t--dataFile: The file containing the lat,lon and zeta values for which\n\
\t\tto produce GMT format contour file.\n\
\t--fort63Mode: The mode for the input data, as discussed above.\n\
\t--scale: A scale factor (default 1.0) applied to the min and max before\n\
\t--outFile: The name of the contour file.\n\
\t--timeStep: The time step in the file (starting with 1) for which to\n\
\t\tproduce the contour file.";
   printf ("%s\n", message);
}

void writeIndexFile (int ncid, char *mode, char *indexFileName) {
    FILE  *OUT_FILE;
    int    error;
    int    neleId;
    int    eleId;
    int    nNele;
    // Could read this from the file. but most triangles have 3 sides ...
    int    nFace = 3; 
    int    thisFace[nFace];
    int    i;
    long   startDims[2];
    long   countDims[2];

    /* Open the output file */
    OUT_FILE = fopen(indexFileName, "w");
    if (OUT_FILE == NULL) {
       printf("Can't open file %s\n", indexFileName);
       exit (1); 
    }

    if (!strcasecmp(mode, "netCDFScoop")) {
       // The number of time steps in the file in in the time dimension
       error = nc_inq_dimid(ncid, "nele", &neleId);
       if (error != NC_NOERR) {
          printf("could not retrieve dimension for nele\n");
          exit (1);
       }
       error = nc_inq_dimlen(ncid, neleId, (size_t *)&nNele);
       if (error != NC_NOERR) {
          printf("could not retrieve length for nele\n");
          exit (1);
       }

       // get the id for the var ele.
       error = nc_inq_varid(ncid, "ele", &eleId);
       if (error != NC_NOERR) {
          printf("could not retrieve id for ele\n");
          exit (1);
       }

       startDims[0] = 0;
       startDims[1] = 0;
       countDims[0] = 1;
       countDims[1] = nFace;

       for (i = 0; i < nNele; i++) {
          startDims[0] = i;
          error = nc_get_vara_int(ncid, eleId, startDims, countDims, thisFace);
          if (error != NC_NOERR) {
             printf("could not retrieve ele %d\n", i);
             exit (1);
          }
          // Values are offset by 1...
          thisFace[0] --;
          thisFace[1] --;
          thisFace[2] --;
          fprintf(OUT_FILE, "%d %d %d\n", thisFace[0], thisFace[1], thisFace[2]);
       }
    }
    fclose(OUT_FILE);
}

int main (int argc, char *argv[]) {

    char   dataFile[MAX_LENGTH];
    char   outFile[MAX_LENGTH];
    char   fort63Mode[MAX_LENGTH];
    char   progName[MAX_LENGTH];
    FILE  *OUT_FILE;
    int    c = 0;
    int    option_index = 0;
    int    i;
    int    ncid;
    int    dimid;
    int    zetaid;
    int    error;
    long   startDims[2];
    long   countDims[2];
    int    timeStep;
    float  zeta;
    double thisLat;
    double thisLon;
    int    nData;
    long   start;
    int    nSteps;
    int    latid;
    int    lonid;
    long   count = 1;
    float  scale = 1.0;

    static struct option long_options[] =
    {
       {"dataFile", required_argument, 0, 'd'},
       {"fort63Mode", required_argument, 0, 'f'},
       {"outFile", required_argument, 0, 'o'},
       {"scale", required_argument, 0, 's'},
       {"help", no_argument, 0, 'p'},
       {"timeStep", required_argument, 0, 't'}
    };

    while (c != -1) {
       c = getopt_long (argc, argv, "d:f:o:s:t:",
                        long_options, &option_index);
       switch (c) {
          case 'p':
          strcpy(progName, argv[0]);
          Usage(progName);
          exit (1);

          case 'd':
          strcpy(dataFile, optarg); 
          break;

          case 'f':
          strcpy(fort63Mode, optarg); 
          break;

          case 'o':
          strcpy(outFile, optarg); 
          break;

          case 's':
          scale = atof(optarg);
          break;

          case 't':
          timeStep = atoi(optarg);
          break;
       }
    }
    /* 
     This code reads adcirc data and writes a triplet of {lon, lat, zeta}
     for each node in the data.  The code writes one timestep for every
     invocation.   

     At the moment, this code only handles netCdf file with the grid included.
     That may have to change. In addition, we only currently handle the
     SCOOP format files.  That will definitely change.
     */

    /* Open the output file */
    OUT_FILE = fopen(outFile, "w");
    if (OUT_FILE == NULL) {
       printf("Can't open file %s\n", outFile);
       exit (1); 
    }

    /* Here's the setup phase, where we open the netCDF file, retrieve
       the needed ids and values etc... */
    if (!strcasecmp(fort63Mode, "netCDFScoop")) {
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

       if (timeStep > nSteps) {
          printf("Requested timeStep (%d) > last time step (%d)\n", 
                 timeStep, nSteps);
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

       // The number of points in the grid is in a dimension called "node"
       error = nc_inq_dimid(ncid, "node", &dimid);
       if (error != NC_NOERR) {
          printf("could not retrieve dimension for node\n");
          exit (1);
       }

       error = nc_inq_dimlen(ncid, dimid, (size_t *)&nData);
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
    }
    startDims[0] = timeStep -1;
    for (i = 0; i < nData; i++) {
       if (!strcasecmp(fort63Mode, "netCDFScoop")) {
          // Get the ith zeta in this time step.
          startDims[1] = i;
          error = nc_get_vara_float(ncid, zetaid, startDims, countDims, &zeta);
          if (error != NC_NOERR) {
             printf("could not retrieve zeta: error %d\n", error);
             exit (1);
          }
  
          // multiply by the scale
          zeta *= scale;
          // get the lat and long values.
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
       }
       fprintf(OUT_FILE, "%lf %lf\t%f\n", thisLon, thisLat, zeta);
    } // end of loop for this timestep.
    fclose(OUT_FILE);
    writeIndexFile (ncid, fort63Mode, strcat(outFile, ".tri"));
    exit(0);
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
