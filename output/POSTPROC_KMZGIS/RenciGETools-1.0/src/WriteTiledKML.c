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
    gcc -Wall -o WriteTiledKML WriteTiledKML.c 
*/

#include <stdio.h>
#include <string.h>
#include <getopt.h>
#include <stdlib.h>
#include "WriteTiledKML.h"

int main (int argc, char *argv[]) {

    char   prefix[MAX_LENGTH];
    int    level;
    int    maxLevel;
    int    x;
    int    y;
    char   minPix[MAX_LENGTH];
    char   maxPix[MAX_LENGTH];
    float  north;
    float  south;
    float  east;
    float  west;

    FILE *fid;
    char  stringLevel[MAX_LENGTH];
    char  stringNextLevel[MAX_LENGTH];
    char  fileName[MAX_LENGTH];
    char  textLabel[MAX_LENGTH];
    char  graphicsName[MAX_LENGTH];
    char  colorbarName[MAX_LENGTH];
    char  logoName[MAX_LENGTH];
    char  date[MAX_LENGTH]; // The starting date
    char  dateEnd[MAX_LENGTH];
    long  count;
    float DeltaLongitude;
    float DeltaLatitude;
    int   xBase;
    int   yBase;
    int   nextLevel;
    int   tile;
    TILE  Tiles[4];
    int   c = 0;
    int   option_index = 0;

    static struct option long_options[] =
    {
       {"prefix", required_argument, 0, 'p'},
       {"level", required_argument, 0, 'l'},
       {"maxLevel", required_argument, 0, 'm'},
       {"tileX", required_argument, 0, 'x'},
       {"tileY", required_argument, 0, 't'},
       {"minPix", required_argument, 0, 'a'},
       {"maxPix", required_argument, 0, 'b'},
       {"north", required_argument, 0, 'n'},
       {"south", required_argument, 0, 's'},
       {"east", required_argument, 0, 'e'},
       {"west", required_argument, 0, 'w'},
       {"colorbar", required_argument, 0, 'c'},
       {"date", required_argument, 0, 'd'},
       {"logo", required_argument, 0, 'i'},
       {"endDate", required_argument, 0, 'f'}
    };

    while (c != -1) {
       c = getopt_long (argc, argv, "p:l:m:x:t:a:b:n:s:e:w:c:d:i:",
                        long_options, &option_index);
       switch (c) {
          case 'p':
          strcpy(prefix, optarg); 
          break;

          case 'l':
          level = strtol(optarg, (char ** )NULL, 10);
          break;

          case 'm':
          maxLevel = strtol(optarg, (char ** )NULL, 10);
          break;

          case 'x':
          x = strtol(optarg, (char ** )NULL, 10);
          break;

          case 't':
          y = strtol(optarg, (char ** )NULL, 10);
          break;

          case 'a':
          strcpy(minPix, optarg); 
          break;

          case 'b':
          strcpy(maxPix, optarg); 
          break;

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

          case 'c':
          strcpy(colorbarName, optarg); 
          break;

          case 'd':
          strcpy(date, optarg); 
          break;

          case 'i':
          strcpy(logoName, optarg); 
          break;

          case 'f':
          strcpy(dateEnd, optarg); 
          break;
       }

    }

    sprintf(stringLevel, "%d.%d.%d", level,x,y);
    sprintf(fileName, "%s.%s.kml", prefix, stringLevel);
    fid = fopen(fileName, "w");
    count = fprintf(fid,"%s\n","<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
    count = fprintf(fid,"%s\n","<!-- KML Region Template  -->");
    count = fprintf(fid,"%s\n","<!-- The first region is the current view. The four");
    count = fprintf(fid,"%s\n","     subsequent regions are the 4 sub regions. -->");
    count = fprintf(fid,"%s\n","<kml xmlns=\"http://earth.google.com/kml/2.1\">");
    count = fprintf(fid,"%s\n","<Document>");
    if (strcmp(date, "-1")) {
       /* Date of -1 means not to create the time span */
       count = fprintf(fid,"%s\n","<TimeSpan>");
       count = fprintf(fid,"\t%s%s%s\n","<begin>", date, "</begin>");
       count = fprintf(fid,"\t%s%s%s\n","<end>", dateEnd, "</end>");
       count = fprintf(fid,"%s\n","</TimeSpan>");
    } 
    count = fprintf(fid,"%s%s%s\n","<name>", prefix, "</name>");
    count = fprintf(fid,"%s\n","<ScreenOverlay>");
    count = fprintf(fid,"\t%s%s%s\n","<name>", "Colormap", "</name>");
    /* This sets up the colormap on the right of the screen, halfway up */
    count = fprintf(fid,"\t%s\n","<overlayXY x=\"1\" y=\"0.5\" xunits=\"fraction\" yunits=\"fraction\"/>");
    count = fprintf(fid,"\t%s\n","<screenXY x=\"1\" y=\"0.5\" xunits=\"fraction\" yunits=\"fraction\"/>");
    fprintf(fid,"\t%s\n","<Icon>");
    fprintf(fid,"\t\t%s%s%s\n","<href>", colorbarName, "</href>");
    fprintf(fid,"\t%s\n","</Icon>");
    count = fprintf(fid,"%s\n","</ScreenOverlay>");

    // Now put in the renci logo
    count = fprintf(fid,"%s\n","<ScreenOverlay>");
    count = fprintf(fid,"\t%s%s%s\n","<name>", "Renci", "</name>");
    /* Bottom left of screen */
    count = fprintf(fid,"\t%s\n","<overlayXY x=\"0\" y=\"0\" xunits=\"fraction\" yunits=\"fraction\"/>");
    count = fprintf(fid,"\t%s\n","<screenXY x=\".025\" y=\".02\" xunits=\"fraction\" yunits=\"fraction\"/>");
    fprintf(fid,"\t%s\n","<Icon>");
    fprintf(fid,"\t\t%s%s%s\n","<href>", logoName, "</href>");
    fprintf(fid,"\t%s\n","</Icon>");
    count = fprintf(fid,"%s\n","</ScreenOverlay>");
    count = fprintf(fid,"%s\n","<Region>");
    count = fprintf(fid,"\t%s\n","<Lod>");
    count = fprintf(fid,"\t\t%s%s%s\n","<minLodPixels>", minPix, "</minLodPixels>");
    if (level < maxLevel) {
        count = fprintf(fid,"\t\t%s%s%s\n","<maxLodPixels>", maxPix, "</maxLodPixels>");
    } else {
        count = fprintf(fid,"\t\t%s-1%s\n","<maxLodPixels>","</maxLodPixels>");
    }
    count = fprintf(fid,"\t%s\n","</Lod>");
    count = fprintf(fid,"\t%s\n","<LatLonAltBox>");
    count = fprintf(fid,"\t\t%s%f%s\n","<north>", north, "</north>");
    count = fprintf(fid,"\t\t%s%f%s\n","<south>", south, "</south>");
    count = fprintf(fid,"\t\t%s%f%s\n","<east>", east, "</east>");
    count = fprintf(fid,"\t\t%s%f%s\n","<west>", west, "</west>");
    count = fprintf(fid,"\t%s\n","</LatLonAltBox>");
    count = fprintf(fid,"%s\n","</Region>");
    
    if (level < maxLevel) {
        // Do the network links if needed.
        // Make an array with the dimensions of the 4 sub-tiles.
        // Tile 0,0 is lower left.
        xBase = ((x - 1) * 2) + 1;
        yBase = ((y - 1) * 2) + 1;
        DeltaLongitude=-((west - east)/2.0);
        DeltaLatitude=(north - south)/2.0;
        Tiles[0].west = west;
        Tiles[0].north = south  + DeltaLatitude;
        Tiles[0].south = south;
        Tiles[0].east = west + DeltaLongitude;
        sprintf(textLabel, "%d.%d", xBase, yBase);
        strcpy(Tiles[0].lable,textLabel);
        
        Tiles[1].west = west + DeltaLongitude;
        Tiles[1].north = south + DeltaLatitude;
        Tiles[1].south = south;
        Tiles[1].east = east;
        sprintf(textLabel, "%d.%d", xBase, yBase + 1);
        strcpy(Tiles[1].lable,textLabel);
        
        Tiles[2].west = west;
        Tiles[2].north = north;
        Tiles[2].south = south + DeltaLatitude;
        Tiles[2].east = west + DeltaLongitude;
        sprintf(textLabel, "%d.%d", xBase + 1, yBase);
        strcpy(Tiles[2].lable,textLabel);
        
        Tiles[3].west = west + DeltaLongitude;
        Tiles[3].north = north;
        Tiles[3].south = south + DeltaLatitude;
        Tiles[3].east = east;
        sprintf(textLabel, "%d.%d", xBase + 1, yBase + 1);
        strcpy(Tiles[3].lable,textLabel);
        
        nextLevel = level + 1;
        for (tile=0; tile < 4; tile ++) {
            fprintf(fid,"%s\n","<NetworkLink>");
            sprintf(stringNextLevel,"%s.%d.%s", prefix, nextLevel,Tiles[tile].lable);
            fprintf(fid,"\t%s%s%s\n","<name>", stringNextLevel, "</name>");
            fprintf(fid,"\t%s\n","<Region>");
            fprintf(fid,"\t\t%s\n","<Lod>");
            fprintf(fid,"\t\t\t%s%s%s\n", "<minLodPixels>", minPix, "</minLodPixels>");
            fprintf(fid,"\t\t\t%s%s%s\n", "<maxLodPixels>", maxPix, "</maxLodPixels>");
            fprintf(fid,"\t\t%s\n","</Lod>");
            fprintf(fid,"\t\t%s\n","<LatLonAltBox>");
            fprintf(fid,"\t\t\t%s%f%s\n", "<north>", Tiles[tile].north, "</north>");
            fprintf(fid,"\t\t\t%s%f%s\n", "<south>", Tiles[tile].south, "</south>");
            fprintf(fid,"\t\t\t%s%f%s\n", "<east>", Tiles[tile].east, "</east>");
            fprintf(fid,"\t\t\t%s%f%s\n", "<west>", Tiles[tile].west, "</west>");
            fprintf(fid,"\t\t%s\n","</LatLonAltBox>");
            fprintf(fid,"\t%s\n","</Region>");
            fprintf(fid,"\t%s\n","<Link>");
            fprintf(fid,"\t\t%s%s.kml%s\n", "<href>", stringNextLevel, "</href>");
            fprintf(fid,"\t\t%sonregion%s\n", "<viewRefreshMode>", "</viewRefreshMode>");
            fprintf(fid,"\t\t%s\n","<viewFormat/>");
            fprintf(fid,"\t%s\n","</Link>");
            fprintf(fid,"%s\n","</NetworkLink>");
        }
    }
    
    fprintf(fid,"%s\n","<GroundOverlay>");
    fprintf(fid,"\t%s%d%s\n","<drawOrder>", level +1,"</drawOrder>");
    fprintf(fid,"\t%s\n","<Icon>");
    sprintf(graphicsName, "%s.%s.png", prefix, stringLevel);
    fprintf(fid,"\t\t%s%s%s\n","<href>", graphicsName, "</href>");
    fprintf(fid,"\t%s\n","</Icon>");
    fprintf(fid,"\t%s\n","<LatLonBox>");
    fprintf(fid,"\t\t%s%f%s\n","<north>", north, "</north>");
    fprintf(fid,"\t\t%s%f%s\n","<south>", south, "</south>");
    fprintf(fid,"\t\t%s%f%s\n","<east>", east, "</east>");
    fprintf(fid,"\t\t%s%f%s\n","<west>", west, "</west>");
    fprintf(fid,"\t%s\n","</LatLonBox>");
    fprintf(fid,"%s\n","</GroundOverlay>");
    fprintf(fid,"%s\n","</Document>");
    fprintf(fid,"%s\n","</kml>");
    fclose(fid);
    exit (0);
}
/****************************************************************************

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

