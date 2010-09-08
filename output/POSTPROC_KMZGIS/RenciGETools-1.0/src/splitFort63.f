c 
c Copyright (c) 2008  Renaissance Computing Institute. All rights reserved.
c 
c This software is open source. See the bottom of this file for the license.
c 
c Renaissance Computing Institute, 
c (A Joint Institute between the University of North Carolina at Chapel Hill,
c North Carolina State University, and Duke University)
c http://www.renci.org
c 
c For questions, comments please contact software@renci.org
c 
c 
      program splitFort63
      implicit none
      
      interface
      integer function strlen(st)
      implicit none
      character    st*(*)
      end function
      end interface

c this reads the fort.63 file output from adcirc and spits it up into
c individual data files, one for each model output (typically hourly)

      character*256 inpfile, outfile
      character*80 firstLine, dummyStr, prefix, mode
      character*4 timeStamp
      integer numTimesteps, numNodes, numFilled, i, idx, it, fileFmtVers
      integer dummy, itimestep
      real noData, timestep
      real elev(500000), elevPt
      integer stringLen
      integer iargc

!     First arg if any is the name of the input.63
      if (iargc() .gt. 0)then
         call getarg(1,inpfile)
      else
         inpfile = 'fort.63'
      endif

      if (iargc() .gt. 1)then
         call getarg(2,prefix)
      else
         prefix = inpfile
      endif

      if (iargc() .gt. 2)then
         call getarg(3,mode)
      else
         mode='compact'
      endif

      print *, "Splitting file ", inpfile
      open (1, file=inpfile, status='old')


      read (1,1000) firstLine
1000  format (a80)
      print *, firstLine
      read (1,*) numTimesteps, numNodes, timestep, itimestep, dummy,
     $     dummyStr, fileFmtVers
      print *, "Grid size is ",numNodes," with ",numTimesteps,
     $     " time steps"




      do it = 1, numTimesteps
c     define the output file name
       write(timeStamp, fmt='(i4.4)') it
       stringLen = strlen(prefix)
       outfile = prefix
       stringLen = stringLen + 1
       outfile(stringLen:stringLen) = '.'
       stringLen = stringLen + 1
       outfile(stringLen:stringLen + 3) = timeStamp
       print *, "outfile=",outfile

         if (numTimesteps.gt.1) then
             if (mode.eq.'compact') then
                print *, "mode 1=",mode
                read (1,*) timestep, dummy, numFilled, noData
             else
c Not the compact format.  Each node will have data, and it
c doesn't matter what we set "nodata" to because it won't be used
                print *, "mode 2=",mode
                read (1,*) timestep, dummy
                numFilled = numNodes
                noData = 9999 
             endif
         else
             call system("cp "//trim(inpfile)//" "//trim(outfile))
             call exit (0)
         endif
      print *, "timestep ",timestep," dummy ",dummy
      print *, "numFilled ",numFilled," noData ",noData
         
         
c     initialize elevation array to no data s.t. after the
c     points for which there is data are filled in, the remaining
c     will have the noData value
c
c     work around missing f90 compiler
c         elev = noData
         do i=1,500000
            elev(i) = noData
         enddo


c     read a time step and write the value into the elevation array
         do i=1,numFilled
            read (1,*) idx, elevPt
            elev(idx) = elevPt
         enddo



c     open the output file
       open (2, file=outfile, status='unknown')


c     write out the elev for this time step for all nodes
c     
c     first write the header lines         
         write (2,*) firstLine
         write (2,*) " 1 ", numNodes,
     $        " 0.1000000E+01     1     1 FileFmtVersion: ", fileFmtVers
         write (2,*) timestep, itimestep
         
c     now write out all the data points
         do i=1,numNodes
            write (2,*) i, elev(i)
         enddo

c close the file
         close (2)
         
      enddo     ! end of do it

      end

      integer function strlen(st)
      implicit none
      character    st*(*)
      integer       i
      i = len(st)
      do while (st(i:i) .eq. ' ')
        i = i - 1
      enddo
      strlen = i
      return
      end function

c ***************************************************************************
c 
c RENCI Open Source Software License
c The University of North Carolina at Chapel Hill
c 
c The University of North Carolina at Chapel Hill (the "Licensor") through 
c its Renaissance Computing Institute (RENCI) is making an original work of 
c authorship (the "Software") available through RENCI upon the terms set 
c forth in this Open Source Software License (this "License").  This License 
c applies to any Software that has placed the following notice immediately 
c following the copyright notice for the Software:  Licensed under the RENCI 
c Open Source Software License v. 1.0.
c 
c Licensor grants You, free of charge, a world-wide, royalty-free, 
c non-exclusive, perpetual, sublicenseable license to do the following to 
c deal in the Software without restriction, including without limitation the 
c rights to use, copy, modify, merge, publish, distribute, sublicense, 
c and/or sell copies of the Software, and to permit persons to whom the 
c Software is furnished to do so, subject to the following conditions:
c 
c . Redistributions of source code must retain the above copyright notice, 
c this list of conditions and the following disclaimers.
c 
c . Redistributions in binary form must reproduce the above copyright 
c notice, this list of conditions and the following disclaimers in the 
c documentation and/or other materials provided with the distribution.
c 
c . Neither You nor any sublicensor of the Software may use the names of 
c Licensor (or any derivative thereof), of RENCI, or of contributors to the 
c Software without explicit prior written permission.  Nothing in this 
c License shall be deemed to grant any rights to trademarks, copyrights, 
c patents, trade secrets or any other intellectual property of Licensor 
c except as expressly stated herein.
c 
c THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
c IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
c FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
c THE CONTIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR 
c OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, 
c ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR 
c OTHER DEALINGS IN THE SOFTWARE.
c 
c You may use the Software in all ways not otherwise restricted or 
c conditioned by this License or by law, and Licensor promises not to 
c interfere with or be responsible for such uses by You.  This Software may 
c be subject to U.S. law dealing with export controls.  If you are in the 
c U.S., please do not mirror this Software unless you fully understand the 
c U.S. export regulations.  Licensees in other countries may face similar 
c restrictions.  In all cases, it is licensee's responsibility to comply 
c with any export regulations applicable in licensee's jurisdiction.
c 
c ***************************************************************************# 
