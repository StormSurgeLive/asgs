# Patch Management

## Overview

Patch sets are broken down by:

- program (e.g., ImageMagick-7, ADCIRC)
- if necessary, there should one additional directory hierarchy that
  is a combined "patchset", i.e., all patches that can be applied and have
  been grouped so there are no conflicts
- "patchset" directories may very well contain an intersection of patches,
  but should not be totally identical; this means that some patches will
  be duplicated in different patchsets

## Adding ADCIRC Patch Sets

ADCIRC patch sets are applied dynamically based on the contents
of the patches/ADCIRC directory. To add an officially supported
patch set, do the following:

1. create a directory in `patches/ADCIRC` that reflects the name
you wish to preset in the "build adcirc" menu,
```
   mkdir patches/ADCIRC/v53release-my-precious
```
2. copy 2 files from any existing supported version in patches/ADCIRC:
```
   cp patches/ADCIRC/v53release/info.sh patches/ADCIRC/v53release-my-precious/info.sh
   cp patches/ADCIRC/v53release/about.txt patches/ADCIRC/v53release-my-precious/about.txt
```
3. edit these 2 new files:

   1. `patches/ADCIRC/v53release-my-precious/about.txt` <- contains a short description text for the menu

   2. `patches/ADCIRC/v53release-my-precious/info.sh`   <- contains 2 key variables that need to be defined
```
      # example info.sh
      export ADCIRC_GIT_BRANCH=v53release # this is the base git branch, tag or SHA
      export SWANDIR=swan                 # defines where SWAN is located (changed in v55+)
```
4. adding patches

Doing this will be the subject of another future document, but in the example
above, it is sufficient to copy all of the `*.patch` files from `patches/ADCIRC/v53release`:
```
   cp patches/ADCIRC/v53release/*.patch patches/ADCIRC/v53release-my-precious/
```
Nothing else is required to support patches, they will be applied automatically
if they are in the directory and the `info.sh` is set up properly; since the `info.sh`
defines the starting point of the code onto which the patch files are applied.
And this is why defining `ADCIRC_GIT_BRANCH` is extremely important.

Please report issues or bugs to the ASGS Github bug tracker.
