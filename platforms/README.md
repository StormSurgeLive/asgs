# Adding HPC Platforms

## Introduction

The `./platforms` directory is a modular way to add support for
a new HPC machine without modifying any files; but by only adding a directory
in `./platforms` and supporting files. These files can be committed
back to the ASGS repository and shared with others that would also
like to use the ASGS on the same platform.

There is also a mechanism to add new HPC platforms in a way that
keeps the information private, so that it can be shared within
your organization or group, but not externally.

Our the goal is to use the ASGS GitHub repository to maximize
efficiency and prevent duplication of effort when commonly shared
HPC platforms are occasionally added. If support for your
platform is to be kept private, please
read the section below on maintaining "site specific" platform support
outside of the main upstream ASGS git repository.

## Adding Common Platforms

We have set up a system for adding new platforms with all the settings in one place. Start by having a look at the subdirectories under `platforms`; this will give you an idea of the type of information that you'll need to gather to specify the characteristics of the new HPC. There are directories for HPC systems including lonestar6, hatteras, mike, and penguin there. We will also use the method described in this README file to extend support for any/all new HPCs in the future.

### Extend

Steps:

1. Add an issue in the ASGS issue tracker with a title like "Add support for NewHPC at ThisOrg".
2. Create a branch of the ASGS repo with the name issue-xxxx where xxxx is the number GitHub assigned to your new issue from step 1. Making your changes on this branch will help you
   1. keep track of them;
   2. allow you to commit them back to GitHub on their own branch so that we can see them and provide suggestions; and
   3. facilitate a pull request for these changes back to the upstream ASGS repository in the future.
3. Check out your issue branch on the new HPC you'd like to support
4. cp -r `./platforms/penguin` `./platforms/ournewplatform`
5. edit `./platforms/ournewplatform/init.sh` as needed
6. edit `./about.txt` as needed
7. test
8. commit/push your changes on this branch back to the upstream repository; this will help with visibility to the ASGS community in case you run into issues

### Test

Testing a new platform is done by exercising all the normal aspects
from building it to starting and working within the ASGS Shell
Environment.

These testing steps are:
1. build ASGS
2. launch the ASGS Shell Environment, which should detect the platform as expected
3. ensure the command, "guess platform" in the shell reports the current platform as expected
4. build ADCIRC via the "build adcirc" command
5. initiate an ASGS run using a known working configuration

Assuming you're on the platform you're adding (recommended), then it
will be presented as the default option. Platforms added this way
should do what is expected when running the ASGS Shell Environment.

## Adding Private Platforms

Some sites that use ASGS operate HPC platforms that can't or shouldn't be
distributed as an commonly supported platform via ASGS' platform
scheme described above.

Integrating a private platform is straightforward
and follows the same scheme described above. The only difference
is that the directory containing the site's `./platforms` directory
must be specified when `./init-asgs.sh` is run using the `-p` option.

Typical steps look like the following:

1. when inside of the `./asgs` directory, copy the site specific
   assets directory (more on the structure of this below)

2. run `./init-asgs.sh` with the `-p` option,
```
   ./init-asgs.sh -p ./private-assets-dir # YOU provide the dir
```
3. ensure that the private platform is listed as an option of
   platforms presented by `./init-asgs.sh` and that it is the default
   selection

### Private Assets Directory

This directory is maintained privately by a site, and can be done
so in a variety of ways. ASGS only cares that it exists and that
you let `./init-asgs.sh` know about it. It also should move or "go
away" at any point during use.

The structure of the private assets directory reflects that
of the highest level `./asgs` (often called the "script directory"
and tracked using the `SCRIPTDIR` environmental variable inside of
asgsh.

An illustration of this directory structure relative to ./asgs is:
```
./asgs/                  #|
  ./platforms/           #|
  ./meshes/              #| <--<< existing, from the upstream repository
  ./config/              #|
  ...                    #|
  ./private-assets-dir/  #  <--<< this dir can be any name
    ./asgs-configs/
      ./asgs-config-1.sh
      ./asgs-config-2.sh
      ...
    ./meshes/
    ./platforms/
      ./privateplatform1/
        ./init.sh
        ./about.txt
      ./privateplatform2/
        ./init.sh
        ./about.txt
      ...
```

## Conclusion

This README is a living document,
so when in doubt please speak up and ask questions are our various
collaboration spaces.
