Jason,

We are close to running the following automagically, as an epilogue to every advisory on any mesh:
run_adv.sh Currently set to run for everything within an instance's current advisory, as in instance/advisory/ensemble_member, runs a wrapper script to execute FigureGen (v41, with state boundaries) to plot maxele as a sanity check. Then executes a NOS retrieve and compare plot script creating hydrographs.

    Calls:

    fgwrapper.sh - sed replaces from a template to provided options, executes FigureGen

    nos_get.sh - manages the following

        nos-create.pl -or- nos-predictions.pl - Obtains NOS data via API for stations within range of central lat/lon, either preliminary observations or tidal predictions, depending on input provided
        station_transpose.pl - Well, you know...
        gnuplot - gnuplot
        convert (ImageMagick)


I have attached them all (except system installed executables), but must admit that they really are still in a developmental state as far as comments, etc. go.

Hopefully, some of this will help you with getting the NOS validation you were hoping for.
Please, let me know if you have any questions.

Cheers, not jeers,
-Ben
