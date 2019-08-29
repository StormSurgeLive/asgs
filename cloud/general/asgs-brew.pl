#!/usr/bin/perl

use strict;
use warnings;
use Getopt::Long qw(GetOptionsFromArray);
use Cwd qw(getcwd);

package local::asgs_brew;

use constant EXIT_SUCCESS => 0;

# copy existing environment
local %ENV = %ENV;

our $AFFECTED_ENV_VARS = {};    # list of envars that were updated across all steps
our $DEBUG_SKIP_LIST   = {};    # for debugging only, forces a step's "skip_if" to be true

exit __PACKAGE__->run( \@ARGV // [] ) if not caller;

# basic constructor, just for testing at this time
sub new {
    my $pkg  = shift;
    my $self = {};
    bless $self, $pkg;
    return $self;
}

sub run {
    my ( $self, $args_ref ) = @_;
    my $HOME     = ( getpwuid $> )[7];
    my $opts_ref = {
        compiler       => q{gfortran},
        'install-path' => qq{$HOME/opt},
        home           => $HOME,
        'make-jobs'    => 1,

    };
    my $ret = Getopt::Long::GetOptionsFromArray(
        $args_ref, $opts_ref,
        q{clean}, q{compiler=s}, q{debug-skip-steps=s}, q{dump-env}, q{force}, q{home}, q{install-path=s}, q{list-steps}, q{machinename=s}, q{make-jobs=i},
    );

    die $@ if not $ret;

    if ( not $opts_ref->{compiler} or not $opts_ref->{machinename} ) {
        print qq{
Usage:

    ./asgs-brew.pl --machinename <MachineName> --compiler <CompilerFamily> [--install-path some/path --home /path/other/than/user/\$HOME --force --clean --list-steps --debug-skip-steps]

Required Flags:

    --compiler, --machinename

Note:

    --install-path if not specified defaults to \$HOME/opt

More Help and Information:

    \$ perldoc path/to/asgs-brew.pl

};
        exit 255;    # die's exit code
    }

    $self->_process_opts($opts_ref);    # additional processing of options

    $self->_run_steps($opts_ref);

    $self->_run_finalize($opts_ref);

    return EXIT_SUCCESS;
}

sub _process_opts {
    my ( $self, $opts_ref ) = @_;

    # add to list of steps to skip - this is to assist in debugging only, not to affect the
    # flow of the building of ASGS
    if ( $opts_ref->{'debug-skip-steps'} ) {
        for my $step ( split ',', $opts_ref->{'debug-skip-steps'} ) {
            ++$DEBUG_SKIP_LIST->{$step};
        }
    }

    return;
}

sub _run_steps {
    my ( $self, $opts_ref ) = @_;

  LIST_STEPS:
    if ( $opts_ref->{'list-steps'} ) {
        foreach my $op ( @{ $self->get_steps($opts_ref) } ) {
            die q{Steps require a 'key' and a 'description' field for --list-steps to work properly} if not $op->{key} or not $op->{description};
            print sprintf( "% 20s - %s\n", $op->{key}, $op->{description} );
        }

        # proceeds no further if --list-steps is used
        return 0;
    }

    my $start_dir = Cwd::getcwd();
  RUN_STEPS:
    foreach my $op ( @{ $self->get_steps($opts_ref) } ) {
        print $op->{name} . qq{\n} unless $opts_ref->{'dump-env'};

        # start in known location (step pwd can be relative)
        chdir $start_dir;

        # move to specified directory
        chdir $op->{pwd};

        # augment ENV based on $op->{export_ENV}
        $self->_setup_ENV( $op, $opts_ref );
        next RUN_STEPS if $opts_ref->{'dump-env'};

        # check for skip condition for run step, unless --force is used
        # if op is contained in --debug-skip-steps list then the step is skipped unconditionally (--force is ignored)
        if ( defined $DEBUG_SKIP_LIST->{ $op->{key} } or ( ref $op->{skip_if} eq q{CODE} and $op->{skip_if}->( $op, $opts_ref ) and not $opts_ref->{force} ) ) {
            print qq{Skipping $op->{name} because 'skip_if' condition has been met.\n};
            next RUN_STEPS;
        }

        # precondition checking needs to be more robust and more clearly
        # defined (i.e., what to do on failure for subsequent runs
        # check is skipped if --clean is passed
        if ( not $self->_run_precondition_check ) {
            die qq{pre condition for "$op->{name}" FAILED, stopping. Please fix and rerun.\n};
        }

        # run command or clean_command (looks for --clean)
        $self->_run_command( $op, $opts_ref );

        # verify step completed successfully
        # check is skipped if --clean is passed
        if ( $self->_run_postcondition_check( $op, $opts_ref ) ) {
            print qq{"$op->{name}" was completed successfully\n};
        }
        else {
            die qq{post condition for "$op->{name}" FAILED, stopping. Please fix and rerun.\n};
        }
    }
    return 1;
}

sub _run_precondition_check {
    my ( $self, $op, $opts_ref ) = @_;

    # skips check if --clean or precondition check doesn't exist in step's definition as a CODE ref
    return 1
      if $opts_ref->{clean}
      or not ref $op->{precondition_check} eq q{CODE}
      or $op->{precondition_check}->( $op, $opts_ref );
    return undef;
}

sub _run_postcondition_check {
    my ( $self, $op, $opts_ref ) = @_;

    # skips check if --clean or postcondition check doesn't exist in step's definition as a CODE ref
    return 1
      if $opts_ref->{clean}
      or not ref $op->{postcondition_check} eq q{CODE}
      or $op->{postcondition_check}->( $op, $opts_ref );
    return undef;
}

sub _run_finalize {
    my ( $self, $opts_ref ) = @_;

    return 1 if $opts_ref->{'list-steps'} or $opts_ref->{'clean'};    # do not show

    $self->_print_summary($opts_ref);

    return 1;
}

sub _run_command {
    my ( $self, $op, $opts_ref ) = @_;
    my $compiler     = $opts_ref->{compiler};
    my $install_path = $opts_ref->{'install-path'};

    # choose command to run
    my $command = ( not $opts_ref->{clean} ) ? $op->{command} : $op->{clean_command};

    return 1 if not defined $command;

    local $| = 1;

    print qq{\n$command\n};
    system("$command 2>&1");

    return 1;
}

sub _print_summary {
    my ( $self, $opts_ref ) = @_;

    return 1 if $opts_ref->{clean};
    print q{-} x 45 . qq{\nSummary of updated environmental variables (these need to be added to ~/.bash_profile or similar):\n\n};
    foreach my $envar ( keys %$AFFECTED_ENV_VARS ) {
        printf( qq{export %s=%s\n}, $envar, $ENV{$envar} );
    }

    return 1;
}

sub _setup_ENV {
    my ( $self, $op, $opts_ref ) = @_;
    my $install_path = $opts_ref->{'install-path'};
  SETUP_ENV:
    foreach my $envar ( keys %{ $op->{export_ENV} } ) {
        ++$AFFECTED_ENV_VARS->{$envar};    # track all environmental variables that are touched

        # default "how" mode is to prepend if the envar is already defined
        if ( not defined $op->{export_ENV}->{$envar}->{how} or $op->{export_ENV}->{$envar}->{how} eq q{prepend} ) {
            $ENV{$envar} = sprintf( "%s%s", $op->{export_ENV}->{$envar}->{value}, ( $ENV{$envar} ) ? q{:} . $ENV{$envar} : q{} );
        }
        elsif ( $op->{export_ENV}->{$envar}->{how} eq q{append} ) {
            $ENV{$envar} = sprintf( "%s%s", ( $ENV{$envar} ) ? $ENV{$envar} . q{:} : q{}, $op->{export_ENV}->{$envar}->{value} );
        }
        elsif ( $op->{export_ENV}->{$envar}->{how} eq q{replace} ) {
            $ENV{$envar} = $op->{export_ENV}->{$envar}->{value};
        }

        #print qq{setting $envar=$ENV{$envar}\n};
    }
    return 1;
}

# Note, user's environment is available via %ENV
# TODO - add in preconditions, need per step validation of required options/params
sub get_steps {
    my ( $self, $opts_ref ) = @_;
    my $install_path = $opts_ref->{'install-path'};
    my $compiler     = $opts_ref->{compiler};
    my $home         = $opts_ref->{home};
    my $machinename  = $opts_ref->{machinename};
    my $makejobs     = $opts_ref->{'make-jobs'};

    my $steps = [
        {
            key           => q{openmpi},
            name          => q{Step for OpenMPI 1.8.1 for gfortran},
            description   => q{Downloads and builds OpenMPI on all platforms for ASGS. Note: gfortran is required, so any compiler option causes this step to be skipped.},
            pwd           => q{./cloud/general},
            command       => qq{bash init-openmpi.sh $install_path $compiler $makejobs},
            clean_command => qq{bash init-openmpi.sh $install_path clean},

            # augment existing %ENV (cumulative)
            export_ENV => {
                PATH => { value => qq{$install_path/$compiler/bin}, how => q{prepend} },
            },

            # skip this step if the compiler is not set to gfortran
            skip_if            => sub { return ( ( $compiler ne q{gfortran} ) ) ? 1 : 0 },
            precondition_check => sub { 1 },
            postcondition_check => sub {
                my ( $op, $opts_ref ) = @_;
                my $bin          = qq{$opts_ref->{'install-path'}/$compiler/bin};
                my $ok           = 1;
                my @mpi_binaries = (
                    qw/mpic++ mpic++-vt mpif77-vt mpirun ompi-top orted orte-top otfaux  otfmerge otfshrink vtcc vtfilter  vtrun mpicc mpicxx mpif90 ompi-clean opal_wrapper orte-info oshcc otfcompress otfmerge-mpi shmemcc vtCC vtfiltergen vtunify mpiCC mpicxx-vt mpif90-vt ompi_info opari  orte-ps oshfort otfconfig otfprint shmemfort vtcxx vtfiltergen-mpi vtunify-mpi mpicc-vt mpiexec mpifort ompi-ps ortecc orterun oshmem_info otfdecompress otfprofile shmemrun vtf77 vtfilter-mpi vtwrapper mpiCC-vt mpif77 mpifort-vt ompi-server orte-clean orte-server oshrun otfinfo otfprofile-mpi vtc++ vtf90 vtfort/
                );
                map { $ok = -e qq[$bin/$_] && $ok } @mpi_binaries;
                return $ok;
            },
        },
        {
            key           => q{hdf5-netcdf},
            name          => q{Step for NetCDF, HDF5 libraries and utilities},
            description   => q{Downloads and builds the versions of HDF5 and NetCDF that have been tested to work on all platforms for ASGS.},
            pwd           => q{./cloud/general},
            command       => qq{bash init-hdf5-netcdf4.sh $install_path $compiler $makejobs},
            clean_command => qq{bash init-hdf5-netcdf4.sh $install_path clean},

            # augment existing %ENV (cumulative)
            export_ENV => {
                LD_LIBRARY_PATH => { value => qq{$install_path/lib},       how => q{prepend} },
                LIBRARY_PATH    => { value => qq{$install_path/lib},       how => q{prepend} },
                LD_INCLUDE_PATH => { value => qq{$install_path/include},   how => q{prepend} },
                NETCDFHOME      => { value => qq{$install_path},           how => q{replace} },
                CPPFLAGS        => { value => qq{-I$install_path/include}, how => q{replace} },
                LDFLAGS         => { value => qq{-L$install_path/lib},     how => q{replace} },
            },
            skip_if            => sub { 0 },    # if true and --force is not used, unilaterally skips the run step
            precondition_check => sub { 1 },    # just a "1" indicates no checking is done
            postcondition_check => sub {
                my ( $op, $opts_ref ) = @_;
                my $bin = qq{$opts_ref->{'install-path'}/bin};
                my $ok  = 1;
                map { $ok = -e qq[$bin/$_] && $ok } (qw/gif2h5 h5cc h5debug h5dump h5import h5ls h5perf_serial h5repack h5stat nc-config ncdump ncgen3 h52gif h5copy h5diff h5fc h5jam h5mkgrp h5redeploy h5repart h5unjam nccopy ncgen nf-config/);
                return $ok;
            },
        },
        {
            key         => q{wgrib2},
            name        => q{Step for wgrib2},
            description => q{Downloads and builds wgrib2 on all platforms for ASGS. Note: gfortran is required, so any compiler option passed is overridden.},
            pwd         => q{./},

            # -j > 1 breaks this makefile
            command             => qq{make clean && make -j 1 NETCDFPATH=$install_path NETCDF=enable NETCDF4=enable NETCDF4_COMPRESSION=enable MACHINENAME=$machinename compiler=gfortran},
            clean_command       => q{make clean},
            skip_if             => sub { 0 },
            precondition_check  => sub { 1 },
            postcondition_check => sub { my ( $op, $opts_ref ) = @_; return -e qq{./wgrib2}; },
        },
        {
            key                 => q{cpra-postproc},
            name                => q{Step for in output/cpra_postproc},
            description         => q{Runs the makefile and builds associated utilities in the output/cpra_postproc directory},
            pwd                 => q{./output/cpra_postproc},
            command             => qq{make clean && make -j $makejobs NETCDF_CAN_DEFLATE=enable NETCDFPATH=$install_path NETCDF=enable NETCDF4=enable NETCDF4_COMPRESSION=enable MACHINENAME=$machinename compiler=$compiler},
            clean_command       => q{make clean},
            skip_if             => sub { 0 },
            precondition_check  => sub { 1 },
            postcondition_check => sub { my ( $op, $opts_ref ) = @_; return -e qq{./FigureGen}; },
        },
        {
            key         => q{output},
            name        => q{Step for in output/},
            description => q{Runs the makefile and builds associated utilities in the util/ directory.},
            pwd         => q{./output},

            # -j > 1 breaks this makefile
            command             => qq{make clean && make -j 1 NETCDFPATH=$install_path NETCDF=enable NETCDF4=enable NETCDF4_COMPRESSION=enable MACHINENAME=$machinename compiler=$compiler},
            clean_command       => q{make clean},
            skip_if             => sub { 0 },
            precondition_check  => sub { 1 },
            postcondition_check => sub {
                my ( $op, $opts_ref ) = @_;
                return -e qq{./netcdf2adcirc.x};
            },
        },
        {
            key                 => q{util},
            name                => q{Step for in util/},
            description         => q{Runs the makefile and builds all associated utilities in the util/ directory.},
            pwd                 => q{./util},
            command             => qq{make clean && make -j $makejobs NETCDFPATH=$install_path NETCDF=enable NETCDF4=enable NETCDF4_COMPRESSION=enable MACHINENAME=$machinename compiler=$compiler},
            clean_command       => q{make clean},
            skip_if             => sub { 0 },
            precondition_check  => sub { 1 },
            postcondition_check => sub { my ( $op, $opts_ref ) = @_; return -e qq{./makeMax.x}; },
        },
        {
            key                 => q{input-mesh},
            name                => q{Step for in util/input/mesh},
            description         => q{Runs the makefile and builds all associated util/input/mesh in the util/ directory.},
            pwd                 => qq{./util/input/mesh},
            command             => qq{make clean && make -j $makejobs NETCDFPATH=$install_path NETCDF=enable NETCDF4=enable NETCDF4_COMPRESSION=enable MACHINENAME=$machinename compiler=$compiler},
            clean_command       => q{make clean},
            skip_if             => sub { 0 },
            precondition_check  => sub { 1 },
            postcondition_check => sub {
                my ( $op, $opts_ref ) = @_;
                return -e qq{./boundaryFinder.x};
            },
        },
        {
            key         => q{input-nodalattr},
            name        => q{Step for in util/input/nodalattr},
            description => q{Runs the makefile and builds associated utilities in the util/input/nodalattr directory.},
            pwd         => q{./util/input/nodalattr},

            # -j > 1 breaks this makefile
            command             => qq{make clean && make -j 1 NETCDFPATH=$install_path NETCDF=enable NETCDF4=enable NETCDF4_COMPRESSION=enable MACHINENAME=$machinename compiler=$compiler},
            clean_command       => q{make clean},
            skip_if             => sub { 0 },
            precondition_check  => sub { 1 },
            postcondition_check => sub { my ( $op, $opts_ref ) = @_; return -e qq{./convertna.x}; },
        },
        {
            key           => q{perlbrew},
            name          => q{Step for perlbrew and perl for ASGS},
            description   => q{Installs local Perl version used for ASGS.},
            pwd           => q{./},
            command       => q{bash ./cloud/general/init-perlbrew.sh},
            clean_command => q{bash ./cloud/general/init-perlbrew.sh clean},

            # augment existing %ENV (cumulative) - this assumes that perlbrew is installed in $HOME and we're
            # using perl-5.28.2
            export_ENV => {
                PATH             => { value => qq{$home/perl5/perlbrew/perls/perl-5.28.2/bin},                          how => q{prepend} },
                PERLBREW_PERL    => { value => q{perl-5.28.2},                                                          how => q{replace} },
                PERLBREW_MANPATH => { value => qq{$home/perl5/perlbrew/perls/perl-5.28.2/man},                          how => q{prepend} },
                PERLBREW_PATH    => { value => qq{$home/perl5/perlbrew/bin:$home/perl5/perlbrew/perls/perl-5.28.2/bin}, how => q{prepend} },
                PERLBREW_HOME    => { value => qq{$home/.perlbrew},                                                     how => q{replace} },
                PERLBREW_ROOT    => { value => qq{$home/perl5/perlbrew},                                                how => q{replace} },
            },
            skip_if             => sub { return ( -e qq{$home/perl5/perlbrew/perls/perl-5.28.2/bin/perl} ) ? 1 : 0 },
            postcondition_check => sub {
                my ( $op, $opts_ref ) = @_;
                return -e qq{$home/perl5/perlbrew/etc/bashrc};
            },
        },
        {
            key                 => q{perl-modules},
            name                => q{Step for installing required Perl modulesS},
            description         => q{Installs local Perl modules used for ASGS.},
            pwd                 => q{./},
            command             => q{bash ./cloud/general/init-perl-modules.sh},
            clean_command       => q{},
            precondition_check  => sub { return ( -e qq{$home/perl5/perlbrew/perls/perl-5.28.2/bin/perl} ) ? 1 : 0 },
        },
    ];
    return $steps;
}

1;

__END__

=head1 NAME

asgs-brew.pl

=head1 DESCRIPTION

This script is handles building all necessary libraries and utilities that are required for
a functioning local ASGS environment. It can be extended by registering makefiles, scripts,
or pure commands in the C<get_steps> subroutine. Steps are processed in the order that they appear.

=head2 Note on script dependencies

It is very intentional that this script doesn't require anything other than core Perl libraries
that are readily available in any environment that perl is also available.

=head1 SYNOPSIS

Options generally reflect those values that are passed on to the various makefiles:

    ./asgs-brew.pl --machinename <MachineName> --compiler <CompilerFamily> [--install-path some/path --home /path/other/than/user/$HOME --force]

Note: C<--install-path> defaults to $HOME/opt.

There is also a "clean" mode that will invoke the C<clean_command> for any step that defines it:

    ./asgs-brew.pl --clean

=head1 OPTIONS

Options translate loosely to the options that are required directly in the commands that each
step requires, and they can be added quite easily to accomodate new pieces of information.
Below is a summary of what's been added so far.

=over 3

=item C<--clean>

For each step, only the C<clean_command> (if defined) is run and then the script quits. The
purpose of this is to provide access to the the C<clean> target that makefiles generally
provide, but any command can be specificed in the step definition.

=item C<--compiler>

This option allows one to define the compiler family or group, as it is typically passed to
makefiles used by ASGS. The two most common values for this flag are going to be C<gfortran>
and C<intel>.  The step is not required to use this value. It is merely passed along so that
the C<command> string may have access to the value if it is needed.

=item C<--debug-skip-steps>

This flag is for debugging only so that one may target a specific step, it is not meant for
the general run case of building up the ASGS environment. It accepts a comma delimited list
of run step keys (no spaces) to skip. For example, if one wished to skip the C<perlbrew>,
C<openmpi>, and C<hdf5-netcdf> steps and begin with the C<wgrib2> step - but continue through
the rest of the list, the flag would be specificed as,

   --debug-skip-steps=perlbrew,openmpi,hdf5-netcdf

To get the list of keys, use the C<--list-keys> option.

=item C<--dump-env>

Runs through each step, but only sets up the environment that is specified (if it is specified).
It then prints to STDOUT the list of variables and their values in a way that is suitable to be
used in a bash script.

=item C<--force>

If used, the C<skip_if> defined for a step (if defined) is not run. C<--debug-skip-steps> is
checked before C<--force> and therefore overrides it.

=item C<--home>

The default is set to the effective user's actual home directory, which is the value that the
environmental variable $HOME is typically assigned. As with the C<--compiler> flag, this value
may or may not be used to define some part of a step.

=item C<--install-path>

Equivalent to config's C<--prefix> option, available for use as the main parent directory under
which to pass to scripts, makefiles, and other commands as the intended home for all of the
utilities you wish to rehome.

=item C<--list-steps>

Prints a nice listing of each step's key (in order of execution) and the description. It then
exits, doing nothing else. It's handy when using C<--debug-skip-steps> since this option takes
a list of keys to skip.

=item C<--machinename>

This option allows one to define the C<machine> name, which is a common value that is used in
typical ASGS makefiles. It is made available for use when defining a step.

=item C<--make-jobs>

Provides a way to specify the the level of concurrency one may use with a makefile when defining
the command in a step. Some makefiles are not properly able to use this option, so it is optionally
used when defining the step command itself.

=back

=head1 ENVIRONMENT AND CONFIGURATION

When a Perl script is executed, it stores the user's environmental variables (e.g., the
output of the C<env> command) into a global hash, %ENV. This script makes a local copy
of that hash so that it may use and modify it as each step requires.  Each step may define
a set of environmental variables it wishes to set and make available for subsequent
steps. Once %ENV is updated, all subsequent perl commands and spawned subshells will
have access to the modified %ENV. For example, when a Perl script runs a system command
using the backticks (e.g., `some command from the shell`), the environmental variables
and their values are governed by the %ENV global variable.

When a step builds a library that is a dependency for subsequent steps, it is a good
idea to leverage the C<export_ENV> key for each step to define the variable and what
it's new value should be.

A good example is the step that builds the NETCDF and HDF5 libraries, utlized extensively
by any utility that must read or modify ADCIRC output files, hot start files, or
external forcing data. Therefore, before the step is executed, the following variables
are updated in %ENV:

=over 3

=item LD_LIBRARY_PATH

=item LD_INCLUDE_PATH

=item PATH           

=back

=head2 Exporting The Environment

asgs-brew.pl doesn't automatically update the user's environment as it exists after
all steps have been run successfully. Since there is the ability to export environmental
variables in each step (available for the current and subsequent steps), it might be desired
to be able to recreate this post-run.  To get a dump of the variables set, how they are
set, one may use the C<dump-env> option. Adding this to the set of options provided at
build time will produce the update variables as they appear at the end of a fully successful
running of all steps. 

=head1 ADDING AND MANAGING STEPS 

When adding a new step, it is important to consider where in the order of steps
it should appear. If it's a library used by many utilities (e.g., NetCDF or HDF5),
additional care must be made when specifying the step - in particular the
environmental variables that is standard for compilers utilize, such as C<LD_INCLUDE_PATH>
or C<LD_LIBRARY_PATH>.

The defined C<keys> to define when adding a step are as follow,

=over 3

=item C<key>

Unique key that is meant to unambiguously refer to the step in options that may accept
a list of steps. For example, it's required for using the C<--list-steps> flag.

=item C<description>

Provides a fair, short summary of the step. Required for the C<--list-steps> flag to
act appropriately.

=item C<name>

Short name for the step, doesn't have to be unique across steps and runs, but it's good
to make sure that the name is short and informative.

=item C<pwd>

The directory specified is the one from where the step's C<command> should be run.

=item C<command>

This is the primary command is run in the current step unless the C<--clean> flag
is specified. Options are passed from the commandline of asgs-brew.pl to the C<$opts_ref>
hash reference by using the C<Getops::Long> options definition in the C<run> subroutine.
Options can be added easily to the arguments list of asgs-brew.pl, but the general rule
of thumb should be that the options be kept to a minimum. Most of the complexity associated
with a step should be hidden within the step's makefile, script, or program supporting
the command.

=item C<export_ENV>

Environmental variables affect a great number of things in this space, so it is important
to be able to manage them as each step completes. Each step may define any number of environmental
variables and how they should be updated. 

Below is an example used for the C<hdf5-netcdf> step. This example demonstrates which variables
to update, with what value, and how.

    export_ENV => {
	LD_LIBRARY_PATH => { value => qq{$install_path/lib},     how => q{prepend} },
	LD_INCLUDE_PATH => { value => qq{$install_path/include}, how => q{prepend} },
	NETCDFHOME      => { value => qq{$install_path},         how => q{replace} },
    },

Options for C<how> include: C<prepend> (default if not defined), C<append>, and C<replace>.

=item C<clean_command> 

Defines the command used to C<clean> a directory tree.

=item C<skip_if>

Defines the condition on which the run step can be skipped. This check can be bypassed
if the C<--force> flag is passed. It is similar in nature to the C<postcondition_check>
or C<precondition_check>, but has been separated out for the purpose of selectively
ignoring a run step if desired. This check happens BEFORE C<precondition_check>.

=item C<precondition_check>

If this method is private, it is run before C<command>. If it fails, then appropriate
action should be taken to fix that.

=item C<postcondition_check>

If this method is private, it is run after C<command>. If it fails, then appropriate.

=back

=head2 A Note about Interdependency Among Steps

This script has no means of enforcing dependency among scripts, but this can be added
if needed in the future. The steps are executed serially, but there is no limit to the
concurrency that each individiual step can have - for example, C<make> is very good at
concurrently running independent portions of the makefile if it is set up correctly
(and by using the C<-j> option).

It is assumed that each step MUST complete successfully, and this is why there is a
C<postcondition_check> method that can be customized to ensure that the expected
state after the step is complete has been satisfied. Failure of this method at any
step will result in the script stopping. Similarly, each step is able to also define
a C<precondition_check> that also must be evaluate positively if the step itself is
to be attempted.

=head1 LICENSE AND COPYRIGHT

This file is part of the ADCIRC Surge Guidance System (ASGS).
The ASGS is free software: you can redistribute it and/or modify it under the terms
of the GNU General Public License as published by the Free Software Foundation, either
version 3 of the License, or (at your option) any later version.

ASGS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with the ASGS. If
not, see <http://www.gnu.org/licenses/>.
