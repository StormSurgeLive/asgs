#!/usr/bin/perl
package Date::Handler::Range;

use strict;
use Carp;
use Data::Dumper;

use Date::Handler;
use Date::Handler::Delta;

use vars qw($VERSION);
$VERSION = '1.0';

sub new
{
	my ($classname, $args) = @_;

	croak "No arguments to new()" if not defined $args;
	croak "No date argument passed to new()" if not defined $args->{date};

	my $self = {};
	bless $self, $classname;

	if(ref($args) eq 'HASH')
	{
		#Get the date from the arguments.
		if(UNIVERSAL::isa($args->{date}, 'Date::Handler'))
		{
			$self->{date} = $args->{date};
		}
		else
		{
			my $date = new Date::Handler({ date => $args->{date} });
			if(defined $date)
			{
				$self->{date} = $date;
			}
			else
			{
				croak "Invalid format for date in new()";
			}
		}
	
		if(UNIVERSAL::isa($args->{delta}, 'Date::Handler::Delta'))
		{
			$self->{delta} = $args->{delta};
		}
		else
		{
			my $delta = Date::Handler::Delta->new($args->{delta});
			if(defined $delta)
			{
				$self->{delta} = $delta;
			}
			else
			{
				croak "Invalid format for delta in new()";
			}
		}
	}
	else
	{
		croak "Invalid arguments passed to new() (not HASHREF)";
	}

	return $self;
}


sub Direction
{
	my $self = shift;
	my $direction = shift;

	if(defined $direction)
	{
		if($direction =~ /FORWARDS/i || $direction eq 1)
		{
			$self->{direction} = 1;
		}
		elsif($direction =~ /BACKWARDS/i || $direction eq 0)
		{
			$self->{direction} = 0;
		}
	}

	$self->{direction} = 1 if not defined $self->{direction};
	return $self->{direction};
}
			

sub Delta
{
	my $self = shift;
	my $delta = shift;

	if(defined $delta && $delta->isa('Date::Handler::Delta'))
	{
		$self->{delta} = $delta;
	}
	return $self->{delta};
}

sub Date
{
	my $self = shift;
	my $date = shift;

	if(defined $date && $date->isa('Date::Handler'))
	{
		$self->{date} = $date;
	}
	return $self->{date};
}

sub StartDate
{
	my $self = shift;

	if($self->Direction())
	{
		return $self->Date();
	}
	else
	{	
		return $self->Date() - $self->Delta();
	}
}

sub EndDate
{
	my $self = shift;
	
	if($self->Direction())
	{
		return $self->Date() + $self->Delta();
	}
	else
	{
		return $self->Date();
	}
}

sub Overlaps
{
	my $self = shift;
	my $range = shift;

	croak "Arguments to Overlaps() is not a Date::Handler::Range" if !$range->isa('Date::Handler::Range');

	return 0 if($self->EndDate() <= $range->StartDate());
	return 0 if($range->EndDate() <= $self->StartDate());
	return 1;

}

	

666;
__END__

=head1 NAME

Date::Handler::Range - Time range object 

=head1 SYNOPSIS

  use Date::Handler::Range;
 
   my $range = new Date::Handler::Range( { date => $date, delta => $delta);

   $range->new						Constructor	
   $range->StartDate()					Returns a Date::Handler object of the range's start date.
   $range->EndDate()					Returns a Date::Handler object of the range's end date.
   $range->Overlaps($range2)				Returns true if $range overlaps $range2
   $range->Direction('FORWARDS' || 'BACKWARDS') 	Sets the direction of the range	
   $range->Delta()					Returns the delta contained by this range.
   $range->Date()					Returns the date contained by this range.

=head1 DESCRIPTION

Range objects are used to specify moments in time between 2 dates. For the moment, this functionnality
is very primitive but will evolve in future versions. Each range object is constructed by a base Date::Handler
object. Then combined to that is a Date::Handler::Delta will specify the length of the range. Then thirdly, the
range is given a direction, either 'FORWARDS' or 'BACKWARDS' in time. From these three values are calculated
a start date and an end date.

Example:

	#Creation of the range starting now, ending in 2 days.
	my $range = Date::Handler::Range->new({
							date => time(),
							delta => [0,0,2,0,0],
	});

	#This is already set to 'FORWARDS' by default.
	$range->Direction('FORWARDS');

	print "This range start ".$range->StartDate()." and ends ". $range->EndDate()."\n";

	#Another range, starting at the end date of range 1, going backwards in time 7 days.
	my $range2 = Date::Handler::Range->new({
						date => $range->StartDate(),
						delta => [0,0,7,0,0],
						});

	$range2->Direction('BACKWARDS');

	#This returns true if range overlaps range2.
	$range->Overlaps($range2);


As I said earlier, the range functionnality is still very primitive, and the interface might change.
Refer to Date::Handler::Range(1) for more information.


=head1 BUGS (known)

Deltas going after 2038 are not handled by this module yet. (POSIX)

Deltas before 1902 are not handled by this module. (POSIX)

If you find bugs with this module, do not hesitate to contact the author.
Your comments and rants are welcomed :)

=head1 TODO

Implement overloading on ranges.
Provide the value of the overlap in Overlaps()

=head1 COPYRIGHT

Copyright(c) 2001 Benoit Beausejour <bbeausej@pobox.com>

All rights reserved. This program is free software; you can redistribute it and/or modify it under the same
terms as Perl itself. 

Portions Copyright (c) Philippe M. Chiasson <gozer@cpan.org>

Portions Copyright (c) Szabó, Balázs <dlux@kapu.hu>

Portions Copyright (c) Larry Rosler 


=head1 AUTHOR

Benoit Beausejour <bbeausej@pobox.com>

=head1 SEE ALSO

Date::Handler(1).
Date::Handler::Delta(1).
Class::Date(1).
Time::Object(1).
Date::Calc(1).
perl(1).

=cut
