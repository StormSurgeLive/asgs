#!/usr/bin/perl
package Date::Handler::Delta;

use strict;
use Carp;
use Data::Dumper;
use vars qw(@ISA $VERSION);
$VERSION = '1.0';

use POSIX qw(floor strftime mktime);

use overload (
	'""'	=> 'AsScalar',
	'0+'	=> 'AsNumber',
	'neg'	=>	'Neg',
	'+'	=>	'Add',
	'-'	=>	'Sub',
	'*' =>  'Mul',
	'/' =>  'Div',
	'<=>'	=>	'Cmp',
	'++' => 'Incr',
	'%' => sub { croak "Impossible to modulo a Date::Handler::Delta"; },
	'**' => sub { croak "Trying to obtain square virtual minutes out of a virtual delta boy ?"; }, 
	fallback	=> 1,
);


sub new
{
	my ($classname, $delta) = @_;

	my $self = {};
	bless $self, $classname;

	croak "No args to new()" if not defined $delta;

	if(ref($delta) =~ /ARRAY/)
	{
		my $delta_array = $self->DeltaFromArray($delta);
		$self->{months} = $delta_array->[0];
		$self->{seconds} = $delta_array->[1];
	}
	elsif(ref($delta) =~ /HASH/)
	{
		my $delta_array = $self->DeltaFromArray([
							$delta->{years},
							$delta->{months},
							$delta->{days},
							$delta->{hours},
							$delta->{minutes},
							$delta->{seconds},
							]);
		$self->{months} = $delta_array->[0];
		$self->{seconds} = $delta_array->[1];
							 
	}
	elsif(!ref($delta))
	{
		$self->{months} = 0;
		$self->{seconds} = $delta;
	}
	else
	{
		croak "Arguments to new in unknown format.";
	}
	
	croak "Could not parse delta" if !defined $self->{months} && !defined $self->{seconds};

	return $self;
}	



#Accessors (Might want to optimised some of those)
sub Months { return shift->{months}; }
sub Seconds  { return shift->{seconds}; }


#Oveload methods.
sub AsScalar { return sprintf("%d:%d", @{shift()->AsArray()}); }
sub AsNumber { return sprintf("%d.%d",@{shift()->AsArray()}); }

sub AsArray
{
	my $self = shift;
	return [$self->Months()||0,$self->Seconds()||0];
}

sub AsHash
{
	my $self = shift;

	return {
				month => $self->Months(),
				seconds => $self->Seconds(),
		};
}


sub Add 
{
	my ($self, $delta) = @_;


	my $self_array = $self->AsArray();

	if(!ref($delta))
	{
		$self_array->[1] += $delta;
	}
	elsif($delta->isa('Date::Handler::Delta'))
	{
		$self_array->[0] += $delta->Months();
		$self_array->[1] += $delta->Seconds();
	}
	elsif($delta->isa('Date::Handler'))
	{
		return $delta->Add($self);
	}
	else
	{
		$self_array->[1] += $delta;
	}

	return ref($self)->new([0, $self_array->[0], 0,0,0,$self_array->[1]]);
}

sub Sub 
{
	my ($self, $delta) = @_;

	if(ref($delta) && $delta->isa('Date::Handler'))
	{
		croak "Cannot substract a date from a Delta.";
	}

	return $self->Add(-$delta);
}
sub Cmp 
{
	my ($self, $delta) = @_;

	croak "Cannot compare a Delta with something else than another Delta" if(!ref($delta));

	if($delta->isa('Date::Handler::Delta'))
	{
		my $self_time = 24*60*60*(30*$self->Months())+$self->Seconds();
		my $delta_time = 24*60*60*(30*$delta->Months())+$delta->Seconds();

		return $self_time <=> $delta_time;
	}
	else
	{
		croak "Cannot compare a Delta with something else than another Delta";
	}
	
}

sub Mul 
{
	my ($self, $delta) = @_;

	if(!ref($delta)) 
	{
		my $months = $self->Months() * $delta;
		my $seconds = $self->Seconds() * $delta;

		return ref($self)->new([0, $months,0,0,0,$seconds]);
	}
	elsif($delta->isa('Date::Handler::Delta'))
	{
		croak "Cannot obtain square minutes from Delta multiplication";
	}
	elsif($delta->isa('Date::Handler'))
	{
		croak "Cannot Multiply a date with a delta.";
	}
	else
	{
		my $months = $self->Months() * $delta;
		my $seconds = $self->Seconds() * $delta;

		return ref($self)->new([0, $months,0,0,0,$seconds]);
	}

}
sub Div 
{
	my ($self, $delta) = @_;

	if(!ref($delta))
	{
		my $months = floor($self->Months() / $delta);
		my $seconds = floor($self->Seconds() / $delta);

		return ref($self)->new([0, $months,0,0,0,$seconds]);
	}
	elsif($delta->isa('Date::Handler::Delta'))
	{
		croak "Cannot divide a delta expressed in months and seconds." if($self->Months() && $self->Seconds());
		croak "Cannot divide a delta expressed in months and seconds." if($delta->Months() && $delta->Seconds());

		croak "You can only divide by a delta expressed in the same unit." if($self->Months() && $delta->Seconds());
		croak "You can only divide by a delta expressed in the same unit." if($self->Seconds() && $delta->Months());

		if($self->Months())
		{
			my $recurrence = $self->Months() / $delta->Months();
			return $recurrence;
		}
		else
		{
			if($delta->Seconds())
			{
				my $recurrence = $self->Seconds() / $delta->Seconds();
				return $recurrence;
			}
			else
			{
				return (0 / $delta->Months());	
			}
		}
	}
	elsif($delta->isa('Date::Handler'))
	{
		croak "Cannot divide a date and a delta.";
	}
	else
	{
		my $months = floor($self->Months() / $delta);
		my $seconds = floor($self->Seconds() / $delta);

		return ref($self)->new([0, $months,0,0,0,$seconds]);
	}
}

sub ApproximateInSeconds
{
	my $self = shift;
	my $direction = shift;

	if($direction eq 'over')
	{
		my $years = floor($self->Months() / 12);
		return $self->Seconds() + (24 * 60 * 60 * (($years * 366) + ($self->Months() % 12) * 31));
	}
	elsif($direction eq 'under')
	{
		my $years = floor($self->Months() / 12);
		my $months = $self->Months() % 12;
		my $months_as_days = 0;

		if($months)
		{
			$months--;
			$months_as_days += 28;	
			$months_as_days += $months * 30;
		}

		return $self->Seconds() + (24 * 60 * 60 * ( ($years * 365) + ($months_as_days) ) );
	}
	else
	{
		croak "Allowed argument for ApproximateInSeconds is 'over','under','average'";
	}
}

sub Incr
{
	my $self = shift;

	my $secs = $self->Seconds();

	return ref($self)->new([0, $self->Months(),0,0,0,$secs++]);
}

sub Neg
{
	my $self = shift;

	return ref($self)->new([0, -$self->Months(),0,0,0,-$self->Seconds()]);
}


#Useful methods.

#Taken from Class::Date
sub DeltaFromArray 
{
	my $self = shift;
	my $input = shift;
  	my ($y,$m,$d,$hh,$mm,$ss) = @{$input}[0,1,2,3,4,5];

	$y = 0 unless (defined $y);
	$y = floor($y * 12);
	$m += $y;
	return [$m||0,($ss||0)+60*(($mm||0)+60*(($hh||0)+24*($d||0)))];
}


666;
__END__

=head1 NAME

Date::Handler::Delta - Time lapse object 

=head1 SYNOPSIS

  use Date::Handler::Delta;
 
   my $delta = new Date::Handler::Delta([3,1,10,2,5,5]);
   my $delta = new Date::Handler::Delta({
						years => 3,
						months => 1,
						days => 10,
						hours => 2,
						minutes => 5,
						seconds => 5,
					});

   $delta->new				(More information in perldoc Date::Handler::Delta)
   $delta->Months() 			Number of months in delta
   $delta->Seconds() 			Number of seconds in delta
   $delta->AsScalar() 			"%d months and %d seconds"
   $delta->AsNumber() 			"%d-%d-%d"
   $delta->AsArray()			[y,m,ss]
   $delta->AsHash()			{ months => m, seconds => ss }

   $date + $delta = Date::Handler
   $date - $delta = Date::Handler
   $date - $date2 = Date::Handler::Delta
   $date + n = (+n seconds)
   $date - n = (-n seconds)

   $delta + $delta = Date::Handler::Delta
   $delta - $delta = Date::Handler::Delta
   $delta * n = Date::Handler::Delta
   $delta / n = Date::Handler::Delta
   $delta + n = (+n seconds)
   $delta - n = (-n seconds)



=head1 DESCRIPTION


Date::Handler::Delta is an object that represents a lapse of time. It's internal representation
of a time lapse if reduced to months and seconds. A Date::Handler::Delta object is always relative
to a Date::Handler object, it's calculation methods become active when the delta is applied to a date.


=head1 IMPLEMENTATION

Implementation details

=head2 Creating a Date::Handler::Delta object

The new() constructor receives only one argument as an array reference or hash reference:

	my $delta = Date::Handler::Delta->new([1,3,5,0,0]);
	my $delta = Date::Handler::Delta->new({
						years => 1,
						months => 3,
						days => 5,
						minutes= > 0,
						seconds => 0,
					});


=over 3 

=item * As array reference, the order if years, months, days, minutes seconds 

=item * As hash reference, the keys are years, months, days, minutes, seconds. 

=back


=head2 Accessors


You can access the data inside the object using any of the provided methods.
These methods are detailed in the SYNOPSIS up above.


Since Date::Handler uses operator overloading, you can 'apply' a Delta object on an absolute date
simply by using built-in operators. 

Example:

	#A Delta of 1 year.
	my $delta = new Date::Handler::Delta([1,0,0,0,0,0]);

	my $date = new Date::Handler({ date => time } );

	#$newdate is now one year in the furure.
	my $newdate = $date+$delta;
	
	
=head2 Operator overload special cases

The Date::Handler overloaded operator have special cases. Refer to the
SYNOPSIS to get a description of each overloaded operator's behaviour.

One special case of the overload is when adding an integer 'n' to a Date::Handler's reference. This is treated as if 'n' was in seconds. Same thing for substraction.

Example Uses of the overload:

	my $date = new Date::Handler({ date =>
					{
						year => 2001,
						month => 5,
						day => 14,
						hour => 5,
						min => 0,
						sec => 0,
					}});
	#Quoted string overload 
	print "Current date is $date\n";
	
	my $delta = new Date::Handler::Delta({ days => 5, });
	
	#'+' overload, now, $date is 5 days in the future.	
	$date += $delta;

	#Small clock. Not too accurate, but still ;)
	while(1)
	{
		#Add one second to the date. (same as $date + 1)
		$date++;
		print "$date\n";
		sleep(1);
	}


=head1 BUGS (known)

Deltas going after 2038 are not handled by this module yet. (POSIX)

Deltas before 1902 are not handled by this module. (POSIX)

If you find bugs with this module, do not hesitate to contact the author.
Your comments and rants are welcomed :)

=head1 TODO

Refine reduction to simplest expression of the delta.

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
Date::Handler::Range(1).
Class::Date(1).
Time::Object(1).
Date::Calc(1).
perl(1).

=cut

