#!/usr/bin/perl
package Date::Handler;

use strict;
use Carp;
use Data::Dumper;
use vars qw(@ISA $VERSION);

$VERSION = '1.2';

use POSIX qw(floor strftime mktime setlocale);

use Date::Handler::Constants;
use constant DEFAULT_FORMAT_STRING => '%c';
use constant DEFAULT_TIMEZONE => 'GMT';
use constant DEFAULT_LOCALE => 'en_US';

use constant DELTA_CLASS => 'Date::Handler::Delta';

use constant INTUITIVE_MONTH_CALCULATIONS => 0;
use constant INTUITIVE_TIME_CALCULATIONS => 0;
use constant INTUITIVE_DST_ADJUSTMENTS => 0;

use overload (
	'""'	=> 'AsScalar',
	'0+'	=> 'AsNumber',
	'+'	=>	'Add',
	'-'	=>	'Sub',
	'<=>'	=>	'Cmp',
	'++' => 'Incr',
	'*' => sub { croak "Cannot multiply an absolute date"; },
	'**' => sub { croak "Cannot power an absolute date"; },
	'/' => sub { croak "Cannot divide an absolute date"; },
	fallback	=> 1,
);


sub new
{
	my $classname = shift;
	my $args = shift;


	# Allow classic style arguments passing. # Thanks to Roland Rauch <roland@rauch.com> for the spot 
	unless (ref($args) eq "HASH") 
	{ 
		unshift(@_, $args);             
		$args = {@_};                
	}

	my $self = {};
	bless $self, $classname;

	croak "No args to new()" if not defined $args;
	croak "Argument to new() is not a hashref" if not ref($args) =~ /HASH/;
	croak "No date specified for new()" if not defined $args->{date};

	my $date = $args->{date};

	my $timezone = $args->{time_zone} || $self->DEFAULT_TIMEZONE();
	$self->TimeZone($timezone);

	$self->{locale} = "";
	if(defined $args->{locale})
	{
		$self->SetLocale($args->{locale}) || $self->SetLocale($self->DEFAULT_LOCALE());
	}
	else
	{
		$self->SetLocale($self->DEFAULT_LOCALE());
	}

	if(not defined $self->Locale())
	{
		warn "Impossible to set locale OR default locale correctly. Defaulting to GMT/UTC.";
		$self->SetLocale('GMT');
	}


	if(ref($date) =~ /SCALAR/)
	{
		if($date !~ /\s/ && $date !~ /[A-Za-z]/)
		{
			$self->{epoch} = $date;
		}
	}
	elsif(ref($date) =~ /ARRAY/)
	{
		$self->{epoch} = $self->Array2Epoch($date);
	}
	elsif(ref($date) =~ /HASH/)
	{
		$self->{epoch} = $self->Array2Epoch([ 
						$date->{year},
						$date->{month},
						$date->{day},
						$date->{hour},
						$date->{min},
						$date->{sec},
		]);
	}
	else
	{
		if($date !~ /\s/ && $date !~ /[A-Za-z]/)
		{
			$self->{epoch} = $date;
		}
	}

	$self->{_intuitive_day} = $args->{intuitive_day} if($self->INTUITIVE_MONTH_CALCULATIONS());
	$self->{_intuitive_hour} = $args->{intuitive_hour} if($self->INTUITIVE_TIME_CALCULATIONS());

	croak "Date format not recognized." if not defined $self->{epoch};

	return $self;
}	


#Accessors (Might want to optimised some of those)
sub Year { return shift->AsArray()->[0]; }
sub Day { return shift->AsArray()->[2]; }
sub Hour { return shift->AsArray()->[3]; }
sub Min { return shift->AsArray()->[4]; }
sub Sec { return shift->AsArray()->[5]; }


#To be consistent with our WeekDay function, wich is zero based.
sub Month
{
	my $self = shift;

	local $ENV{'TZ'} = $self->TimeZone();
	local $ENV{'LC_TIME'} = $self->Locale();

	return strftime('%m', localtime($self->{epoch}));
}

sub Epoch
{
	my $self = shift;

	if(@_)
	{
		my $epoch = shift;

		$self->{epoch} = $epoch;
	}

	return $self->{epoch};
}

sub TimeZone
{
	my $self = shift;
	
	if(@_)
	{
		my $time_zone = shift;
		$self->{time_zone} = $time_zone;
	}

	return $self->{time_zone};
}

sub Locale
{
	my $self = shift;

	if(@_)
	{
		warn "Calling Locale() with an argument to set the locale is deprecated. Please use SetLocale(locale) instead.\n";

		return $self->SetLocale(@_); 
	}

	return $self->{locale};
}

sub SetLocale
{
	my $self = shift;
	my $locale = shift;

	croak "No locale passed to SetLocale()" if not defined $locale;

	my $locale_return = POSIX::setlocale(&POSIX::LC_TIME, $locale);

	if( defined $locale_return )
	{
		$self->{locale} = $locale;
		$self->{locale_realname} = $locale_return;

		return $self->{locale};
	}

	print STDERR "Locale $locale does not seem to be implemented on this system, keeping locale ".$self->{locale}."\n";
	return undef;
}

	
sub LocaleRealName
{
	my $self = shift;

	return $self->{locale_realname} || $self->DEFAULT_LOCALE();
}
	
#Time Conversion and info methods 

sub TimeZoneName
{
	my $self = shift;

	local $ENV{'TZ'} = $self->TimeZone();
	local $ENV{'LC_TIME'} = $self->Locale();

	#Old code.
	#my ($std,$dst) = POSIX::tzname();
	#return $std." / ".$dst;

	return strftime("%Z", localtime($self->{epoch}) );
}

sub LocalTime
{
	my $self = shift;

	local $ENV{'TZ'} = $self->TimeZone();
	local $ENV{'LC_TIME'} = $self->Locale();

	return localtime($self->{epoch});
}


sub TimeFormat
{
	my $self = shift;
	my $format_string = shift;

	local $ENV{'TZ'} = $self->TimeZone();
	local $ENV{'LC_TIME'} = $self->Locale();
		
	$format_string ||= $self->DEFAULT_FORMAT_STRING(); 

	return strftime($format_string, localtime($self->{epoch}));
}


sub GmtTime
{
	my $self = shift;

	local $ENV{'TZ'} = $self->TimeZone();
	local $ENV{'LC_TIME'} = $self->Locale();

	return gmtime($self->{epoch});
}

sub UtcTime
{
	my $self = shift;

	local $ENV{'TZ'} = $self->TimeZone();
	local $ENV{'LC_TIME'} = $self->Locale();

	return gmtime($self->{epoch});
}


#Idea and base code for this function from:
# Larry Rosler, February 13, 1999, Thanks Larry! -<bbeausej@pobox.com>

sub GmtOffset 
{
	my $self = shift;

	local $ENV{'TZ'} = $self->TimeZone();
	local $ENV{'LC_TIME'} = $self->Locale();

	#Old code.
	#use Time::Local;
	#my $gmt_time = timegm( gmtime $self->{epoch} );
	#my $local_time = timelocal( gmtime $self->{epoch} );


	my $now = $self->Epoch();
	
	my ($l_min, $l_hour, $l_year, $l_yday) = (localtime $now)[1, 2, 5, 7];
	my ($g_min, $g_hour, $g_year, $g_yday) = (gmtime $now)[1, 2, 5, 7];

	return (($l_min - $g_min)/60 + $l_hour - $g_hour + 24 * ($l_year - $g_year || $l_yday - $g_yday)) * 3600;
}


#Useful methods
sub MonthName
{
	my $self = shift;

	local $ENV{'TZ'} = $self->TimeZone();
	local $ENV{'LC_TIME'} = $self->Locale();

	return strftime('%B', localtime($self->{epoch}));
}

sub WeekDay
{
	my $self = shift;

	local $ENV{'TZ'} = $self->TimeZone();
	local $ENV{'LC_TIME'} = $self->Locale();

	return strftime('%u', localtime($self->{epoch}));
}

sub WeekDayName
{
	my $self = shift;

	local $ENV{'TZ'} = $self->TimeZone();
	local $ENV{'LC_TIME'} = $self->Locale();

	return strftime('%A', localtime($self->{epoch}));
}

sub FirstWeekDayOfMonth
{
	my $self = shift;

	local $ENV{'TZ'} = $self->TimeZone();
	local $ENV{'LC_TIME'} = $self->Locale();
		
	return (($self->WeekDay() - $self->Day() % 7) + 8) % 7;
}

sub WeekOfMonth
{
	my $self = shift;

	local $ENV{'TZ'} = $self->TimeZone();
	local $ENV{'LC_TIME'} = $self->Locale();

	return int(($self->Day() + $self->FirstWeekDayOfMonth() - 1) / 7) + 1;
}
	

sub DaysInMonth
{
	my $self = shift;

	local $ENV{'TZ'} = $self->TimeZone();
	local $ENV{'LC_TIME'} = $self->Locale();

	my $month = $self->Month() - 1;

	if($month == 1) #Feb
	{
		return 29 if $self->IsLeapYear();
		return 28;
	}
	else
	{
		return $DAYS_IN_MONTH->{$month};
	}
}
	
sub DayLightSavings
{
	my $self = shift;

	local $ENV{'TZ'} = $self->TimeZone();
	local $ENV{'LC_TIME'} = $self->Locale();

	my @self_localtime = localtime($self->{epoch});

	return $self_localtime[8]; 
}

sub DayOfYear
{
	my $self = shift;
	
	local $ENV{'TZ'} = $self->TimeZone();
	local $ENV{'LC_TIME'} = $self->Locale();
	
	my @self_localtime = localtime($self->{epoch});

	return $self_localtime[7];
}

sub DaysInYear
{
	my $self = shift;

	local $ENV{'TZ'} = $self->TimeZone();
	local $ENV{'LC_TIME'} = $self->Locale();

	return 365 if !$self->IsLeapYear();
	return 366 if $self->IsLeapYear();
}

sub DaysLeftInYear
{
	my $self = shift;

	local $ENV{'TZ'} = $self->TimeZone();
	local $ENV{'LC_TIME'} = $self->Locale();

	my $days = $self->DaysInYear();
	my $day = $self->DayOfYear();

	return $days - $day;
}	

sub LastDayOfMonth
{
	my $self = shift;

	local $ENV{'TZ'} = $self->TimeZone();
	local $ENV{'LC_TIME'} = $self->Locale();

	if($self->Day() >= $self->DaysInMonth())
	{
		return 1;
	}
		
}

sub IsLeapYear
{
	my $self = shift;

	local $ENV{'TZ'} = $self->TimeZone();
	local $ENV{'LC_TIME'} = $self->Locale();

	my $year = $self->Year();
 
	return 1 if( !($year % 400) );
	return 1 if( !($year %4) && ($year % 100) );
	return 0;
}

sub IntuitiveDay
{
	my $self = shift;
	my $intuitive_day = shift;

	if($intuitive_day)
	{
		$self->{_intuitive_day} = $intuitive_day;	
	}
	return $self->{_intuitive_day};
}

sub IntuitiveHour
{
	my $self = shift;
	my $intuitive_hour = shift;
	
	if($intuitive_hour)
	{
		$self->{_intuitive_hour} = $intuitive_hour;
	}
	return $self->{_intuitive_hour};
}

sub Array2Epoch
{
	my $self = shift;
	my $input = shift;

	my ($y,$m,$d,$h,$mm,$ss) = @{$input}[0,1,2,3,4,5];

	local $ENV{'TZ'} = $self->TimeZone();
	local $ENV{'LC_TIME'} = $self->Locale();

	return mktime(
							$ss || 0, 
							$mm || 0, 
							$h || 0, 
							$d || 1,
							($m || 1)-1,
							($y || 2000)-1900,
							0,
							0,
							-1);
}


#Oveload methods.

sub AsScalar { return shift->TimeFormat(shift); }
sub AsNumber { return shift->{epoch}; }

sub AsArray
{
	my $self = shift;

	local $ENV{'TZ'} = $self->TimeZone();
	local $ENV{'LC_TIME'} = $self->Locale();


	my ($ss,$mm,$h,$d,$m,$y) = localtime($self->{epoch});
	$y += 1900;
	$m += 1;

	return [$y,$m,$d,$h,$mm,$ss];
}

sub AsHash
{
	my $self = shift;

	my $self_array = $self->AsArray();

	return {
				year => $self_array->[0],
				month => $self_array->[1],
				day => $self_array->[2],
				hour => $self_array->[3],
				min => $self_array->[4],
				sec => $self_array->[5],
	};
}


sub Add 
{
	my ($self, $delta) = @_;

	if(!ref($delta))
	{
		$delta = $self->DELTA_CLASS()->new([0,0,0,0,0,$delta]);
		return $self + $delta;
	}
	elsif($delta->isa($self->DELTA_CLASS()))
	{
		local $ENV{'TZ'} = $self->TimeZone();
		local $ENV{'LC_TIME'} = $self->Locale();


		my $epoch = $self->{epoch};

		my $newdate = ref($self)->new({ date => $epoch, time_zone => $self->TimeZone() });

		my $self_array = $newdate->AsArray();
		#Take care of the months.
		$self_array->[1] += $delta->Months();

		my $years = floor(($self_array->[1]-1)/12);
		$self_array->[1] -= 12*$years;

		#Take care of the years.
		$self_array->[0] += $years;

		my $posix_date = ref($self)->new({ date => $self_array,
						 time_zone => $self->TimeZone(), 
		});

		if($self->INTUITIVE_MONTH_CALCULATIONS())
		{
			if((($self->Month() + $delta->Months() - 1) % 12 + 1) != $posix_date->Month())
			{
				my $compensation_seconds = 86400 * $posix_date->Day();
				my $compensated_epoch = $posix_date->Epoch();
	
				$compensated_epoch -= $compensation_seconds;
	
				$posix_date->Epoch($compensated_epoch);
	
				$posix_date->{_intuitive_day} = $self->{_intuitive_day} || $self->Day();
			}
			else
			{
				if($self->{_intuitive_day})
				{
					my $lastdayofmonth = $self->{_intuitive_day};
					my $compensated_seconds = 86400 * ($lastdayofmonth - $posix_date->Day());
					if($compensated_seconds > 0)
					{
						my $epoch = $posix_date->Epoch();
						$epoch += $compensated_seconds;
						$posix_date->Epoch($epoch);
					}
	
					if($self->{_intuitive_day} > $lastdayofmonth)
					{
						$posix_date->{_intuitive_day} = $self->{_intuitive_day};
					}
					
				}
			}
		}

		#Take care of the seconds
		my $posix_epoch = $posix_date->Epoch();
		$posix_epoch += $delta->Seconds();
		$posix_date->Epoch($posix_epoch);


		my $adjustment_epoch = $posix_date->Epoch();
		my $add_intuitive_hour = 0;
		my $intuitive_hour;

		if($posix_date->DayLightSavings() && !$self->DayLightSavings())
		{
			my $posix_hour = $posix_date->Hour();
			$posix_hour -= 1;
			$intuitive_hour = $posix_hour;

			if($self->INTUITIVE_DST_ADJUSTMENTS())
			{
				$adjustment_epoch -= 3600;	
				$posix_date->Epoch($adjustment_epoch);

				$posix_hour = 0 if $posix_hour == 24;	
				if($posix_date->Hour() != $posix_hour)
				{
					$add_intuitive_hour = 1;
					$adjustment_epoch += 3600;
					$posix_date->Epoch($adjustment_epoch);
				}	
			}
		}
		elsif(!$posix_date->DayLightSavings() && $self->DayLightSavings())
		{

			my $posix_hour = $posix_date->Hour();
			$posix_hour += 1;
			$intuitive_hour = $posix_hour;

			if($self->INTUITIVE_DST_ADJUSTMENTS())
			{
				$adjustment_epoch += 3600;
				$posix_date->Epoch($adjustment_epoch);

				$posix_hour = 0 if $posix_hour == 24;	
				if($posix_date->Hour() != $posix_hour)
				{
					$add_intuitive_hour = 1;
					$adjustment_epoch -= 3600;
					$posix_date->Epoch($adjustment_epoch);
				}	
			}
		}

		if($self->INTUITIVE_TIME_CALCULATIONS())
		{
			if($add_intuitive_hour)
			{
				$posix_date->{_intuitive_hour} = $intuitive_hour;
			}

			if(defined $self->{_intuitive_hour})
			{
				my $hour = $posix_date->Hour();
				my $intuitive_epoch = $posix_date->Epoch();

				if($hour > $self->{_intuitive_hour})
				{		
					$intuitive_epoch -= 3600;
					$posix_date->Epoch($intuitive_epoch);
				}
				#elsif($hour < $self->{_intuitive_hour})
				#{
				#	print STDERR "Intuitive Adjust +1 hour\n";
				#	$intuitive_epoch += 3600;
				#	$posix_date->Epoch($intuitive_epoch);
				#}
			}
		}

		return $posix_date;

	}
	else
	{
		croak "Trying to add/substract an unknown object to a Date::Handler";
	}
}


sub Sub
{
	my ($self, $delta) = @_;

	if(!ref($delta))
	{
		$delta = $self->DELTA_CLASS()->new([0,0,0,0,0,$delta]);
		return $self - $delta;
	}
	elsif($delta->isa($self->DELTA_CLASS()))
	{
		return $self->Add(-$delta);
	}
	elsif($delta->isa('Date::Handler'))
	{

		my $seconds = $self->Epoch() - $delta->Epoch();

		if(($self->DayLightSavings() && !$delta->DayLightSavings()) ||
		   !$self->DayLightSavings() && $delta->DayLightSavings())	
		{
			$seconds += 3600;
		}

		return $self->DELTA_CLASS()->new($seconds);
	}
	else
	{
		croak "Cannot substract something else than a ".$self->DELTA_CLASS()." or Date::Handler or constant from a Date::Handler";
	}
}


sub Cmp 
{
	my ($self, $date, $reverse) = @_;

	my $cmp_date;

	if(!ref($date))
	{
		$cmp_date = $date;
	}
	elsif($date->isa('Date::Handler'))
	{
		$cmp_date = $date->{epoch};
	}
	elsif($date->isa($self->DELTA_CLASS()))
	{
		croak "Cannot compare a Date::Handler to a Delta.";
	}
	else
	{
		croak "Trying to compare a Date::Handler to an unknown object.";
	}
		
	return $self->{epoch} <=> $cmp_date;
}

sub Incr
{
	my ($self) = @_;

	my $epoch = $self->{epoch};
	$epoch++;

	return ref($self)->new({ date => $epoch, time_zone => $self->TimeZone() });
}


sub AllInfo
{
	my $self = shift;
	my $out_string;

	local $ENV{'TZ'} = $self->TimeZone();
	local $ENV{'LC_TIME'} = $self->Locale();

	$out_string .= "LocalTime: ".$self->LocalTime()."\n";
	$out_string .= "TimeFormat: ".$self->TimeFormat()."\n";
	$out_string .= "Epoch: ".$self->Epoch()."\n";
	$out_string .= "Locale: ".$self->Locale()."\n";
	$out_string .= "LocaleRealName: ".$self->LocaleRealName()."\n";
	$out_string .= "TimeZone: ".$self->TimeZone()." (".$self->TimeZoneName().")\n";
	$out_string .= "DayLightSavings: ".$self->DayLightSavings()."\n";
	$out_string .= "GMT Time: ".$self->GmtTime()."\n";	
	$out_string .= "GmtOffset: ".$self->GmtOffset()." (".($self->GmtOffset() / 60 / 60).")\n";
	$out_string .= "Year: ".$self->Year()."\n";
	$out_string .= "Month: ".$self->Month()."\n";
	$out_string .= "Day: ".$self->Day()."\n";
	$out_string .= "Hour: ".$self->Hour()."\n";
	$out_string .= "Min: ".$self->Min()."\n";
	$out_string .= "Sec: ".$self->Sec()."\n";
	$out_string .= "WeekDay: ".$self->WeekDay()."\n";
	$out_string .= "WeekDayName: ".$self->WeekDayName()."\n";
	$out_string .= "FirstWeekDayOfMonth: ".$self->FirstWeekDayOfMonth()."\n";
	$out_string .= "WeekOfMonth: ".$self->WeekOfMonth()."\n";
	$out_string .= "DayOfYear: ".$self->DayOfYear()."\n";
	$out_string .= "MonthName: ".$self->MonthName()."\n";
	$out_string .= "DaysInMonth: ".$self->DaysInMonth()."\n";
	$out_string .= "Leap Year: ".$self->IsLeapYear()."\n";
	$out_string .= "DaysInYear: ".$self->DaysInYear()."\n";
	$out_string .= "DaysLeftInYear: ".$self->DaysLeftInYear()."\n";
	$out_string .= "Intuitive Day: ".$self->IntuitiveDay()."\n";
	$out_string .= "Intuitive Hour: ".$self->IntuitiveHour()."\n";
	$out_string .= "\n\n";
	return $out_string;
}

666;
__END__
