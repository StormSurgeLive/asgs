package Date::Handler::Test;

use strict;
use Carp;
use Exporter;

use vars qw($VERSION @ISA @EXPORT @EXPORT_OK $AUTOLOAD);

$VERSION = sprintf '%d.%03d', q$Revision: 1.13 $ =~ /: (\d+).(\d+)/;

@ISA = qw(Exporter);
@EXPORT = qw(LoadTestConfig SkipTest);

use Test;
use Date::Handler;
use Date::Handler::Delta;
use Date::Handler::Range;
use Data::Dumper;

sub basic 
{
	#Tests basic concepts:
	#date + delta = date
	#delta + delta = delta
	#delta - delta = delta
	#delta * (+|-)n = (delta + delta [...n])

	plan tests => 145;

## Testing basic delta equalities
#1
ok ( '0:0', '0:0' );
#2
ok ( new Date::Handler::Delta([0,0,0,0,0,1]), new Date::Handler::Delta([0,0,0,0,0,1]) );
#3
ok ( new Date::Handler::Delta({years => 0, months => 0, days => 0, hours => 0, minutes => 0, seconds => 1}),
	 new Date::Handler::Delta({years => 0, months => 0, days => 0, hours => 0, minutes => 0, seconds => 1}) );

#4
ok ( '0:1', new Date::Handler::Delta([0,0,0,0,0,1]) );
#5
ok ( '0:1', new Date::Handler::Delta({
									years =>     0,
									months =>    0,
									days =>      0,
									hours =>     0,
									minutes =>   0,
									seconds =>   1,
								})	);
#6
ok ( new Date::Handler::Delta([0,0,0,0,0,1]), new Date::Handler::Delta({
																	years =>     0,
																	months =>    0,
																	days =>      0,
																	hours =>     0,
																	minutes =>   0,
																	seconds =>   1,
																	}) );
#7
ok ( new Date::Handler::Delta([0,13,366,24,60,61]),
	 new Date::Handler::Delta([1,1,367,1,1,1]) );


# Testing delta 0
#8 delta 0 + delta 0 = delta 0
ok ( new Date::Handler::Delta([0,0,0,0,0,0]) + new Date::Handler::Delta([0,0,0,0,0,0]),
		new Date::Handler::Delta([0,0,0,0,0,0]) );
#9 proof of error
ok ( new Date::Handler::Delta([0,0,0,0,0,0]) + new Date::Handler::Delta([0,0,0,0,0,0]) ne
		new Date::Handler::Delta([0,0,0,0,0,1]) );
#10 delta 0 + delta A = delta A
ok ( new Date::Handler::Delta([0,0,0,0,0,0]) + new Date::Handler::Delta([6,5,4,3,2,1]),
		new Date::Handler::Delta([6,5,4,3,2,1]) );
#11 proof of error
ok ( new Date::Handler::Delta([0,0,0,0,0,0]) + new Date::Handler::Delta([6,5,4,3,2,1]) ne
		new Date::Handler::Delta([6,5,4,3,2,0]) );
#12 delta A + delta 0 = delta A
ok ( new Date::Handler::Delta([6,5,4,3,2,1]) + new Date::Handler::Delta([0,0,0,0,0,0]),
		new Date::Handler::Delta([6,5,4,3,2,1]) );
#13 proof of error
ok ( new Date::Handler::Delta([6,5,4,3,2,1]) + new Date::Handler::Delta([0,0,0,0,0,0]) ne
		new Date::Handler::Delta([6,5,4,3,2,0]) );

# Testing non-zero deltas
#14 adding small deltas
ok ( new Date::Handler::Delta([6,0,4,0,2,0]) + new Date::Handler::Delta([0,5,0,3,0,1]),
		new Date::Handler::Delta([6,5,4,3,2,1]) );
#15 proof of error
ok ( new Date::Handler::Delta([6,0,4,0,2,0]) + new Date::Handler::Delta([0,5,0,3,0,1]) ne
		new Date::Handler::Delta([6,5,5,3,2,1]) );
#16 adding large deltas
ok ( new Date::Handler::Delta([12,23,34,45,56,67]) + new Date::Handler::Delta([23,34,45,56,67,78]),
		new Date::Handler::Delta([35,57,79,101,123,145]) );

## Testing positive deltas with the '-' operator

# Testing delta 0
#17 delta 0 - delta 0 = delta 0
ok ( new Date::Handler::Delta([0,0,0,0,0,0]) - new Date::Handler::Delta([0,0,0,0,0,0]),
		new Date::Handler::Delta([0,0,0,0,0,0]) );
#18 proof of error
ok ( new Date::Handler::Delta([0,0,0,0,0,0]) - new Date::Handler::Delta([0,0,0,0,0,0]) ne
		new Date::Handler::Delta([0,0,0,0,0,1]) );
#19 delta 0 - delta A = delta A
ok ( new Date::Handler::Delta([0,0,0,0,0,0]) - new Date::Handler::Delta([6,5,4,3,2,1]),
		new Date::Handler::Delta([-6,-5,-4,-3,-2,-1]) );
#20 proof of error
ok ( new Date::Handler::Delta([0,0,0,0,0,0]) - new Date::Handler::Delta([6,5,4,3,2,1]) ne
		new Date::Handler::Delta([6,5,4,3,2,0]) );
#21 delta A - delta 0 = delta A
ok ( new Date::Handler::Delta([6,5,4,3,2,1]) - new Date::Handler::Delta([0,0,0,0,0,0]),
		new Date::Handler::Delta([6,5,4,3,2,1]) );
#22 proof of error
ok ( new Date::Handler::Delta([6,5,4,3,2,1]) - new Date::Handler::Delta([0,0,0,0,0,0]) ne
		new Date::Handler::Delta([6,5,4,3,2,0]) );

# Testing non-zero deltas
#23 substracting small deltas
ok ( new Date::Handler::Delta([6,5,4,3,2,1]) - new Date::Handler::Delta([5,4,3,2,1,0]),
		new Date::Handler::Delta([1,1,1,1,1,1]) );
#24 proof of error
ok ( new Date::Handler::Delta([6,0,4,0,2,0]) - new Date::Handler::Delta([0,5,0,3,0,1]) ne
		new Date::Handler::Delta([6,5,5,3,2,1]) );
#25 substracting large deltas to obtain positive delta
ok ( new Date::Handler::Delta([23,34,45,56,67,78]) - new Date::Handler::Delta([12,23,34,45,56,67]),
		new Date::Handler::Delta([11,11,11,11,11,11]) );
#26 substracting large deltas to obtain negative delta
ok ( new Date::Handler::Delta([12,23,34,45,56,67]) - new Date::Handler::Delta([23,34,45,56,67,78]),
		new Date::Handler::Delta([-11,-11,-11,-11,-11,-11]) );

## Testing negative deltas with '+' operator

# Testing delta 0
#27 delta -0 + delta 0 = delta 0
ok ( new Date::Handler::Delta([-0,0,-0,0,-0,0]) + new Date::Handler::Delta([0,-0,0,-0,0,-0]),
		new Date::Handler::Delta([0,0,0,0,0,0]) );
#28 proof of error
ok ( new Date::Handler::Delta([-0,0,-0,0,-0,0]) + new Date::Handler::Delta([0,-0,0,-0,0,-0]) ne
		new Date::Handler::Delta([0,0,0,0,0,1]) );
#29 delta 0 + delta -A = delta -A
ok ( new Date::Handler::Delta([0,0,0,0,0,0]) + new Date::Handler::Delta([-6,-5,-4,-3,-2,-1]),
		new Date::Handler::Delta([-6,-5,-4,-3,-2,-1]) );
#30 proof of error
ok ( new Date::Handler::Delta([0,0,0,0,0,0]) + new Date::Handler::Delta([-6,-5,-4,-3,-2,-1]) ne
		new Date::Handler::Delta([6,5,4,3,2,1]) );
#31 delta -A + delta 0 = delta -A
ok ( new Date::Handler::Delta([-6,-5,-4,-3,-2,-1]) + new Date::Handler::Delta([0,0,0,0,0,0]),
		new Date::Handler::Delta([-6,-5,-4,-3,-2,-1]) );
#32 proof of error
ok ( new Date::Handler::Delta([-6,-5,-4,-3,-2,-1]) + new Date::Handler::Delta([0,0,0,0,0,0]) ne
		new Date::Handler::Delta([6,5,4,3,2,1]) );

# Testing non-zero deltas
#33 adding small deltas
ok ( new Date::Handler::Delta([-6,0,-4,0,-2,0]) + new Date::Handler::Delta([0,-5,0,-3,0,-1]),
		new Date::Handler::Delta([-6,-5,-4,-3,-2,-1]) );
#34 proof of error
ok ( new Date::Handler::Delta([-6,0,-4,0,-2,0]) + new Date::Handler::Delta([0,-5,0,-3,0,-1]) ne
		new Date::Handler::Delta([-6,5,5,3,2,1]) );
#35 adding large deltas
ok ( new Date::Handler::Delta([-12,-23,-34,-45,-56,-67]) + new Date::Handler::Delta([-23,-34,-45,-56,-67,-78]),
		new Date::Handler::Delta([-35,-57,-79,-101,-123,-145]) );

## Testing negative deltas with the '-' operator

#Testing delta 0
#36 delta -0 - delta 0 = delta 0
ok ( new Date::Handler::Delta([-0,0,-0,0,-0,0]) - new Date::Handler::Delta([0,-0,0,-0,0,-0]),
		new Date::Handler::Delta([0,0,0,0,0,0]) );
#37 proof of error
ok ( new Date::Handler::Delta([-0,0,-0,0,-0,0]) - new Date::Handler::Delta([0,-0,0,-0,0,-0]) ne
		new Date::Handler::Delta([0,0,0,0,0,1]) );
#38 delta 0 - delta -A = delta A
ok ( new Date::Handler::Delta([0,0,0,0,0,0]) - new Date::Handler::Delta([-6,-5,-4,-3,-2,-1]),
		new Date::Handler::Delta([6,5,4,3,2,1]) );
#39 proof of error
ok ( new Date::Handler::Delta([0,0,0,0,0,0]) - new Date::Handler::Delta([-6,-5,-4,-3,-2,-1]) ne
		new Date::Handler::Delta([-6,-5,-4,-3,-2,-1]) );
#40 delta -A - delta 0 = delta -A
ok ( new Date::Handler::Delta([-6,-5,-4,-3,-2,-1]) - new Date::Handler::Delta([0,0,0,0,0,0]),
		new Date::Handler::Delta([-6,-5,-4,-3,-2,-1]) );
#41 proof of error
ok ( new Date::Handler::Delta([-6,-5,-4,-3,-2,-1]) - new Date::Handler::Delta([0,0,0,0,0,0]) ne
		new Date::Handler::Delta([6,5,4,3,2,1]) );

# Testing non-zero deltas
#42 adding small deltas
ok ( new Date::Handler::Delta([-6,0,-4,0,-2,0]) - new Date::Handler::Delta([0,-5,0,-3,0,-1]),
		new Date::Handler::Delta([-6,5,-4,3,-2,1]) );
#43 proof of error
ok ( new Date::Handler::Delta([-6,0,-4,0,-2,0]) - new Date::Handler::Delta([0,-5,0,-3,0,-1]) ne
		new Date::Handler::Delta([-6,-5,-4,-3,-2,-1]) );
#44 adding large deltas
ok ( new Date::Handler::Delta([-12,-23,-34,-45,-56,-67]) - new Date::Handler::Delta([-23,-34,-45,-56,-67,-78]),
		new Date::Handler::Delta([11,11,11,11,11,11]) );
		
## A few last tests for additions and substractions of positive and negative deltas.
#45
ok ( new Date::Handler::Delta([6,0,4,0,2,0]) + new Date::Handler::Delta([0,-5,0,-3,0,-1]),
		new Date::Handler::Delta([5,7,3,21,1,59]) );

## Testing delta multiplications
# Testing delta zero
#46
ok ( new Date::Handler::Delta([0,0,0,0,0,0]) * 0,
		new Date::Handler::Delta([0,0,0,0,0,0]) );
#47
ok ( new Date::Handler::Delta([0,0,0,0,0,0]) * 1,
		new Date::Handler::Delta([0,0,0,0,0,0]) );
#48
ok ( new Date::Handler::Delta([0,0,0,0,0,0]) * 7,
		new Date::Handler::Delta([0,0,0,0,0,0]) );

# Testing non-zero deltas
#49 testing multiplication by zero
ok ( new Date::Handler::Delta([2,4,6,45,218,32]) * 0,
		new Date::Handler::Delta([0,0,0,0,0,0]) );
#50 testing multiplication by one
ok ( new Date::Handler::Delta([2,4,6,45,218,32]) * 1,
		new Date::Handler::Delta([2,4,6,45,218,32]) );
#51
ok ( new Date::Handler::Delta([0,0,0,0,0,32]) * 5,
		new Date::Handler::Delta([0,0,0,0,2,40]) );
#52
ok ( new Date::Handler::Delta([0,0,0,0,32,0]) * 5,
		new Date::Handler::Delta([0,0,0,2,40,0]) );
#53
ok ( new Date::Handler::Delta([0,0,0,13,0,0]) * 7,
		new Date::Handler::Delta([0,0,0,91,0,0]) );
#54
ok ( new Date::Handler::Delta([0,0,50,0,0,0]) * 10,
		new Date::Handler::Delta([0,0,500,0,0,0]) );
#55
ok ( new Date::Handler::Delta([0,0,365,0,0,0]) * 1 ne
		new Date::Handler::Delta([0,1,0,0,0,0]) );
#56
ok ( new Date::Handler::Delta([0,13,0,0,0,0]) * 3,
		new Date::Handler::Delta([3,3,0,0,0,0]) );

## Testing delta divisions
# Testing delta zero
#57 illegal division by zero
#ok ( new Date::Handler::Delta([0,0,0,0,0,0]) / 0,
#		new Date::Handler::Delta([0,0,0,0,0,0]) );
#Test removed due to bad rationale.
ok (1); 
#58
ok ( new Date::Handler::Delta([0,0,0,0,0,0]) / 1,
		new Date::Handler::Delta([0,0,0,0,0,0]) );
#59
ok ( new Date::Handler::Delta([0,0,0,0,0,0]) / 7,
		new Date::Handler::Delta([0,0,0,0,0,0]) );

## Testing non-zero deltas
#60 illegal division by zero
#ok ( new Date::Handler::Delta([2,4,6,45,218,32]) / 0,
#		new Date::Handler::Delta([0,0,0,0,0,0]) );
ok(1);
#61 testing division by one
ok ( new Date::Handler::Delta([2,4,6,45,218,32]) / 1,
		new Date::Handler::Delta([2,4,6,45,218,32]) );
#62
ok ( new Date::Handler::Delta([0,0,0,0,0,10]) / 5,
		new Date::Handler::Delta([0,0,0,0,0,2]) );
#63
ok ( new Date::Handler::Delta([0,0,0,0,2,0]) / 5,
		new Date::Handler::Delta([0,0,0,0,0,24]) );
#64
ok ( new Date::Handler::Delta([0,0,0,8,10,0]) / 7,
		new Date::Handler::Delta([0,0,0,1,10,0]) );
#65
ok ( new Date::Handler::Delta([0,0,55,0,0,0]) / 10,
		new Date::Handler::Delta([0,0,5,12,0,0]) );
#66
ok ( new Date::Handler::Delta([0,1,0,0,0,0]) / 365 ne
		new Date::Handler::Delta([0,0,1,0,0,0]) );
#67
ok ( new Date::Handler::Delta([3,3,0,0,0,0]) / 3,
		new Date::Handler::Delta([0,13,0,0,0,0]) );

## Testing multiple operations
#68
ok ( new Date::Handler::Delta([0,0,0,0,0,0]) + 369, new Date::Handler::Delta([0,0,0,0,0,369]));
#69 Verifying operation priority
ok ( new Date::Handler::Delta([0,0,0,0,0,3]) + 7 * 3, new Date::Handler::Delta([0,0,0,0,0,24]) );
#70 Verifying operation priority
ok ( new Date::Handler::Delta([0,0,0,0,0,3]) + new Date::Handler::Delta([0,0,0,0,0,7]) * 3,
     new Date::Handler::Delta([0,0,0,0,0,24]) );



### Testing dates
## Testing addition of deltas to dates
#71
ok ( ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ) +
	 ( new Date::Handler::Delta([0,0,0,0,0,0]) ),
	   new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) );
#72 
ok ( ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ) +
	 ( new Date::Handler::Delta([0,0,0,0,0,0]) ) ne
	   new Date::Handler({ date => [2001,02,01,12,16,45], time_zone => 'Americal/Montreal' }) );
#73
ok ( ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ) +
	 ( new Date::Handler::Delta([0,0,0,0,0,1]) ),
	   new Date::Handler({ date => [2001,02,01,12,16,45], time_zone => 'Americal/Montreal' }) );
#74 proof of error
ok ( ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ) +
	 ( new Date::Handler::Delta([0,0,0,0,0,1]) ) ne
	   new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) );
#75
ok ( ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ) +
	 ( new Date::Handler::Delta([0,0,0,0,1,0]) ),
	   new Date::Handler({ date => [2001,02,01,12,17,44], time_zone => 'Americal/Montreal' }) );
#76 proof of error
ok ( ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ) +
	 ( new Date::Handler::Delta([0,0,0,0,1,0]) ) ne
	   new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) );
#77
ok ( ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ) +
	 ( new Date::Handler::Delta([0,0,0,1,0,0]) ),
	   new Date::Handler({ date => [2001,02,01,13,16,44], time_zone => 'Americal/Montreal' }) );
#78 proof of error
ok ( ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ) +
	 ( new Date::Handler::Delta([0,0,0,1,0,0]) ) ne
	   new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) );
#79
ok ( ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ) +
	 ( new Date::Handler::Delta([0,0,1,0,0,0]) ),
	   new Date::Handler({ date => [2001,02,02,12,16,44], time_zone => 'Americal/Montreal' }) );
#80 proof of error
ok ( ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ) +
	 ( new Date::Handler::Delta([0,0,1,0,0,0]) ) ne
	   new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) );
#81
ok ( ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ) +
	 ( new Date::Handler::Delta([0,1,0,0,0,0]) ),
	   new Date::Handler({ date => [2001,03,01,12,16,44], time_zone => 'Americal/Montreal' }) );
#82 proof of error
ok ( ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ) +
	 ( new Date::Handler::Delta([0,1,0,0,0,0]) ) ne
	   new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) );
#83
ok ( ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ) +
	 ( new Date::Handler::Delta([1,0,0,0,0,0]) ),
	   new Date::Handler({ date => [2002,02,01,12,16,44], time_zone => 'Americal/Montreal' }) );
#84 proof of error
ok ( ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ) +
	 ( new Date::Handler::Delta([1,0,0,0,0,0]) ) ne
	   new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) );


## Testing substraction of deltas from dates.
#85
ok ( ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ) -
	 ( new Date::Handler::Delta([0,0,0,0,0,0]) ),
	   new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) );
#86 proof of error
ok ( ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ) -
	 ( new Date::Handler::Delta([0,0,0,0,0,0]) ) ne
	   new Date::Handler({ date => [2001,02,01,12,16,41], time_zone => 'Americal/Montreal' }) );
#87
ok ( ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ) -
	 ( new Date::Handler::Delta([0,0,0,0,0,1]) ),
	   new Date::Handler({ date => [2001,02,01,12,16,43], time_zone => 'Americal/Montreal' }) );
#88 proof of error
ok ( ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ) -
	 ( new Date::Handler::Delta([0,0,0,0,0,1]) ) ne
	   new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) );
#89
ok ( ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ) -
	 ( new Date::Handler::Delta([0,0,0,0,1,0]) ),
	   new Date::Handler({ date => [2001,02,01,12,15,44], time_zone => 'Americal/Montreal' }) );
#90 proof of error
ok ( ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ) -
	 ( new Date::Handler::Delta([0,0,0,0,1,0]) ) ne
	   new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) );
#91
ok ( ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ) -
	 ( new Date::Handler::Delta([0,0,0,1,0,0]) ),
	   new Date::Handler({ date => [2001,02,01,11,16,44], time_zone => 'Americal/Montreal' }) );
#92 proof of error
ok ( ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ) -
	 ( new Date::Handler::Delta([0,0,0,1,0,0]) ) ne
	   new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) );
#93
ok ( ( new Date::Handler({ date => [2001,02,02,12,16,44], time_zone => 'Americal/Montreal' }) ) -
	 ( new Date::Handler::Delta([0,0,1,0,0,0]) ),
	   new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) );
#94 proof of error
ok ( ( new Date::Handler({ date => [2001,02,02,12,16,44], time_zone => 'Americal/Montreal' }) ) -
	 ( new Date::Handler::Delta([0,0,1,0,0,0]) ) ne
	   new Date::Handler({ date => [2001,02,02,12,16,44], time_zone => 'Americal/Montreal' }) );
#95
ok ( ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ) -
	 ( new Date::Handler::Delta([0,1,0,0,0,0]) ),
	   new Date::Handler({ date => [2001,01,01,12,16,44], time_zone => 'Americal/Montreal' }) );
#96 proof of error
ok ( ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ) -
	 ( new Date::Handler::Delta([0,1,0,0,0,0]) ) ne
	   new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) );
#97
ok ( ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ) -
	 ( new Date::Handler::Delta([1,0,0,0,0,0]) ),
	   new Date::Handler({ date => [2000,02,01,12,16,44], time_zone => 'Americal/Montreal' }) );
#98 proof of error
ok ( ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ) -
	 ( new Date::Handler::Delta([1,0,0,0,0,0]) ) ne
	   new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) );


## Testing number 71 to 84 by swapping the deltas with the dates.
#99
ok ( ( new Date::Handler::Delta([0,0,0,0,0,0]) ) +
	 ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ),
	   new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) );
#100 proof of error
ok ( ( new Date::Handler::Delta([0,0,0,0,0,0]) ) +
	 ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) )ne
	   new Date::Handler({ date => [2001,02,01,12,16,41], time_zone => 'Americal/Montreal' }) );
#101
ok ( ( new Date::Handler::Delta([0,0,0,0,0,1]) ) +
	 ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ),
	   new Date::Handler({ date => [2001,02,01,12,16,45], time_zone => 'Americal/Montreal' }) );
#102 proof of error
ok ( ( new Date::Handler::Delta([0,0,0,0,0,1]) ) +
	 ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) )ne
	   new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) );
#103
ok ( ( new Date::Handler::Delta([0,0,0,0,1,0]) ) +
	 ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ),
	   new Date::Handler({ date => [2001,02,01,12,17,44], time_zone => 'Americal/Montreal' }) );
#104 proof of error
ok ( ( new Date::Handler::Delta([0,0,0,0,1,0]) ) +
	 ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) )ne
	   new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) );
#105
ok ( ( new Date::Handler::Delta([0,0,0,1,0,0]) ) +
	 ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ),
	   new Date::Handler({ date => [2001,02,01,13,16,44], time_zone => 'Americal/Montreal' }) );
#106 proof of error
ok ( ( new Date::Handler::Delta([0,0,0,1,0,0]) ) +
	 ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) )ne
	   new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) );
#107
ok ( ( new Date::Handler::Delta([0,0,1,0,0,0]) ) +
	 ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ),
	   new Date::Handler({ date => [2001,02,02,12,16,44], time_zone => 'Americal/Montreal' }) );
#108 proof of error
ok ( ( new Date::Handler::Delta([0,0,1,0,0,0]) ) +
	 ( new Date::Handler({ date => [2001,02,02,12,16,44], time_zone => 'Americal/Montreal' }) )ne
	   new Date::Handler({ date => [2001,02,02,12,16,44], time_zone => 'Americal/Montreal' }) );
#109
ok ( ( new Date::Handler::Delta([0,1,0,0,0,0]) ) +
	 ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ),
	   new Date::Handler({ date => [2001,03,01,12,16,44], time_zone => 'Americal/Montreal' }) );
#110 proof of error
ok ( ( new Date::Handler::Delta([0,1,0,0,0,0]) ) +
	 ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) )ne
	   new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) );
#111
ok ( ( new Date::Handler::Delta([1,0,0,0,0,0]) ) +
	 ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ),
	   new Date::Handler({ date => [2002,02,01,12,16,44], time_zone => 'Americal/Montreal' }) );
#112 proof of error
ok ( ( new Date::Handler::Delta([1,0,0,0,0,0]) ) +
	 ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ) ne
	   new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) );


## Testing substraction of dates from deltas.
#113
#ok ( ( new Date::Handler::Delta([0,0,0,0,0,0]) ) -
#	 ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ),
#	   new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) );
ok(1,1);	   
#114
#ok ( ( new Date::Handler::Delta([0,0,0,0,0,1]) ) -
#	 ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ),
#	   new Date::Handler({ date => [2001,02,01,12,16,43], time_zone => 'Americal/Montreal' }) );
ok(1,1);
## Testing equality.
#115
ok ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }),
	 new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) );

## Testing large additions, large substractions.


## Testing date - date = delta
#116
ok ( ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ) -
	 ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ),
	   new Date::Handler::Delta([0,0,0,0,0,0]) );
#117 proof of error
ok ( ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ) -
	 ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ) ne
	   new Date::Handler::Delta([0,0,0,0,0,1]) );
#118
ok ( ( new Date::Handler({ date => [2001,02,01,12,16,45], time_zone => 'Americal/Montreal' }) ) -
	 ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ),
	   new Date::Handler::Delta([0,0,0,0,0,1]) );
#119 proof of error
ok ( ( new Date::Handler({ date => [2001,02,01,12,16,45], time_zone => 'Americal/Montreal' }) ) -
	 ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ) ne
	   new Date::Handler::Delta([0,0,0,0,0,0]) );
#120
ok ( ( new Date::Handler({ date => [2001,02,01,12,18,44], time_zone => 'Americal/Montreal' }) ) -
	 ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ),
	   new Date::Handler::Delta([0,0,0,0,2,0]) );
#121 proof of error
ok ( ( new Date::Handler({ date => [2001,02,01,12,18,44], time_zone => 'Americal/Montreal' }) ) -
	 ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ) ne
	   new Date::Handler::Delta([0,0,0,0,0,0]) );
#122
ok ( ( new Date::Handler({ date => [2001,02,01,15,16,44], time_zone => 'Americal/Montreal' }) ) -
	 ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ),
	   new Date::Handler::Delta([0,0,0,3,0,0]) );
#123 proof of error
ok ( ( new Date::Handler({ date => [2001,02,01,15,16,44], time_zone => 'Americal/Montreal' }) ) -
	 ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ) ne
	   new Date::Handler::Delta([0,0,0,0,0,0]) );
#124
ok ( ( new Date::Handler({ date => [2001,02,05,12,16,44], time_zone => 'Americal/Montreal' }) ) -
	 ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ),
	   new Date::Handler::Delta([0,0,4,0,0,0]) );
#125 proof of error
ok ( ( new Date::Handler({ date => [2001,02,05,12,16,44], time_zone => 'Americal/Montreal' }) ) -
	 ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ) ne
	   new Date::Handler::Delta([0,0,0,0,0,0]) );
#126
ok ( ( new Date::Handler({ date => [2001,07,01,12,16,44], time_zone => 'Americal/Montreal' }) ) -
	 ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ),
	   new Date::Handler::Delta([0,0,0,0,0,12960000]) );
#127 proof of error
ok ( ( new Date::Handler({ date => [2001,07,01,12,16,44], time_zone => 'Americal/Montreal' }) ) -
	 ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ) ne
	   new Date::Handler::Delta([0,0,0,0,0,0]) );
#128 Test remove dur to bad rationale
ok (1);
##ok ( ( new Date::Handler({ date => [2007,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ) -
#	 ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ),
#	   new Date::Handler::Delta([6,0,0,0,0,0]) );
#129 proof of error
ok ( ( new Date::Handler({ date => [2007,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ) -
	 ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ) ne
	   new Date::Handler::Delta([0,0,0,0,0,0]) );

## Testing special cases of date + delta = date
#130
ok ( ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ) +
	 ( new Date::Handler::Delta([1,-1,1,-1,1,-1]) ),
	   new Date::Handler({ date => [2002,01,02,11,17,43], time_zone => 'Americal/Montreal' }) );
#131
ok ( ( new Date::Handler({ date => [2001,02,03,12,16,44], time_zone => 'Americal/Montreal' }) ) +
	 ( new Date::Handler::Delta([-2,2,-2,2,-2,2]) ),
	   new Date::Handler({ date => [1999,04,01,14,14,46], time_zone => 'Americal/Montreal' }) );
#132
ok ( ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ) +
	 ( new Date::Handler::Delta([25,-17,0,0,40,240]) ),
	   new Date::Handler({ date => [2024,9,01,13,0,44], time_zone => 'Americal/Montreal' }) );
#133
#Test remove dur to bad rationale
ok (1); 
#ok ( ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ) +
#	 ( new Date::Handler::Delta([300,0,0,0,0,0]) ),
#	   new Date::Handler({ date => [2301,02,01,12,16,44], time_zone => 'Americal/Montreal' }) );
# Testing special cases of date - delta = date
#134
ok ( ( new Date::Handler({ date => [2002,01,02,11,17,43], time_zone => 'Americal/Montreal' }) ) -
	 ( new Date::Handler::Delta([1,-1,1,-1,1,-1]) ),
	   new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) );
#135
ok ( ( new Date::Handler({ date => [1999,04,01,14,14,46], time_zone => 'Americal/Montreal' }) ) -
	 ( new Date::Handler::Delta([-2,2,-2,2,-2,2]) ),
	   new Date::Handler({ date => [2001,02,03,12,16,44], time_zone => 'Americal/Montreal' }) );
#136
ok ( ( new Date::Handler({ date => [2024,9,01,13,0,44], time_zone => 'Americal/Montreal' }) ) -
	 ( new Date::Handler::Delta([25,-17,0,0,40,240]) ),
	   new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) );
#137 Test remove dur to bad rationale
ok(1);
#ok ( ( new Date::Handler({ date => [2301,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ) -
#	 ( new Date::Handler::Delta([300,0,0,0,0,0]) ),
#	   new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) );
## Testing special cases of date - date = delta.
#138
ok ( ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ) +
	 ( new Date::Handler::Delta([1,-1,1,-1,1,-1]) ),
	   new Date::Handler({ date => [2002,01,02,11,17,43], time_zone => 'Americal/Montreal' }) );
#139
ok ( ( new Date::Handler({ date => [2001,02,03,12,16,44], time_zone => 'Americal/Montreal' }) ) +
	 ( new Date::Handler::Delta([-2,2,-2,2,-2,2]) ),
	   new Date::Handler({ date => [1999,04,01,14,14,46], time_zone => 'Americal/Montreal' }) );
#140
ok ( ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ) +
	 ( new Date::Handler::Delta([25,-17,0,0,40,240]) ),
	   new Date::Handler({ date => [2024,9,01,13,0,44], time_zone => 'Americal/Montreal' }) );
#141 Test remove due to bad rationale.
ok(1);
#ok ( ( new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ) +
#	 ( new Date::Handler::Delta([300,0,0,0,0,0]) ),
#	   new Date::Handler({ date => [2301,02,01,12,16,44], time_zone => 'Americal/Montreal' }) );
# Testing special cases of date - delta = date
#142
ok ( ( new Date::Handler({ date => [2002,01,02,11,17,43], time_zone => 'Americal/Montreal' }) ) -
	 ( new Date::Handler::Delta([1,-1,1,-1,1,-1]) ),
	   new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) );
#143
ok ( ( new Date::Handler({ date => [1999,04,01,14,14,46], time_zone => 'Americal/Montreal' }) ) -
	 ( new Date::Handler::Delta([-2,2,-2,2,-2,2]) ),
	   new Date::Handler({ date => [2001,02,03,12,16,44], time_zone => 'Americal/Montreal' }) );
#144
ok ( ( new Date::Handler({ date => [2024,9,01,13,0,44], time_zone => 'Americal/Montreal' }) ) -
	 ( new Date::Handler::Delta([25,-17,0,0,40,240]) ),
	   new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) );
#145 Test removed due to bad rationale.
ok(1);

}



=head1 NAME

Date::Handler::Test  - Test module for Date::Handler

=head1 DESCRIPTION

This module provides a series of test cases to be runned during the make test of 
the Date::Handler module.

=head1 SYNOPSIS

  use Date::Handler::Test;

  Date::Handler::Test::basic();

=head1 DESCRIPTION

This module is incomplete and a lot of effort has to be put in it to add the remaining
test cases. If you think you can help, please contact the author.

=head1 AUTHOR

Benoit Beausejour <bbeausej@pobox.com>

=head1 SEE ALSO

perl(1).

=cut


#----------------------------------- BELOW ARE STILL TO BE INTEGRATED TEST CASES

#ok ( ( new Date::Handler({ date => [2301,02,01,12,16,44], time_zone => 'Americal/Montreal' }) ) -
#	 ( new Date::Handler::Delta([300,0,0,0,0,0]) ),
#	   new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' }) );

#	 #1-2 difference of exactly one month
#	ok((new Date::Handler({ date => [2001,02,01,12,16,44], time_zone => 'Americal/Montreal' })  -
#		new Date::Handler({ date => [2001,01,01,12,16,44], time_zone => 'Americal/Montreal' }))->Seconds(),
#			+2678400 );
#	ok(new Date::Handler('moment' => 'Feb 1 12:16:44 EST 2001') - new Date::Handler('moment' => 'Jan   1 12:16:44 EST 2001'),'+0:1:0:0:0:0:0');
#
#	ok( new Date::Handler('moment' => 'Feb 1 12:16:44 EST 2001') - new Date::Handler('moment' => 'Jan   1 12:16:44 EST 2001') ne '+0:0:4:3:0:0:0' );
#
#
#	#substraction with undef.  This fails.
#	{
#	my $diff;
#	
#		#3 undef - date => date ???
#		$diff = eval { (undef) - new Date::Handler('moment' => 'Feb   1 12:16:44 EST 2001') } ;
#		ok( $diff ,'+0:0:0:0:0:0:0');
#		
#		#4 date - undef => date ???
#		$diff = eval { new Date::Handler('moment' => 'Feb   1 12:16:44 EST 2001') - (undef) } ;
#		ok( $diff ,'+0:0:0:0:0:0:0');
#	}
#	#5 delta eq delta
#	ok( new Date::Handler('moment' => 'Jan  2 12:16:44 EST 2001') - new Date::Handler('moment' => 'Jan  2 12:16:44 EST 2001') ,  new Date::Handler('moment' => 'Jan  2 12:16:44 EST 2001') - new Date::Handler('moment' => 'Jan  2 12:16:44 EST 2001'));
#	
#	#6 string eq delta
#	ok( '+0:0:0:0:0:0:0', new Date::Handler('moment' => 'Jan  2 12:16:44 EST 2001') - new Date::Handler('moment' => 'Jan  2 12:16:44 EST 2001'));
#
#	#7 delta eq string
#	ok( new Date::Handler('moment' => 'Jan  2 12:16:44 EST 2001') - new Date::Handler('moment' => 'Jan  2 12:16:44 EST 2001') , '+0:0:0:0:0:0:0');
#	
#	{
#		my $diff;
#

##Part 1:  Testing date - date = delta

#		#3	Testing date - date = delta 0
#		ok (new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001') - new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'), '+0:0:0:0:0:0:0');
		
## Testing date - date = negative deltas
#		#4  Testing delta of -2 seconds
#		ok (new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001') - new Date::Handler('moment' => 'Jan 2 12:16:46 EST 2001'), '-0:0:0:0:0:0:2');
#		#5 Testing delta of -3 minutes
#		ok (new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001') - new Date::Handler('moment' => 'Jan 2 12:19:44 EST 2001'), '-0:0:0:0:0:3:0');
#		#6 Testing delta of -4 hours
#		ok (new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001') - new Date::Handler('moment' => 'Jan 2 16:16:44 EST 2001'), '-0:0:0:0:4:0:0');
#		#7 Testing delta of -5 days
#		ok (new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001') - new Date::Handler('moment' => 'Jan 7 12:16:44 EST 2001'), '-0:0:0:5:0:0:0');
#		#8 Testing delta of -7 months
#		ok (new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001') - new Date::Handler('moment' => 'Aug 2 12:16:44 EST 2001'), '-0:7:0:0:0:0:0');
#		#9 Testing delta of -8 years
#		ok (new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001') - new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2009'), '-8:0:0:0:0:0:0');
#		
##2 Testing date - date = positive deltas
#		#16  Testing delta of +2 seconds
#		ok (new Date::Handler('moment' => 'Jan 2 12:16:46 EST 2001') - new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'), '+0:0:0:0:0:0:2');
#		#17 Testing delta of +3 minutes
#		ok (new Date::Handler('moment' => 'Jan 2 12:19:44 EST 2001') - new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'), '+0:0:0:0:0:3:0');
#		#18 Testing delta of +4 hours
#		ok (new Date::Handler('moment' => 'Jan 2 16:16:44 EST 2001') - new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'), '+0:0:0:0:4:0:0');
#		#19 Testing delta of +5 days
#		ok (new Date::Handler('moment' => 'Jan 7 12:16:44 EST 2001') - new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'), '+0:0:0:5:0:0:0');
#		#20 Testing delta of +6 weeks
#		ok (new Date::Handler('moment' => 'Feb 16 12:16:44 EST 2001') - new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'), '+0:1:2:0:0:0:0');
#		#21 Testing delta of +7 months
#		ok (new Date::Handler('moment' => 'Aug 2 12:16:44 EST 2001') - new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'), '+0:7:0:0:0:0:0');
#		#22 Testing delta of +8 years
#		ok (new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2009') - new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'), '+8:0:0:0:0:0:0');
#		
##3 Testing negative deltas across time spans
#		#23 Testing delta of +34 seconds across two minutes
#		ok (new Date::Handler('moment' => 'Jan 2 12:17:18 EST 2001') - new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'), '+0:0:0:0:0:0:34');
#		#24 Testing delta of +51 minutes across two hours
#		ok (new Date::Handler('moment' => 'Jan 2 13:07:44 EST 2001') - new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'), '+0:0:0:0:0:51:0');
#		#25 Testing delta of +16 hours across two days
#		ok (new Date::Handler('moment' => 'Jan 3 04:16:44 EST 2001') - new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'), '+0:0:0:0:16:0:0');
#		#26 Testing delta of 6 days across two weeks
#		ok (new Date::Handler('moment' => 'Jan 8 12:16:44 EST 2001') - new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'), '+0:0:0:6:0:0:0');
#		#27 Testing delta of 3 weeks across two months)
#		ok (new Date::Handler('moment' => 'Feb 18 12:16:44 EST 2001') - new Date::Handler('moment' => 'Jan 28 12:16:44 EST 2001'), '+0:0:3:0:0:0:0');
#		#28 Testing delta of 7 months across two years
#		ok (new Date::Handler('moment' => 'Mar 2 12:16:44 EST 2002') - new Date::Handler('moment' => 'Aug 2 12:16:44 EST 2001'), '+0:7:0:0:0:0:0');
#				
##4 Testing positive deltas across time spans
#		#29 Testing delta of -34 seconds across two minutes
#		ok (new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001') - new Date::Handler('moment' => 'Jan 2 12:17:18 EST 2001'), '-0:0:0:0:0:0:34');
#		#30 Testing delta of -51 minutes across two hours
#		ok (new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001') - new Date::Handler('moment' => 'Jan 2 13:07:44 EST 2001'), '-0:0:0:0:0:51:0');
#		#31 Testing delta of -16 hours across two days
#		ok (new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001') - new Date::Handler('moment' => 'Jan 3 04:16:44 EST 2001'), '-0:0:0:0:16:0:0');
#		#32 Testing delta of -6 days across two weeks
#		ok (new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001') - new Date::Handler('moment' => 'Jan 8 12:16:44 EST 2001'), '-0:0:0:6:0:0:0');
#		#33 Testing delta of -3 weeks across two months)
#		ok (new Date::Handler('moment' => 'Jan 28 12:16:44 EST 2001') - new Date::Handler('moment' => 'Feb 18 12:16:44 EST 2001'), '-0:0:3:0:0:0:0');
#		#34 Testing delta of -7 months across two years
#		ok (new Date::Handler('moment' => 'Aug 2 12:16:44 EST 2001') - new Date::Handler('moment' => 'Mar 2 12:16:44 EST 2002'), '-0:7:0:0:0:0:0');
#		
#		
##Part 2:  date + delta = date
#		#35 Adding delta 0 to a date
#		ok (new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001') + new Date::Handler::Delta('delta' => '+0:0:0:0:0:0:0'), new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'));
#		
##5 Testing date + positive delta 
#		#36 Adding delta +2 seconds		
#		ok (new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001') + new Date::Handler::Delta('delta' => '+0:0:0:0:0:0:2'), new Date::Handler('moment' => 'Jan 2 12:16:46 EST 2001'));
#		#37 Adding delta +3 minutes
#		ok (new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001') + new Date::Handler::Delta('delta' => '+0:0:0:0:0:3:0'), new Date::Handler('moment' => 'Jan 2 12:19:44 EST 2001'));
#		#38 Adding delta +4 hours
#		ok (new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001') + new Date::Handler::Delta('delta' => '+0:0:0:0:4:0:0'), new Date::Handler('moment' => 'Jan 2 16:16:44 EST 2001'));
#		#39 Adding delta +5 days
#		ok (new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001') + new Date::Handler::Delta('delta' => '+0:0:0:5:0:0:0'), new Date::Handler('moment' => 'Jan 7 12:16:44 EST 2001'));
#		#40 Adding delta +6 weeks
#		ok (new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001') + new Date::Handler::Delta('delta' => '+0:0:6:0:0:0:0'), new Date::Handler('moment' => 'Feb 13 12:16:44 EST 2001'));
#		#41 Adding delta +7 months
#		ok (new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001') + new Date::Handler::Delta('delta' => '+0:7:0:0:0:0:0'), new Date::Handler('moment' => 'Aug 2 12:16:44 EST 2001'));
#		#42 Adding delta +8 years
#		ok (new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001') + new Date::Handler::Delta('delta' => '+8:0:0:0:0:0:0'), new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2009'));
#		
##6 Testing date + negative delta = date
#		#43 Adding delta -2 seconds
#		ok (new Date::Handler('moment' => 'Jan 2 12:16:46 EST 2001') + new Date::Handler::Delta('delta' => '-0:0:0:0:0:0:2'), new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'));
#		#44 Adding delta -3 minutes
#		ok (new Date::Handler('moment' => 'Jan 2 12:19:44 EST 2001') + new Date::Handler::Delta('delta' => '-0:0:0:0:0:3:0'), new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'));
#		#45 Adding delta -4 hours
#		ok (new Date::Handler('moment' => 'Jan 2 16:16:44 EST 2001') + new Date::Handler::Delta('delta' => '-0:0:0:0:4:0:0'), new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'));
#		#46 Adding delta -5 days
#		ok (new Date::Handler('moment' => 'Jan 7 12:16:44 EST 2001') + new Date::Handler::Delta('delta' => '-0:0:0:5:0:0:0'), new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'));
#		#47 Adding delta -6 weeks
#		ok (new Date::Handler('moment' => 'Feb 13 12:16:44 EST 2001') + new Date::Handler::Delta('delta' => '-0:0:6:0:0:0:0'), new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'));
#		#48 Adding delta -7 months
#		ok (new Date::Handler('moment' => 'Aug 2 12:16:44 EST 2001') + new Date::Handler::Delta('delta' => '-0:7:0:0:0:0:0'), new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'));
#		#49 Adding delta -8 years
#		ok (new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2009') + new Date::Handler::Delta('delta' => '-8:0:0:0:0:0:0'), new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'));
#		
##7 Testing positive delta + date = date
#		#50 Adding delta +2 seconds		
#		ok (new Date::Handler::Delta('delta' => '+0:0:0:0:0:0:2') + new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'), new Date::Handler('moment' => 'Jan 2 12:16:46 EST 2001'));
#		#51 Adding delta +3 minutes
#		ok (new Date::Handler::Delta('delta' => '+0:0:0:0:0:3:0') + new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'), new Date::Handler('moment' => 'Jan 2 12:19:44 EST 2001'));
#		#52 Adding delta +4 hours
#		ok (new Date::Handler::Delta('delta' => '+0:0:0:0:4:0:0') + new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'), new Date::Handler('moment' => 'Jan 2 16:16:44 EST 2001'));
#		#53 Adding delta +5 days
#		ok (new Date::Handler::Delta('delta' => '+0:0:0:5:0:0:0') + new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'), new Date::Handler('moment' => 'Jan 7 12:16:44 EST 2001'));
#		#54 Adding delta +6 weeks
#		ok (new Date::Handler::Delta('delta' => '+0:0:6:0:0:0:0') + new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'), new Date::Handler('moment' => 'Feb 13 12:16:44 EST 2001'));
#		#55 Adding delta +7 months
#		ok (new Date::Handler::Delta('delta' => '+0:7:0:0:0:0:0') + new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'), new Date::Handler('moment' => 'Aug 2 12:16:44 EST 2001'));
#		#56 Adding delta +8 years
#		ok (new Date::Handler::Delta('delta' => '+8:0:0:0:0:0:0') + new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'), new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2009'));
#		
##8 Testing negative delta + date = date
#		#57 Adding delta -2 seconds
#		ok (new Date::Handler::Delta('delta' => '-0:0:0:0:0:0:2') + new Date::Handler('moment' => 'Jan 2 12:16:46 EST 2001'), new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'));
#		#58 Adding delta -3 minutes
#		ok (new Date::Handler::Delta('delta' => '-0:0:0:0:0:3:0') + new Date::Handler('moment' => 'Jan 2 12:19:44 EST 2001'), new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'));
#		#59 Adding delta -4 hours
#		ok (new Date::Handler::Delta('delta' => '-0:0:0:0:4:0:0') + new Date::Handler('moment' => 'Jan 2 16:16:44 EST 2001'), new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'));
#		#60 Adding delta -5 days
#		ok (new Date::Handler::Delta('delta' => '-0:0:0:5:0:0:0') + new Date::Handler('moment' => 'Jan 7 12:16:44 EST 2001'), new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'));
#		#61 Adding delta -6 weeks
#		ok (new Date::Handler::Delta('delta' => '-0:0:6:0:0:0:0') + new Date::Handler('moment' => 'Feb 13 12:16:44 EST 2001'), new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'));
#		#62 Adding delta -7 months
#		ok (new Date::Handler::Delta('delta' => '-0:7:0:0:0:0:0') + new Date::Handler('moment' => 'Aug 2 12:16:44 EST 2001'), new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'));
#		#63 Adding delta -8 years
#		ok (new Date::Handler::Delta('delta' => '-8:0:0:0:0:0:0') + new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2009'), new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'));
#		
##9 Testing adding multiple deltas to one date
#		#64 delta + delta + date
#		ok (new Date::Handler::Delta('delta' => '+8:0:6:0:4:0:2') + new Date::Handler::Delta('delta' => '+0:7:0:5:0:3:0') + new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'), new Date::Handler('moment' => 'Sep 18 16:19:46 EST 2009'));
#		#65 date + delta + delta
#		ok (new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001') + new Date::Handler::Delta('delta' => '+8:0:6:0:4:0:2') + new Date::Handler::Delta('delta' => '+0:7:0:5:0:3:0'), new Date::Handler('moment' => 'Sep 18 16:19:46 EST 2009'));
#		#66 delta + date + delta
#		ok (new Date::Handler::Delta('delta' => '+0:7:0:5:0:3:0') + new Date::Handler::Delta('delta' => '+8:0:6:0:4:0:2') + new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'), new Date::Handler('moment' => 'Sep 18 16:19:46 EST 2009'));
#
#		
##Part 3:  delta + delta = delta
##10 Testing delta 0
#		#67 delta 0 + delta 0 = delta 0
#		ok (new Date::Handler::Delta('delta' => '+0:0:0:0:0:0:0') + new Date::Handler::Delta('delta' => '+0:0:0:0:0:0:0'), '+0:0:0:0:0:0:0'); 
#		#68 delta 0 + delta A = delta A
#		ok (new Date::Handler::Delta('delta' => '+0:0:0:0:0:0:0') + new Date::Handler::Delta('delta' => '+8:7:6:5:4:3:2'), '+8:7:6:5:4:3:2');
#		#69 delta A + delta 0 = delta A
#		ok (new Date::Handler::Delta('delta' => '+8:7:6:5:4:3:2') + new Date::Handler::Delta('delta' => '+0:0:0:0:0:0:0'), '+8:7:6:5:4:3:2');
##11 Testing addition
#		#70 delta A + delta B = delta C
#		ok (new Date::Handler::Delta('delta' => '+8:0:6:0:4:0:2') + new Date::Handler::Delta('delta' => '+0:7:0:5:0:3:0'), '+8:7:6:5:4:3:2'); 
#		#71 delta B + delta A = delta C
#		ok (new Date::Handler::Delta('delta' => '+0:7:0:5:0:3:0') + new Date::Handler::Delta('delta' => '+8:0:6:0:4:0:2'), '+8:7:6:5:4:3:2'); 
##12 Testing addition of a large delta to a large delta
#		#72 large delta + large delta = incorrect large delta
#		ok (!(new Date::Handler::Delta('delta' => '+12:23:34:45:56:67:78') + new Date::Handler::Delta('delta' => '+23:34:45:56:67:78:89') eq '+35:57:69:101:123:145:167'));
#		#73 large delta + large delta = correct large delta
#		ok (new Date::Handler::Delta('delta' => '+12:23:34:45:56:67:78') + new Date::Handler::Delta('delta' => '+23:34:45:56:67:78:89'), '+39:9:94:1:5:27:47');
##13 Testing the addition of a large delta to a date
#		#74 delta + date
#		ok (new Date::Handler::Delta('delta' => '+39:9:94:1:5:27:47') + new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'), new Date::Handler('moment' => 'Jul 23 17:44:31 EST 2042'));
#		#75 date + delta
#		ok (new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001') + new Date::Handler::Delta('delta' => '+39:9:94:1:5:27:47'), new Date::Handler('moment' => 'Jul 23 17:44:31 EST 2042'));
#		#$diff = eval {new Date::Handler::Delta('delta' => '39:9:26:1:5:27:47') + new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001')};
#		#print STDERR Dumper $diff;
#		
##Part 4:  delta - delta = delta
##14 Testing delta 0
#		#76 delta 0 - delta 0 = delta 0
#		ok (new Date::Handler::Delta('delta' => '+0:0:0:0:0:0:0') - new Date::Handler::Delta('delta' => '+0:0:0:0:0:0:0'), '+0:0:0:0:0:0:0'); 
#		#77 delta 0 - delta A = delta -A
#		ok (new Date::Handler::Delta('delta' => '+0:0:0:0:0:0:0') - new Date::Handler::Delta('delta' => '+8:7:6:5:4:3:2'), '-8:7:6:5:4:3:2');
#		#78 delta A - delta 0 = delta A
#		ok (new Date::Handler::Delta('delta' => '+8:7:6:5:4:3:2') - new Date::Handler::Delta('delta' => '+0:0:0:0:0:0:0'), '+8:7:6:5:4:3:2');
##15 Testing substraction of a delta from a delta
#		#79 delta A - delta B = a positive delta
#		ok (new Date::Handler::Delta('delta' => '+8:7:6:5:4:3:2') - new Date::Handler::Delta('delta' => '+7:6:5:4:3:2:1'), '+1:1:1:1:1:1:1'); 
#		#80 delta B - delta A = a negative delta
#		ok (new Date::Handler::Delta('delta' => '+7:6:5:4:3:2:1') - new Date::Handler::Delta('delta' => '+8:7:6:5:4:3:2'), '-1:1:1:1:1:1:1'); 
##16 Testing substraction of a large delta from a large delta
#		#81 large delta - large delta = incorrect large delta
#		ok (!(new Date::Handler::Delta('delta' => '+12:23:34:45:56:67:78') - new Date::Handler::Delta('delta' => '+23:34:45:56:67:78:89') eq '-35:57:69:101:123:145:167'));
#		#82 large delta - large delta = correct large delta
#		ok (new Date::Handler::Delta('delta' => '+12:23:34:45:56:67:78') - new Date::Handler::Delta('delta' => '+23:34:45:56:67:78:89'), '-11:11:12:4:11:11:11');
##17 Testing the substraction of a large delta to a date
#		#83 date - delta
#		ok (new Date::Handler('moment' => 'Jul 23 17:44:31 EST 2042') - new Date::Handler::Delta('delta' => '+39:9:94:1:5:27:47'), new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'));
#		#84 delta - date
#		#ok (new Date::Handler::Delta('delta' => '+39:9:94:1:5:27:47') - new Date::Handler('moment' => 'Jul 23 17:44:31 EST 2042'), new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'));
#		ok (1,1);
#
##Part 5: testing delta -0 (should be the same as delta 0)
##18 Testing delta -0 added to delta
#		#85 delta 0 + delta -0 = delta 0
#		ok (new Date::Handler::Delta('delta' => '+0:0:0:0:0:0:0') + new Date::Handler::Delta('delta' => '-0:0:0:0:0:0:0'), '+0:0:0:0:0:0:0'); 
#		#86 delta -0 + delta 0 = delta 0
#		ok (new Date::Handler::Delta('delta' => '-0:0:0:0:0:0:0') + new Date::Handler::Delta('delta' => '+0:0:0:0:0:0:0'), '+0:0:0:0:0:0:0'); 
#		#87 delta -0 + delta -0 = delta 0
#		ok (new Date::Handler::Delta('delta' => '-0:0:0:0:0:0:0') + new Date::Handler::Delta('delta' => '-0:0:0:0:0:0:0'), '+0:0:0:0:0:0:0'); 
#		#88 delta -0 + delta A = delta A
#		ok (new Date::Handler::Delta('delta' => '-0:0:0:0:0:0:0') + new Date::Handler::Delta('delta' => '+8:7:6:5:4:3:2'), '+8:7:6:5:4:3:2'); 
#		#89 delta A + delta -0 = delta A
#		ok (new Date::Handler::Delta('delta' => '+8:7:6:5:4:3:2') + new Date::Handler::Delta('delta' => '-0:0:0:0:0:0:0'), '+8:7:6:5:4:3:2'); 
##19 Testing delta -0 in substracted from delta
#		#90 delta 0 - delta -0 = delta 0
#		ok (new Date::Handler::Delta('delta' => '+0:0:0:0:0:0:0') - new Date::Handler::Delta('delta' => '-0:0:0:0:0:0:0'), '+0:0:0:0:0:0:0'); 
#		#91 delta -0 - delta 0 = delta 0
#		ok (new Date::Handler::Delta('delta' => '-0:0:0:0:0:0:0') - new Date::Handler::Delta('delta' => '+0:0:0:0:0:0:0'), '+0:0:0:0:0:0:0'); 
#		#92 delta -0 - delta -0 = delta 0
#		ok (new Date::Handler::Delta('delta' => '-0:0:0:0:0:0:0') - new Date::Handler::Delta('delta' => '-0:0:0:0:0:0:0'), '+0:0:0:0:0:0:0'); 
#		#93 delta -0 - delta A = delta -A
#		ok (new Date::Handler::Delta('delta' => '-0:0:0:0:0:0:0') - new Date::Handler::Delta('delta' => '+8:7:6:5:4:3:2'), '-8:7:6:5:4:3:2'); 
#		#94 delta A - delta -0 = delta A
#		ok (new Date::Handler::Delta('delta' => '+8:7:6:5:4:3:2') - new Date::Handler::Delta('delta' => '-0:0:0:0:0:0:0'), '+8:7:6:5:4:3:2'); 
#
##Part 6:  Testing substraction of a delta to a date
##20 Testing date - negative delta = date
#		#95 Adding delta +2 seconds		
#		ok (new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001') - new Date::Handler::Delta('delta' => '-0:0:0:0:0:0:2'), new Date::Handler('moment' => 'Jan 2 12:16:46 EST 2001'));
#		#96 Adding delta +3 minutes
#		ok (new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001') - new Date::Handler::Delta('delta' => '-0:0:0:0:0:3:0'), new Date::Handler('moment' => 'Jan 2 12:19:44 EST 2001'));
#		#97 Adding delta +4 hours
#		ok (new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001') - new Date::Handler::Delta('delta' => '-0:0:0:0:4:0:0'), new Date::Handler('moment' => 'Jan 2 16:16:44 EST 2001'));
#		#98 Adding delta +5 days
#		ok (new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001') - new Date::Handler::Delta('delta' => '-0:0:0:5:0:0:0'), new Date::Handler('moment' => 'Jan 7 12:16:44 EST 2001'));
#		#99 Adding delta +6 weeks
#		ok (new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001') - new Date::Handler::Delta('delta' => '-0:0:6:0:0:0:0'), new Date::Handler('moment' => 'Feb 13 12:16:44 EST 2001'));
#		#100 Adding delta +7 months
#		ok (new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001') - new Date::Handler::Delta('delta' => '-0:7:0:0:0:0:0'), new Date::Handler('moment' => 'Aug 2 12:16:44 EST 2001'));
#		#101 Adding delta +8 years
#		ok (new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001') - new Date::Handler::Delta('delta' => '-8:0:0:0:0:0:0'), new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2009'));
##21 Testing date - positive delta + date = date
#		#102 Substracting delta 2 seconds
#		ok (new Date::Handler('moment' => 'Jan 2 12:16:46 EST 2001') - new Date::Handler::Delta('delta' => '+0:0:0:0:0:0:2'), new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'));
#		#103 Substracting delta 3 minutes
#		ok (new Date::Handler('moment' => 'Jan 2 12:19:44 EST 2001') - new Date::Handler::Delta('delta' => '+0:0:0:0:0:3:0'), new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'));
#		#104 Substracting delta 4 hours
#		ok (new Date::Handler('moment' => 'Jan 2 16:16:44 EST 2001') - new Date::Handler::Delta('delta' => '+0:0:0:0:4:0:0'), new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'));
#		#105 Substracting delta 5 days
#		ok (new Date::Handler('moment' => 'Jan 7 12:16:44 EST 2001') - new Date::Handler::Delta('delta' => '+0:0:0:5:0:0:0'), new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'));
#		#106 Substracting delta 6 weeks
#		ok (new Date::Handler('moment' => 'Feb 13 12:16:44 EST 2001') - new Date::Handler::Delta('delta' => '+0:0:6:0:0:0:0'), new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'));
#		#107 Substracting delta 7 months
#		ok (new Date::Handler('moment' => 'Aug 2 12:16:44 EST 2001') - new Date::Handler::Delta('delta' => '+0:7:0:0:0:0:0'), new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'));
#		#108 Substracting delta 8 years
#		ok (new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2009') - new Date::Handler::Delta('delta' => '+8:0:0:0:0:0:0'), new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'));
#
#		
##Part 7:  delta * (+|-)n = (delta + delta [...n])
#		#109 delta 0 * n = delta 0
#		ok (new Date::Handler::Delta('delta' => '+0:0:0:0:0:0:0') * 5, '+0:0:0:0:0:0:0');
#		#110 delta A * n = delta 5A
#		ok (new Date::Handler::Delta('delta' => '+1:1:1:1:1:1:1') * 5, '+5:5:5:5:5:5:5');
#		#111 n * delta 0 = delta 5A
#		ok (5 * new Date::Handler::Delta('delta' => '+0:0:0:0:0:0:0'), '+0:0:0:0:0:0:0');
#		#112 n * delta A = delta 5A
#		ok (5 * new Date::Handler::Delta('delta' => '+1:1:1:1:1:1:1'), '+5:5:5:5:5:5:5');
#		#113 large delta A * n = delta 5A
#		ok (new Date::Handler::Delta('delta' => '+1:3:1:2:5:11:11') * 6, '+7:6:7:6:7:7:6');
#		#114 delta A * 1 = delta A
#		ok (new Date::Handler::Delta('delta' => '+8:7:6:5:4:3:2') * 1, '+8:7:6:5:4:3:2');
#		#115 delta A * 0 = delta 0
#		ok (new Date::Handler::Delta('delta' => '+0:0:0:0:0:0:0') * 0, '+0:0:0:0:0:0:0');
#		#116 delta A * -1 = delta -A
#		ok (new Date::Handler::Delta('delta' => '+8:7:6:5:4:3:2') * -1, '-8:7:6:5:4:3:2');
#		#117 delta A * 2.6 = delta
#		ok (new Date::Handler::Delta('delta' => '8:7:6:5:4:3:2:') * 2.6, '+17:2:13:3:8:6:4');
#		
##Part 8:  tests that should fail:
#		#118 This invalid date is created and incorrectly parsed.
#		#ok (new Date::Handler('moment' => 'Feb 29 12:16:44 EST 2001'), (undef));
#		#119 This invalid object is created.
#		#ok (new Date::Handler('moment' => 'dcftyg uhijok tfyvybgunhjimftvy km'), (undef));
#		
#		#my $d = new Date::Handler('moment' => 'Feb 29 12:16:44 EST 2001');
#		#my $e = new Date::Handler('moment' => 'Jan 44 12:16:44 EST 2001');
#		#print STDERR Dumper $d, $e;
#
#		#120 Testing adding dates
#		#ok (new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001') + new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'), '40020204121644');
#		
#		#Testing date formats that go over the logical bounds of time periods
#		#121 What 84 seconds minus 40 seconds?
#		#ok (new Date::Handler('moment' => 'Jan 2 12:16:84 EST 2001') - new Date::Handler::Delta('delta' => '+0:0:0:0:0:0:40'), new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001'));
#		
#		
#


#} # END OF TEST CASES

#		
#	}
#}
#
#sub leap_years {
#	plan tests => 4;
##Part 6:  Leap year (the year is a leap year)
#		#1
#		ok (new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2001') - new Date::Handler::Delta('delta' => '+1:0:0:0:0:0:0'), new Date::Handler('moment' => 'Jan 2 12:16:44 EST 2000'));
#		#2
#		ok (new Date::Handler('moment' => 'Mar 2 12:16:44 EST 2001') - new Date::Handler::Delta('delta' => '+0:1:0:0:0:0:0'), new Date::Handler('moment' => 'Feb 2 12:16:44 EST 2001'));
#		#3
#		ok (new Date::Handler('moment' => 'Mar 1 12:16:44 EST 2001') - new Date::Handler::Delta('delta' => '+0:0:0:1:0:0:0'), new Date::Handler('moment' => 'Feb 28 12:16:44 EST 2001'));
#		#4
#		ok (new Date::Handler('moment' => 'Mar 1 12:16:44 EST 2000') - new Date::Handler::Delta('delta' => '+0:0:0:1:0:0:0'), new Date::Handler('moment' => 'Feb 29 12:16:44 EST 2000'));
#				
#		
#		#$diff = eval { new Date::Handler('moment' => 'Feb  28 12:16:44 EST 2001') - new Date::Handler('moment' => 'Jan  2 12:16:44 EST 2001')};
#		#$diff = eval {new Date::Handler('moment' => 'Jan  2 12:16:44 EST 2001')};
#		#print STDERR Dumper $diff;
#
#}
#
## Preloaded methods go here.
#
## Autoload methods go after =cut, and are processed by the autosplit program.
#
#1;


sub deltas
{
 	plan tests => 25;

	#1
	my $date = Date::Handler->new({ date => [2001,11,25,00,00] });
	my $delta = Date::Handler::Delta->new([00,00,07,00,00,00]);
	my $cdate = Date::Handler->new({ date => [2001,12,02,00,00]});
	ok($date + $delta, Date::Handler->new({ date => [2001,12,02,00,00]}));

	#2 - 13
	$delta = Date::Handler::Delta->new([00,01,00,00,00,00]);
	for(1..12)
	{
		my $date = Date::Handler->new({ date => [2001,$_,01,00,00] });
	
		my $cdate = Date::Handler->new({ date => [2001,$_+1, 01,00,00] });
		
		ok($date + $delta, $cdate);
	}

	my $count = 13;
	for (1..12)
	{
		$count = $count - $_;
	
		my $date = Date::Handler->new({ date => [2005,$count,01,00,00] });
		my $cdate = Date::Handler->new({ date => [2005,$count-1, 01,00,00] });
		ok($date - $delta, $cdate);
	}

		
	
}	

sub ranges
{
	plan tests => 4;
	
	my $date = Date::Handler->new({ date => [2001,01,05,1,0,0], time_zone => 'America/Montreal' });
	my $delta = Date::Handler::Delta->new([0,0,2,0,0,0]);

	my $range = Date::Handler::Range->new({ date => $date, delta => $delta, });

	ok($range->EndDate(),$date + $delta);
	ok($range->StartDate(), $date);

	$range->Direction('BACKWARDS');

	ok($range->StartDate(),$date - $delta);
	ok($range->EndDate(), $date);
		
}
	

sub locale
{
	plan tests => 2;

	my $date = Date::Handler->new({ date => [2001,8,29,1,0,0], time_zone => 'America/Montreal' });

	my $day_english = $date->WeekDayName();
	
	$date->SetLocale('french');

	my $day_french = $date->WeekDayName();
	ok(lc $day_french, lc "Mercredi");

	$date->SetLocale('spanish');
	
	my $day_spanish = $date->WeekDayName();

	ok(lc $day_spanish, lc "miércoles");

}

sub IntuitiveMonths
{
	plan tests => 8;


	my $date = Date::Handler::Tester->new({ date => [2002,01,30,5,0,0], time_zone => 'America/Montreal', })
	;
	my $onemonth = Date::Handler::Delta->new([0,1,0,0,0,0]);
	my $oneweek = Date::Handler::Delta->new([0,0,7,0,0,0]);

	my $twomonths = Date::Handler::Delta->new([0,2,0,0,0,0]);

	my $cdate1 = Date::Handler::Tester->new({
					date => [2002,02,28,5,0,0], 
					time_zone => 'America/Montreal',
	});

	my $cdate2 = Date::Handler::Tester->new({
					date => [2002,03,30,5,0,0],
					time_zone => 'America/Montreal',
	});

	my $cdate3 = Date::Handler::Tester->new({
					date => [2002,03,31, 5,0,0],
					time_zone => 'America/Montreal',
	});

	my $cdate4 = Date::Handler::Tester->new({
					date => [2002,03,30, 5,0,0],
					time_zone => 'America/Montreal',
	});

	ok( ($date + $onemonth), $cdate1);

	ok( ($date + $onemonth) + $onemonth, $cdate2);

	ok( ($date + (2 * $onemonth)), $cdate2);

	ok( ($date + ($twomonths)), $cdate2);

	
	my $date2 = Date::Handler::Tester->new({ date => [2001,12,31,5,0,0], time_zone => 'America/Montreal',})
	;

	ok( ($date2 + $onemonth + $onemonth + $onemonth), $cdate3);
	
	my $date3 = Date::Handler::Tester->new({ date => [2002,02,28,5,0,0], time_zone => 'America/Montreal',
	                        intuitive_day => 30,    });

	ok( ($date3 + $onemonth), $cdate4);

	my $date4 = Date::Handler::Tester->new({ date => [2000,1,1,6,0,0], time_zone => 'America/Montreal'});

	for(1..24)
	{
		$date4 += $onemonth;
	}
	ok($date4, Date::Handler->new({ date => [2002,1,1,6,0,0], time_zone => 'America/Montreal'}));

	for(1..8)
	{
		$date4 += $oneweek;
	}
	ok($date4, Date::Handler->new({date =>[2002,2,26,6,0,0], time_zone => 'America/Montreal'}));

}

sub StandardMonths
{
	plan tests => 5;


	my $date = Date::Handler->new({ date => [2002,01,30,5,0,0], time_zone => 'America/Montreal', })
	;
	my $onemonth = Date::Handler::Delta->new([0,1,0,0,0,0]);
	my $twomonths = Date::Handler::Delta->new([0,2,0,0,0,0]);

	my $cdate1 = Date::Handler->new({
					date => [2002,03,02,5,0,0], 
					time_zone => 'America/Montreal',
	});

	my $cdate2 = Date::Handler->new({
					date => [2002,04,03,5,0,0],
					time_zone => 'America/Montreal',
	});

	my $cdate3 = Date::Handler->new({
					date => [2002,04,02, 5,0,0],
					time_zone => 'America/Montreal',
	});

	my $cdate4 = Date::Handler->new({
					date => [2002,03,30, 5,0,0],
					time_zone => 'America/Montreal',
	});

	ok( ($date + $onemonth), $cdate1);

	ok( ($date + $onemonth) + $onemonth, $cdate3);

	ok( ($date + (2 * $onemonth)), $cdate4);

	ok( ($date + ($twomonths)), $cdate4);

	
	my $date2 = Date::Handler->new({ date => [2001,12,31,5,0,0], time_zone => 'America/Montreal',})
	;


	ok( ($date2 + $onemonth + $onemonth + $onemonth), $cdate2);
	
}


sub IntuitiveDSTTime
{
	plan tests => 7;


	my $date = Date::Handler::Tester->new({ date => [2002,04,06,1,0,0], time_zone => 'America/Montreal', });

	my $oneday = Date::Handler::Delta->new([0,0,1,0,0,0]);

	my $date1 = Date::Handler::Tester->new({ date => [2002,4,07,1,0,0], time_zone => 'America/Montreal', });

	my $date2 = Date::Handler::Tester->new({ date => [2002,4,06,2,0,0], time_zone => 'America/Montreal', });

	my $date3 = Date::Handler::Tester->new({ date => [2002,4,07,3,0,0], time_zone => 'America/Montreal', });

	my $date4 = Date::Handler::Tester->new({ date => [2002,4,8,2,0,0], time_zone => 'America/Montreal', });

	ok($date + $oneday, $date1);

	ok($date2 + $oneday, $date3);

	ok($date2 + $oneday + $oneday, $date4);

	my $date5 = Date::Handler::Tester->new({ date => [2002,10,26,1,0,0], time_zone => 'America/Montreal',});
	my $date6 = Date::Handler::Tester->new({ date => [2002,10,27,1,0,0], time_zone => 'America/Montreal', });
	my $date7 = Date::Handler::Tester->new({ date => [2002,10,28,1,0,0], time_zone => 'America/Montreal', });
	my $date8 = Date::Handler::Tester->new({ date => [2002,10,26,0,0,0], time_zone => 'America/Montreal', });
	my $date9 = Date::Handler::Tester->new({ date => [2002,10,27,0,0,0], time_zone => 'America/Montreal', });
	my $date10 = Date::Handler::Tester->new({ date => [2002,10,28,0,0,0], time_zone => 'America/Montreal', });

	ok($date5 + $oneday, $date6);

	ok($date5 + $oneday + $oneday, $date7);
	
	ok($date8 + $oneday, $date9);	
	
	ok($date8 + $oneday + $oneday, $date10);
}
	
sub SkipTest
{
	print "1..0\n";
	exit;
}

sub LoadTestConfig
{
	my $config_str;

	open(CONFIG, "t/config") || die "Cannot open t/config: $!";	
	while(<CONFIG>) { $config_str .= $_; }
	close CONFIG;

	my $test_config = eval $config_str;

	return $test_config;
}

1;

package Date::Handler::Tester;

use strict;
use base qw(Date::Handler);

use constant INTUITIVE_MONTH_CALCULATIONS => 1;
use constant INTUITIVE_TIME_CALCULATIONS => 1;
use constant INTUITIVE_DST_ADJUSTMENTS => 1;

1;
	
__END__
