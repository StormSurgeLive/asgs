#!/usr/bin/perl
package Date::Handler::Constants;


use strict;
use vars qw(@ISA $VERSION @EXPORT $DAYS_IN_MONTH $SUPPORTED_TIME_ZONES);

use Exporter;
@ISA = qw(Exporter);

$VERSION = '1.0';

@EXPORT = qw($DAYS_IN_MONTH $SUPPORTED_TIME_ZONES);

$DAYS_IN_MONTH = {
			0 => 31, #Jan
			1 => 28, #Feb
			2 => 31, #Mar
			3 => 30, #Apr
			4 => 31, #May
			5 => 30, #Jun
			6 => 31, #Jul
			7 => 31, #Aug
			8 => 30, #Sep
			9 => 31, #Oct
			10 => 30, #Nov
			11 => 31, #Dec
		};	
			
$SUPPORTED_TIME_ZONES = {};

666; #The number of the beast.
