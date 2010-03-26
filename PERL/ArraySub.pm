#!/usr/bin/perl -w
################################################################################
#                               ArraySub package                               #
################################################################################
# This package contains all the  Arrays related subroutines, for both simple   #
# ones and double-dimensional ones (arrays of references pointing to other     #
# arrays)                                                                      #
################################################################################
#               The subroutines implemented here are the following:            #
################################################################################
##takeUndefAway                                                                #
##takeSpaceAway                                                                #
##printArray                                                                   #
##printArrayIn                                                                 #
##printDoubleArray                                                             #
##pushDoubleArray                                                              #
##giveMaxDoubleArray                                                           #
##giveSumDoubleArray                                                           #
##giveMaxArray                                                                 #
##formatArray                                                                  #
##formatDoubleArray                                                            #
##reverseDoubleArray                                                           #
################################################################################
# no other package is used                                                     #
################################################################################
#written by Eve-Marie Devali�re for WFO-Eureka                                 #
#first written: 02/08/04                                                       #
#last update: 07/30/04                                                         #
################################################################################


######################################################
#               Exportation requirements             #
######################################################

package ArraySub;
require Exporter;
@ISA=qw(Exporter);
@EXPORT=qw(takeUndefAway takeSpaceAway printArray printArrayIn formatArray formatDoubleArray pushDoubleArray printDoubleArrayIn printDoubleArray takeSpaceAway giveMaxArray giveMinArray giveMaxDoubleArray giveSumDoubleArray reverseDoubleArray flipArray inArray find);

######################################################
#                    Subroutines                     #
######################################################

################################################################################
# NAME: &takeUndefAway
# CALL: &takeUndefAway($arrayRef)
# GOAL: Scan all the array and if the value is not a numeric one take them away
################################################################################
sub takeUndefAway
	{ 
	local *array1=shift;
        my $x=0;
        foreach (@array1)
                {
                if (!($_=~m/\d+/))
                        {
                        splice (@array1,$x,1);
                        }
                $x++;
                }
	}
	
################################################################################
# NAME: &printArray
# CALL: &printArray(@array)
# GOAL: print an array on the standard output (generally command prompt)
################################################################################
sub printArray
	{
	local *array2=shift;
	my $size=@array2;
	print "I am in printArray and size=$size\n";
	$i=0;
	foreach (@array2)#**************************************************
                {
                print "element $i =$_=\n";
		$i++;
                }
	}

################################################################################
# NAME: formatArray
# CALL: &formatArray($format,@array,)
# GOAL: format an array in the swan file depending on the format we want the
#       array values to be
################################################################################
sub formatArray
	{
	my $format=shift;
	local *array3=shift;
	foreach (@array3)#**************************************************
                {
                $_=sprintf "$format",$_;
                }
	}
	
################################################################################
# NAME: formatDoubleArray
# CALL: &formatDoubleArray($format,@array,$forbiddenValue)
# GOAL: format an double array depending on the format we want the array values
#       to be, $forbiddenValue is optional,to avoid formatting values that can't
#       be
################################################################################
sub formatDoubleArray
	{
	my $format=shift;
        local *array4=shift;
	my $forbiddenValue=shift;
	for $i (0 .. $#array4)
		{
	     	for $j (0 .. $#{$array4[$i]})
			{
			if ($forbiddenValue && $array4[$i][$j]=~/$_[2]/)
				{
				 $array4[$i][$j]=$array4[$i][$j];
				}
				
			else
				{
					#print "for i=$i and j=$j val=$array4[$i][$j]\n";
                		$array4[$i][$j]=sprintf "$format",$array4[$i][$j];
				#print "then val=$array4[$i][$j]\n";
				}
			}
		}	
	}
	
################################################################################
# NAME: &pushDoubleArray
# CALL:&pushDoubleArray(\@arrayA,\@arrayB);
# GOAL: copy a double array(array2) as one part of the other (array1) (kind of
# insertion or push but with one line of array2 correponds as one element of
# array1 if flag=1 or one element correspond to one element if flag=2
################################################################################
sub pushDoubleArray
	{
	local (*arrayA,*arrayB,$flag)=@_;
	my $row;
	my $value;
	my $maxA=@arrayA;#number of elements (last indice +1 because we start by adding 0)
	if ($flag==1)
		{
		for $j (0 .. $#{$arrayB[$#arrayB]})
			{
			for $i (0 .. $#arrayB)
				{
				$value=$arrayB[$i][$j];
				$row.=$value;
				}
			}
		push @arrayA,$row;
		undef $row;
		}
	else 
		{
		for $i (0 .. $#arrayB)
			{
	     		for $j (0 .. $#{$arrayB[$i]})
				{
				$arrayA[$maxA+$i][$j]=$arrayB[$i][$j];
				}
			}
		}
	}
	
################################################################################
# NAME: &printDoubleArray
# CALL: &printDoubleArray($arrayRef)
# GOAL: print an array with double entries on the standard`
################################################################################
sub printDoubleArray
	{
	
	my $i;
	my $j;
	local *array6=shift;
	print "Here is the double array\n";
	for $i (0 .. $#array6)
		{
	     	for $j (0 .. $#{$array6[$i]})
			{
			print "element ($i,$j): *$array6[$i][$j]*\n";
			}
		}	
	}
	
################################################################################
# NAME: takeSpaceAway
# CALL: &takeSpaceAway(@array)
# GOAL:take the space in the cells away, not so important, more beautiful
# when print but when try to take variable to treat it the space doesn't matter,
# KEEP or NOT?
################################################################################
sub takeSpaceAway 
	{
	my $x=0;
        local *array7=shift;
        foreach (@array7)
                {
                if (($_=~m/(\s+)(\S+)/))
                        {
                        $_[$x]=$2;
                        }
                #chomp (@array7);
		#1-FIX-3-04/25/05-get rid of the remaining spaces (the chomp fct was not at the right spot and the incrementation for $x wasn't done, should fix many warnings about undefined values
		$x++;
                }
	chomp (@array7);
	}
	
################################################################################
# NAME: giveMaxDoubleArray
# CALL: &giveMaxDoubleArray($arrayRef)
# GOAL: give the maximum value of the array passed by reference
################################################################################
sub giveMaxDoubleArray
        {
	my $arrayRef=shift;
	my @array=@$arrayRef;
	my $max=0;
	for $i (0 .. $#array)
		{
		for $j (0 .. $#{$array[$i]})
			{
			if ($array[$i][$j]>$max)
				{
				$max=$array[$i][$j];
				$iMax=$i;
				$jMax=$j;
				}
			}
		}
	return ($max,$iMax,$jMax);
	}

################################################################################
# NAME: giveSumDoubleArray
# CALL: &giveSumDoubleArray(@array)
# GOAL: give the sum of all the values of the array passed by reference
################################################################################
sub giveSumDoubleArray	
	{	
	my $arrayRef=shift;
        my @array=@$arrayRef;
	my $sum=0;
	for $i (0 .. $#array)
		{
		for $j (0 .. $#{$array[$i]})
			{
			$sum=+$array[$i][$j];
			}
		}
	return $sum;
	}
															
################################################################################
# NAME: giveMaxArray
# CALL: &giveMaxArray($arrayRef)
# GOAL: give the maximum value of a one dimension array passed by reference
################################################################################
sub giveMaxArray	
	{	
        my $i;
	my $arrayRef=shift;
        my @array=@$arrayRef;
	my $max=0;
	my $iMax;
	for $i (0 .. $#array)
		{
		if ($array[$i]>$max)
			{
			$max=$array[$i];
			$iMax=$i;
			}
		}
	return ($max,$iMax);
	}
	
################################################################################
# NAME: giveMinArray
# CALL: &giveMaxArray($arrayRef)
# GOAL: give the minimum value of a one dimension array passed by reference
################################################################################
sub giveMinArray	
	{	
        my $i;
	my $arrayRef=shift;
        my @array=@$arrayRef;
	my $min=9999999999999;
	my $iMax;
	for $i (0 .. $#array)
		{
		if ($array[$i]<$min)
			{
			$min=$array[$i];
			$iMin=$i;
			}
		}
	return ($min,$iMin);
	}
	
################################################################################
# NAME: reverseDoubleArray
# CALL: &reverseDoubleArray(@array2)
# GOAL: reverse (i;j) to (j,i) in the two-dimensional array passed by reference
################################################################################
sub reverseDoubleArray
	{	
	local *array11=shift;
	my @array2;
	my $row;
	for $j (0 .. $#{$array11[$#array11]})
		{
		my @temp;
		for $i (0 .. $#array11)
			{
			$value=$array11[$i][$j];
			$row.=" $value";
			}

		@temp=split /\s+/,$row;		
		shift @temp;#because the first element is empty
		push @array2, \@temp;
		undef $row;
		}
	#print "array 2\n";
	#&printDoubleArray(\@array2);
	return \@array2;
	}

################################################################################
# NAME: &printArrayIn
# CALL: &printArrayIn(\@array,$filehandleName)
# GOAL: print an array in the corresponding file knowing the reference to this
#      array and the filehandle name. It supposes the file is already openned.
################################################################################
sub printArrayIn
	{
	my $i=0;
	local *array12=shift;
        my $file=shift;
	foreach (@array12)#**************************************************
                {
                print $file "$_\n";
		$i++;
                }
	}
	
################################################################################
# NAME: &printDoubleArrayIn
# CALL: &printDoubleArray($arrayRef)
# GOAL: print an array with double entries on the standard`
################################################################################
sub printDoubleArrayIn
	{
	my $i;
	my $j;
	local *array13=shift;
	foreach $i (0 .. $#array13)
		{
	     	foreach $j (0 .. $#{$array13[$i]})
			{
			print "element ($i,$j): *$array13[$i][$j]*\n";
			}
		}
	}
################################################################################

################################################################################
# NAME: &flipArray
# CALL: &flipArray($arrayRef)
# GOAL: flip a one D array
################################################################################
sub flipArray
	{
	my $i;
	my $arrayRef=shift;
        my @array=@$arrayRef;
	my @flippedArray;
	for $i (0 .. $#array)
	{
		$flippedArray[$#array-$i]=$array[$i];
	}
	return \@flippedArray;
	}
################################################################################
# NAME: &inArray
# CALL: &inArray($needle,@haystack)
# GOAL: return true if needle is in haystack, false otherwise
################################################################################

sub inArray{
	my $needle = shift;
	my @haystack = @_;
	my $isString = 0;

	$isString = 1 if($needle !~ /^[0-9|.|,|-]*$/);

	foreach my $element (@haystack){
		if($isString){
			return 1 if $needle eq $element;
		}else{
			Logs::bug("needle=$needle  element=$element",3);
			return 1 if $needle==$element;
		}
	}
	return 0;
}
################################################################################
# NAME: &find
# CALL: &find($arrayRef,$val,$flag)
# GOAL: find the indeces of array which are 
# flag=-2 less or equal than $val
# flag=-1 less than $val
# flag=0 equal to $val
# flag=1 more than $val
# flag=2 more or equal than $val
################################################################################
sub find{
	my ($arrayRef,$val,$flag)=@_;
	my @array=@$arrayRef;
	my @ind;
	if ($flag==-2)
	{
		for $i (0 .. $#array)
		{
			$ind[$#ind+1]=$i if ($array[$i] <= $val);
		}
	}
	elsif ($flag==-1)
	{
		for $i (0 .. $#array)
		{
			$ind[$#ind+1]=$i if ($array[$i] < $val);
		}
	}
	elsif ($flag==0)
	{
		for $i (0 .. $#array)
		{
			$ind[$#ind+1]=$i if ($array[$i] == $val);
		}
	}
	elsif ($flag==1)
	{
		for $i (0 .. $#array)
		{
			$ind[$#ind+1]=$i if ($array[$i] > $val);
		}
	}
	elsif ($flag==2)
	{
		for $i (0 .. $#array)
		{
			$ind[$#ind+1]=$i if ($array[$i] >= $val);
		}
	}
	return @ind;
}
################################################################################
1;
