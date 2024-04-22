package Config::Tiny;
 
# If you thought Config::Simple was small...
 
use strict;
use 5.008001; # For the utf8 stuff.
 
# Warning: There is another version line, in t/02.main.t.
 
our $VERSION = '2.30';
 
BEGIN {
        $Config::Tiny::errstr  = '';
}
 
# Create an object.
 
sub new { return bless defined $_[1] ? $_[1] : {}, $_[0] }
 
# Create an object from a file.
 
sub read
{
        my($class)           = ref $_[0] ? ref shift : shift;
        my($file, $encoding) = @_;
 
        return $class -> _error('No file name provided') if (! defined $file || ($file eq '') );
 
        # Slurp in the file.
 
        $encoding = $encoding ? "<:$encoding" : '<';
        local $/  = undef;
 
        open(my $CFG, $encoding, $file) or return $class -> _error( "Failed to open file '$file' for reading: $!" );
        my $contents = <$CFG>;
        close($CFG );
 
        return $class -> _error("Reading from '$file' returned undef") if (! defined $contents);
 
        return $class -> read_string( $contents );
 
} # End of read.
 
# Create an object from a string.
 
sub read_string
{
        my($class) = ref $_[0] ? ref shift : shift;
        my($self)  = bless {}, $class;
 
        return undef unless defined $_[0];
 
        # Parse the file.
 
        my $ns      = '_';
        my $counter = 0;
 
        foreach ( split /(?:\015{1,2}\012|\015|\012)/, shift )
        {
                $counter++;
 
                # Skip comments and empty lines.
 
                next if /^\s*(?:\#|\;|$)/;
 
                # Remove inline comments.
 
                s/\s\;\s.+$//g;
 
                # Handle section headers.
 
                if ( /^\s*\[\s*(.+?)\s*\]\s*$/ )
                {
                        # Create the sub-hash if it doesn't exist.
                        # Without this sections without keys will not
                        # appear at all in the completed struct.
 
                        $self->{$ns = $1} ||= {};
 
                        next;
                }
 
                # Handle properties.
 
                if ( /^\s*([^=]+?)\s*=\s*(.*?)\s*$/ )
                {
                        if ( substr($1, -2) eq '[]' )
                        {
                                my $k = substr $1, 0, -2;
                                $self->{$ns}->{$k} ||= [];
                                return $self -> _error ("Can't mix arrays and scalars at line $counter" ) unless ref $self->{$ns}->{$k} eq 'ARRAY';
                                push @{$self->{$ns}->{$k}}, $2;
                                next;
                        }
                        $self->{$ns}->{$1} = $2;
 
                        next;
                }
 
                return $self -> _error( "Syntax error at line $counter: '$_'" );
        }
 
        return $self;
}
 
# Save an object to a file.
 
sub write
{
        my($self)            = shift;
        my($file, $encoding) = @_;
 
        return $self -> _error('No file name provided') if (! defined $file or ($file eq '') );
 
        $encoding = $encoding ? ">:$encoding" : '>';
 
        # Write it to the file.
 
        my($string) = $self->write_string;
 
        return undef unless defined $string;
 
        open(my $CFG, $encoding, $file) or return $self->_error("Failed to open file '$file' for writing: $!");
        print $CFG $string;
        close($CFG);
 
        return 1;
 
} # End of write.
 
# Save an object to a string.
 
sub write_string
{
        my($self)     = shift;
        my($contents) = '';
 
        for my $section ( sort { (($b eq '_') <=> ($a eq '_')) || ($a cmp $b) } keys %$self )
        {
                # Check for several known-bad situations with the section
                # 1. Leading whitespace
                # 2. Trailing whitespace
                # 3. Newlines in section name.
 
                return $self->_error("Illegal whitespace in section name '$section'") if $section =~ /(?:^\s|\n|\s$)/s;
 
                my $block = $self->{$section};
                $contents .= "\n" if length $contents;
                $contents .= "[$section]\n" unless $section eq '_';
 
                for my $property ( sort keys %$block )
                {
                        return $self->_error("Illegal newlines in property '$section.$property'") if $block->{$property} =~ /(?:\012|\015)/s;
 
                        if (ref $block->{$property} eq 'ARRAY') {
                                for my $element ( @{$block->{$property}} )
                                {
                                        $contents .= "${property}[]=$element\n";
                                }
                                next;
                        }
                        $contents .= "$property=$block->{$property}\n";
                }
        }
 
        return $contents;
 
} # End of write_string.
 
# Error handling.
 
sub errstr { $Config::Tiny::errstr }
sub _error { $Config::Tiny::errstr = $_[1]; undef }
 
1;
