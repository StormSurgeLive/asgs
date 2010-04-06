package NetCDF;

require Exporter;
require AutoLoader;
require DynaLoader;
@ISA = qw(Exporter AutoLoader DynaLoader);
# Items to export into callers namespace by default
# (move infrequently used names to @EXPORT_OK below)
@EXPORT = qw(
    FILL_BYTE
    FILL_CHAR
    FILL_DOUBLE
    FILL_FLOAT
    FILL_LONG
    FILL_SHORT
    MAX_ATTRS
    MAX_DIMS
    MAX_NAME
    MAX_OPEN
    MAX_VARS
    MAX_VAR_DIMS
    BYTE
    CHAR
    CLOBBER
    DOUBLE
    EBADDIM
    EBADID
    EBADTYPE
    EEXIST
    EGLOBAL
    EINDEFINE
    EINVAL
    EINVALCOORDS
    EMAXATTS
    EMAXDIMS
    EMAXNAME
    EMAXVARS
    ENAMEINUSE
    ENFILE
    ENOTATT
    ENOTINDEFINE
    ENOTNC
    ENOTVAR
    ENTOOL
    EPERM
    ESTS
    EUNLIMIT
    EUNLIMPOS
    EXDR
    FATAL
    FILL
    FLOAT
    GLOBAL
    INDEF
    LONG
    NOCLOBBER
    NOERR
    NOFILL
    NOWRITE
    SHORT
    SYSERR
    UNLIMITED
    VERBOSE
    WRITE
);
# Other items we are prepared to export if requested
@EXPORT_OK = qw(
);

sub AUTOLOAD {
    if (@_ > 1) {
	$AutoLoader::AUTOLOAD = $AUTOLOAD;
	goto &AutoLoader::AUTOLOAD;
    }
    local($constname);
    ($constname = $AUTOLOAD) =~ s/.*:://;
    $val = constant($constname, @_ ? $_[0] : 0);
    if ($! != 0) {
	if ($! =~ /Invalid/) {
	    $AutoLoader::AUTOLOAD = $AUTOLOAD;
	    goto &AutoLoader::AUTOLOAD;
	}
	else {
	    ($pack,$file,$line) = caller;
	    die "Your vendor has not defined netCDF macro $constname, used at $file line $line.
";
	}
    }
    eval "sub $AUTOLOAD { $val }";
    goto &$AUTOLOAD;
}

bootstrap NetCDF;

# Preloaded methods go here.  Autoload methods go after __END__, and are
# processed by the autosplit program.

1;
__END__
