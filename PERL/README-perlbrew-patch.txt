Recent versions of perlbrew cause fatal
errors on Lustre systems like that on
Stampede2. The fix is to apply the included
patch.

1. install perlbrew, but before you install any versions of
perl,

2. run this command from the same directory as the patch:

	cp patchperl.patch ~/perl5 && cd ~/perl5 && patch -p 1 < ./patchperl.patch

3. install the recommended version of perl using perlbrew:

Assuming the patch got applied correctly, you should be able to
build perl. Recent experience has also shown that 1 test fails
during the final phases of the build install, and this causes
the entire install to be aborted. It is fine to build perl
without tests if this happens:

	perlbrew --notest install perl-5.28.2
