SCRIPTDIR=/home/bblanton
PERL5LIB=${SCRIPTDIR}/PERL
export PERL5LIB=${SCRIPTDIR}:${PERL5LIB}

perl ./get_atcf.pl --storm 06 --year 2018 --ftpsite filesystem --fdir /home/bblanton/asgs-advisories/ --hdir /home/bblanton/asgs-advisories/ --rsssite filesystem --trigger rssembedded --adv 37
