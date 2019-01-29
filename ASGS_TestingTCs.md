# ASGS for IDIOTS, testing with prior TCs

1) spinup as usual, for a TC
   Set the External data sources : Tropical cyclones block in the config file accordingly, such as: 

        STORM=13  # storm number, e.g. 05=ernesto in 2006 
        YEAR=2003 # year of the storm (useful for historical storms) 
        TRIGGER=rssembedded  # either "ftp" or "rss"
        RSSSITE=filesystem   # www.nhc.noaa.gov 
        FTPSITE=filesystem   # ftp.nhc.noaa.gov  # real anon ftp site for hindcast/forecast files
        #FDIR=/atcf/afst     # forecast dir on nhc ftp site 
        #HDIR=/atcf/btk      # hindcast dir on nhc ftp site 
        FDIR=/home/bblanton/GitHub/renci-unc/asgs/input/sample_advisories/2003
        HDIR=/home/bblanton/GitHub/renci-unc/asgs/input/sample_advisories/2003

2) 
