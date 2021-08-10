#!/bin/awk
# "jobtype" : "prep15", "submit" : "2021-Aug-06-T06:42:45-0500", "jobid" : "889391.qb3", "start" : "null", "finish" : "null", "error" : "null"
# the files.status.json file is mostly a pass through
$1 !~ "jobtype" { print $0 }
# turn the closing brace of files.status.json into a comma
$1 == "}" { print "," } 
# add start of json array to start jobs.status section 
# and initialize variables to first lin e in the file
$1 ~ "jobtype" && FNR == 1 { 
   print "\"jobs.status\" : ["
   jobtype=$3
   submit=$6
   jobid=$9
   start=$12
   finish=$15
   error=$18 
} 
$1 ~ "jobtype" && ($9 != jobid && jobid !~ "null")  {
   # write the line of json data when the job id changes from 
   # one non-null value to another
   print "{ \"jobtype\" : "jobtype" \"submit\" : "submit" \"jobid\" : "jobid" \"start\" : "start" \"finish\" : "finish" \"error\" : "error" },"
   # reset variables to values on the current line
   jobtype=$3 
   submit=$6
   jobid=$9
   start=$12
   finish=$15
   error=$18 
}
#  if there is no jobid, this indicates the job submission failed
$1 ~ "jobtype" && $9 ~ "null" {
   print "{ \"jobtype\" : "$3" \"submit\" : "$6" \"jobid\" : "$9" \"start\" : "$12" \"finish\" : "$15" \"error\" : "$18" },"
   jobtype=$3
   submit=$6
   jobid=$9
   start=$12
   finish=$15
   error=$18 
}
$1 ~ "jobtype" {
   jobtype=$3
   if ( $6 !~ "null" )
      submit=$6
   jobid=$9
   if ( $12 !~ "null" )
      start=$12
   if ( $15 !~ "null" ) 
      finish=$15
   if ( $18 !~ "null" )
      error=$18 
}
END {
   if ( $1 ~ "jobtype" ) {
      # need to write the final job data
      print "{ \"jobtype\" : "jobtype" \"submit\" : "submit" \"jobid\" : "jobid" \"start\" : "start" \"finish\" : "finish" \"error\" : "error" }"
      print "]" # end of jobs.status array
      print "}" # end of scenario.status.json
   }
}
