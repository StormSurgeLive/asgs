#!/bin/sh
#@ step_name = %enstorm%
#@ job_type = parallel
#@ output = %advisdir%/%enstorm%/$(step_name).$(jobid).out
#@ error = %advisdir%/%enstorm%/$(step_name).$(jobid).err
#@ notify_user = %notifyuser%
#@ notification = error
#@ class = workq
#@ checkpoint = no
#@ restart = yes
#@ wall_clock_limit = 02:00:00
#@ node_usage = not_shared
#@ node = 2,2
#@ total_tasks = %ncpu%
#@ requirements = (Arch == "Power5")
#@ initialdir = %advisdir%/%enstorm%
#@ executable = /usr/bin/poe
#@ arguments = %adcircdir%/padcirc
#@ network.MPI =sn_single,not_shared,US,HIGH
#@ queue
