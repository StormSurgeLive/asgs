# Note: dev.Dockerfile is not meant to be built by users,
# but is meant to build the ASGS docker image avaiable
# to users via https://hub.docker.com (pending) by ASGS
# maintainers.
#
# This dev.Dockerfile has the ASGS code outside the container 
# to enable development of ASGS as well as the 
# simultaneous installation and use of different versions
# of ASGS as well as installation alongside an ASGS that has
# been directly installed on a host machine (i.e., without
# the use of Docker).
#
# The idea behind this dev.Dockerfile is that the containers 
# are ephemeral but the ASGS source code should be persistent.
# This is a different goal than the main Dockerfile which is 
# meant to encapsulate absolutely everything that is needed 
# for deployment, since it won't need to be modified in production. 
#
# As a result, this image also doesn't contain ADCIRC 
# source or binaries; run `initadcirc` in ASGS Shell to
# obtain. It also doesn't contain ASGS source or binaries;
# the ASGS repository is cloned and edited outside the 
# container and then built inside the container. Running ASGS 
# for nowcast/forecast cycles will require entering the
# container and using asgsh as asgsuser.  
#
# Step by step instructions
#
# Make a directory where you want to work on ASGS code 
# (using sudo if necessary, and possibly needing to 
# chown to $USER and chgrp to something appropriate):
# 
#   mkdir -p /srv/work/asgs.dev
#
# Make directory where you want the nowcasts and forecasts
# to be stored:
#
#   mkdir -p /srv/scratch/asgs.dev
#
# Make directory where you want the ASGS installation artifacts
# to be stored:
#
#   mkdir -p ${HOME}/asgsuser/asgs.dev
#
# For now, copy the ${HOME}/asgs-global.conf file and 
# ${HOME}/.ssh subdirectory to ${HOME}/asgsuser/asgs.dev
# (this step will eventually be replaced by injecting 
# environment variables every time the container is 
# instantiated) ... make sure both these files are 
# up to date, and redo this step if these files change :
#
#   cp ${HOME}/asgs-global.conf ${HOME}/asgsuser/asgs.dev
#
#   cp -a ${HOME}/.ssh ${HOME}/asgsuser/asgs.dev
#
# Git credentials are not needed in the container since
# the ASGS and ADCIRC source files will be edited, 
# committed, and push/pulled outside the container.
# In contrast, any building, rebuilding, or execution of
# ADCIRC and ASGS will have to be done inside the container.  
#
# Go to the directory where you want to work on the code
# and clone the repository :
#
#   cd /srv/work/asgs.dev && git clone https://github.com/jasonfleming/asgs
#
# Switch to your development branch (if necessary)
# 
#   git checkout mybranch  # (if necessary)
#
# Either pull the asgs.dev.image from dockerhub (not yet) or build it :
#
#   # get from dockerhub (FIXME: not yet)
#   FIXME (not yet): docker pull asgsdockerhub/master    
#
#     or 
#
#     cd ./cloud/general && docker build . --file dev.Dockerfile -t asgs.dev.image
# 
# This image expects its containers to have /work /scratch and
# /home/asgsuser mounted to appropriate places on the command line
# that instantiated the container. These locations have the
# following meaning : 
#
# /work           where the ASGS source/scripts/executables directory
#                 is found (i.e., it looks for the asgs dir there)
#
# /scratch        where the ASGS runs and produces
#                 input and output for nowcasts and forecasts
#
# /home/asgsuser  where the asgsh installation artifacts are stored  
#                 (like /home/asgsuser/bin/asgsh etc) and where
#                 configuration (like ~/.ssh and ~/asgs-global.conf)
#                 artifacts are found (for now ... this will be 
#                 replaced by passing env vars in the future)
#
# This can be accomplished with something like the following:  
#
#    docker run --rm                                            \
#               --name asgs.dev                                 \
#               -v /srv/work/asgs.dev:/work                     \
#               -v /srv/scratch/asgs.dev:/scratch               \
#               -v /home/jason/asgsuser/asgs.dev:/home/asgsuser \
#               -t asgs.dev.image
#
# FIXME : Eventually we need to use environment variables to pass
# additional configuration and secrets to the container to deal with
# ssh configuration and asgs-global.conf etc ... for now we are just
# just copying these files manually. This manual copying can happen
# before or after starting the container but must happen before 
# starting asgsh inside the container. 
#
# Then you can get into the running container via the following:
# 
#    docker exec -i -t asgs.dev /bin/bash
#
# From there, build/install ASGS and ADCIRC the usual way. For more
# information on that, visit
#
#    https://hub.docker.com/repository/docker/asgsdockerhub/asgs
#
# After following this process installation, you will be able 
# to stop and start containers from this image and the installation
# files and Operator/Machine configuration will still be there 
# for you. :-) 
# 
# ----------------------------------------------------
#
# using xenial because newer versions of Ubuntu come
# with an openssl version that breaks Perl's Net::SSLeay
FROM ubuntu:xenial

# update to latest security updates and package sources
RUN apt-get update

# install require libraries and tools
RUN apt-get install -y build-essential checkinstall
RUN apt-get install -y zlib1g-dev libssl-dev libexpat1-dev
RUN apt-get install -y gfortran wget curl vim screen htop tmux git sudo

# symlink for /bin/env
RUN ln -s /usr/bin/env /bin/env > /dev/null 2>&1 || echo /usr/bin/env already links to /bin/env

# set env, this is used to identify the environment to ./init-asgsh.sh
ENV _ASGS_CONTAINER docker

# create non-privileged asgsuser
RUN useradd -ms /bin/bash asgsuser

# add asgsuser to sudo so that they can drop into root (sudo su -)
RUN echo "asgsuser ALL=(ALL:ALL) NOPASSWD:ALL" >> /etc/sudoers

# set up WORK and SCRATCH targets to emulate how most other systems
# break up their disk space domains. 
RUN mkdir /work && mkdir /scratch

# I've added project directories so the Operator can use the container
# to run asgs utilities on files in different directory hierarchies.
RUN mkdir /project01 && mkdir /project02 && mkdir /project03

# set up for asgsuser
RUN chown -R asgsuser /work
RUN chown -R asgsuser /scratch
RUN chown -R asgsuser /project01
RUN chown -R asgsuser /project02
RUN chown -R asgsuser /project03

# persist env in .bash_profile
RUN su -c 'echo "export PATH=$PATH:$HOME/bin"   >> /home/asgsuser/.bash_profile' - asgsuser
RUN su -c 'echo "export _ASGS_CONTAINER=docker" >> /home/asgsuser/.bash_profile' - asgsuser
RUN su -c 'export _ASGS_CONTAINER=docker  && cd /home/asgsuser' - asgsuser

# start as a non-privileged user
USER asgsuser
WORKDIR /work
ENTRYPOINT ["tail", "-f", "/dev/null"]#
#ENTRYPOINT echo && echo "run 'asgsh' to enter into ASGS Shell" && . /home/asgsuser/.bash_profile && echo "sourced .bash_profile" && bash -i