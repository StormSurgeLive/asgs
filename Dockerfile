#
# Note 1: Dockerfile is not meant to be built by users,
# but is meant to build the ASGS docker image avaiable
# to users via https://hub.docker.com (pending) by ASGS
# maintainers when a new release is tagged.
#
# Note 2: image doesn't contain ADCIRC source or binaries;
# run `initadcirc` in ASGS Shell to obtain.
#
# Usage
#
# Build command: (used when publishing a new image to dockerhub)
#
#   docker build -t asgsdockerhub/master .
#
# Run command:   (used by most, drops them directly to the ASGS Shell prompt)
#
#   docker run -it asgsdockerhub/asgs    # create running container from pulled image
#
# The latest image may be retreived using the command,
#
#   docker pull asgsdockerhub/asgs
#
# Dockerhub main URL:
#
#   https://hub.docker.com/repository/docker/asgsdockerhub/asgs
#
# Note 3: Using Ubuntu 16 (Xenial) because newer versions of Ubuntu
# come with an openssl version that breaks Perl's Net::SSLeay
#
FROM ubuntu:xenial

# update to latest security updates and package sources
RUN apt-get update

# install require libraries and tools
RUN apt-get install -y build-essential checkinstall
RUN apt-get install -y zlib1g-dev libssl-dev libexpat1-dev
RUN apt-get install -y gfortran wget curl vim tmux git sudo

# symlink for /bin/env
RUN ln -s /usr/bin/env /bin/env > /dev/null 2>&1 || echo /usr/bin/env already links to /bin/env

# set env, this is used to identify the environment to ./init-asgsh.sh
ENV _ASGS_CONTAINER docker

# create non-privileged asgsuser
RUN useradd -ms /bin/bash asgsuser

# add asgsuser to suod so that they can drop into root (sudo su -)
RUN echo "asgsuser ALL=(ALL:ALL) NOPASSWD:ALL" >> /etc/sudoers

# set up WORK and SCRATCH targets to emulate how most other systems
# break up their disk space domains
RUN mkdir /work && mkdir /scratch

# set up for asgsuser
RUN chown -R asgsuser /work
RUN chown -R asgsuser /scratch

# get git repo
RUN su -c 'cd /home/asgsuser && git clone https://github.com/jasonfleming/asgs.git && cd ./asgs && git checkout docker2' - asgsuser
RUN su -c 'cd /home/asgsuser/asgs && git config --global user.email "asgsuser@noemail" && git config --global user.name "asgsuser"'

# persist env in .bash_profile
RUN su -c 'echo "export PATH=$PATH:$HOME/bin"   >> /home/asgsuser/.bash_profile' - asgsuser
RUN su -c 'echo "export _ASGS_CONTAINER=docker" >> /home/asgsuser/.bash_profile' - asgsuser
RUN su -c 'export _ASGS_CONTAINER=docker  && cd /home/asgsuser/asgs && ./init-asgs.sh BATCH=YES' - asgsuser

# start as a non-privileged user
USER asgsuser
WORKDIR /home/asgsuser
ENTRYPOINT echo && echo "run 'asgsh' to enter into ASGS Shell" && . /home/asgsuser/.bash_profile && bash -i
