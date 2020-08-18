FROM ubuntu:xenial

# Note: Dockerfile is not meant to be built by users,
# but is meant to build the ASGS docker image avaiable
# to users via https://hub.docker.com (pending) by ASGS
# maintainers when a new release is tagged.
#
# Build image locally
#
#   docker build --no-cache --force-rm -t asgsdockerhub/master .
#
# Push image to Dockerhub (requires authentication)
#
#    docker push asgsdockerhub/asgs:latest
#
# For more information on running the container, visit
#
#    https://hub.docker.com/repository/docker/asgsdockerhub/asgs
#

RUN apt-get update
RUN apt-get install -y build-essential checkinstall
RUN apt-get install -y zlib1g-dev libssl-dev libexpat1-dev
RUN apt-get install -y gfortran wget curl vim screen htop tmux git sudo

# symlink for /bin/env
RUN ln -s /usr/bin/env /bin/env > /dev/null 2>&1 || echo /usr/bin/env already links to /bin/env

# set env
ENV _ASGS_CONTAINER="docker"

# asgsuser
RUN useradd -ms /bin/bash asgsuser 
RUN echo "asgsuser ALL=(ALL:ALL) NOPASSWD:ALL" >> /etc/sudoers

# set up WORK and SCRATCH targets
RUN mkdir /work && mkdir /scratch

# set up for asgsuser
RUN chown -R asgsuser /work
RUN chown -R asgsuser /scratch

# get git repo
RUN su -c 'cd /home/asgsuser && git clone https://github.com/jasonfleming/asgs.git && cd ./asgs && git checkout master' - asgsuser
RUN su -c 'cd /home/asgsuser/asgs && git config --global user.email "asgsuser@noemail" && git config --global user.name "asgsuser"'

# persist env in .bash_profile
RUN su -c 'echo "export _ASGS_CONTAINER=docker" >> /home/asgsuser/.bash_profile' - asgsuser

# install asgs
RUN su -c 'export _ASGS_CONTAINER=docker  && cd /home/asgsuser/asgs && ./init-asgs.sh BATCH=YES' - asgsuser

COPY entrypoint.sh /home/asgsuser/entrypoint.sh
RUN chmod 750 /home/asgsuser/entrypoint.sh
RUN chown asgsuser /home/asgsuser/entrypoint.sh

RUN su -c 'echo "export PATH=${PATH}:/home/asgsuser/bin" >> /home/asgsuser/.bashrc' - asgsuser
WORKDIR /home/asgsuser
USER asgsuser
ENTRYPOINT ["tail", "-f", "/dev/null"]#
