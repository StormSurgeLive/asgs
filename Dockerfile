# Note: Dockerfile is not meant to be built by users,
# but is meant to build the ASGS docker image avaiable
# to users via https://hub.docker.com (pending) by ASGS
# maintainers when a new release is tagged.
#
# Note: image doesn't contain ADCIRC source or binaries;
# run `initadcirc` in ASGS Shell to obtain.
#
# Build command: (used when publishing a new image to dockerhub)
#
#   docker build -t asgsdockerhub/2019stable .
#
# Run command:   (used by most, drops them directly to the ASGS Shell prompt)
#
#   docker pull asgsdockerhub/2019stable    # get from dockerhub
#   docker run -it asgsdockerhub/2019stable # create running container from pulled image

# docker image is based on ubuntu
FROM ubuntu
RUN apt-get update

# install required libraries and tools
RUN apt-get install -y build-essential checkinstall
RUN apt-get install -y zlib1g-dev
RUN apt-get install -y libssl-dev
RUN apt-get install -y gfortran
RUN apt-get install -y python-pip
RUN apt-get install -y python2.7
RUN apt-get install -y wget curl vim
RUN apt-get install -y git

# link env to expected path
RUN ln -s /usr/bin/env /bin/env > /dev/null 2>&1 || echo /usr/bin/env already links to /bin/env

# get tarball of ASGS, "2019stable"
RUN wget https://github.com/jasonfleming/asgsdockerhub/archive/2019stable.tar.gz

# unarchive/compress, install
RUN tar zxvf /2019stable.tar.gz
RUN mv /asgs-2019stable /asgs

# fix minor issue with an upstream failing test in Date::Handler,
# which is not an ASGS bug, but not handled; this line should go
# away in future releases of ASGS
RUN perl -pi -e 's/Date::Format/Date::Format Date::Handler/g' /asgs/cloud/general/init-perl-modules.sh

# directly runs asgs-brew.pl
RUN cd /asgs && ./cloud/general/asgs-brew.pl --machinename vagrant --compiler gfortran --asgs-profile default

# run whenever user creates a running container of this image
# via `docker run -it asgsdockerhub/2019stable`
ENTRYPOINT cd /asgs &&/root/bin/asgsh
