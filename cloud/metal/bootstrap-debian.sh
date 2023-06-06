#!/usr/bin/env bash

echo "deb http://archive.debian.org/debian stretch main contrib non-free" > /etc/apt/sources.list
apt-get update
apt-get install -y build-essential checkinstall
apt-get install -y libssl-dev libexpat1-dev
apt-get install -y gfortran wget curl vim screen htop tmux git sudo bc
apt-get install -y zip flex gawk procps
apt-get install -y --allow-downgrades zlib1g=1:1.2.8.dfsg-5
apt-get install -y zlib1g-dev

# symlink for /bin/env
ln -s /usr/bin/env /bin/env > /dev/null 2>&1 || echo /usr/bin/env already links to /bin/env

# asgsuser
useradd -ms /bin/bash asgsuser
echo "asgsuser ALL=(ALL:ALL) NOPASSWD:ALL" >> /etc/sudoers

# get git repo
su -c 'cd /home/asgsuser && git clone https://github.com/StormSurgeLive/asgs.git && cd ./asgs && git checkout master' - asgsuser

read -p "Set git user to? [asgsuser]? " GIT_USER
if [ -z "$GIT_USER" ]; then
  GIT_USER=asgsuser
fi

read -p "Set git email to? [asgsuser@noemail]? " GIT_EMAIL
if [ -z "$GIT_EMAIL" ]; then
  GIT_EMAIL="asgsuser@noemail"
fi

su -c "cd /home/asgsuser/asgs && git config --global user.email \"$GIT_EMAIL\" && git config --global user.name \"$GIT_NAME\""

# persist env in .bash_profile
su -c 'echo "export PATH=${PATH}:/home/asgsuser/bin:/home/asgsuser/asgs" >> /home/asgsuser/.bashrc' - asgsuser

/home/asgsuser
mkdir /home/asgsuser/bin
chown asgsuser:asgsuser /home/asgsuser/bin

# NOTE: stuff related to $WORK and $SCRATCH happens in the docker-compose.yml file
#  and is treated as a runtime environmental variable and mounted volumes
# ensure directories are set up and essential default files are in place;
mkdir /home/asgsuser/.ssh
chmod 700 /home/asgsuser/.ssh
chown -R asgsuser:asgsuser /home/asgsuser/asgs-global.conf /home/asgsuser/.ssh

mkdir /work
mkdir /scratch
chmod 777 /work /scratch

# persist env in .bash_profile
#su -c 'cd /home/asgsuser/asgs; ./init-asgs.sh -b' - asgsuser

su -c 'cd /home/asgsuser/asgs && ./init-asgs.sh -b -x "--run-steps openmpi"         || echo openmpi         - something went wonky but preserving docker image'
if [ $? != 0 ]; then
  exit 1
fi

exit
su -c 'cd /home/asgsuser/asgs && ./init-asgs.sh -b -x "--run-steps hdf5"            || echo hdf5            - something went wonky but preserving docker image'
if [ $? != 0 ]; then
  exit 1
fi
su -c 'cd /home/asgsuser/asgs && ./init-asgs.sh -b -x "--run-steps netcdf4"         || echo netcdf4         - something went wonky but preserving docker image'
if [ $? != 0 ]; then
  exit 1
fi
su -c 'cd /home/asgsuser/asgs && ./init-asgs.sh -b -x "--run-steps wgrib2"          || echo wgrib2          - something went wonky but preserving docker image'
if [ $? != 0 ]; then
  exit 1
fi
su -c 'cd /home/asgsuser/asgs && ./init-asgs.sh -b -x "--run-steps cpra-postproc"   || echo cpra-postproc   - something went wonky but preserving docker image'
if [ $? != 0 ]; then
  exit 1
fi
su -c 'cd /home/asgsuser/asgs && ./init-asgs.sh -b -x "--run-steps output"          || echo output          - something went wonky but preserving docker image'
if [ $? != 0 ]; then
  exit 1
fi
su -c 'cd /home/asgsuser/asgs && ./init-asgs.sh -b -x "--run-steps util"            || echo util            - something went wonky but preserving docker image'
if [ $? != 0 ]; then
  exit 1
fi
su -c 'cd /home/asgsuser/asgs && ./init-asgs.sh -b -x "--run-steps input-mesh"      || echo input-mesh      - something went wonky but preserving docker image'
if [ $? != 0 ]; then
  exit 1
fi
su -c 'cd /home/asgsuser/asgs && ./init-asgs.sh -b -x "--run-steps input-nodalattr" || echo input-nodalattr - something went wonky but preserving docker image'
if [ $? != 0 ]; then
  exit 1
fi
su -c 'cd /home/asgsuser/asgs && ./init-asgs.sh -b -x "--run-steps perl"            || echo perl            - something went wonky but preserving docker image'
if [ $? != 0 ]; then
  exit 1
fi
su -c 'cd /home/asgsuser/asgs && ./init-asgs.sh -b -x "--run-steps perl-modules"    || echo perl-modules    - something went wonky but preserving docker image'
if [ $? != 0 ]; then
  exit 1
fi
su -c 'cd /home/asgsuser/asgs && ./init-asgs.sh -b -x "--run-steps image-magick"    || echo image-magick    - something went wonky but preserving docker image'
if [ $? != 0 ]; then
  exit 1
fi
su -c 'cd /home/asgsuser/asgs && ./init-asgs.sh -b -x "--run-steps python3"         || echo python3         - something went wonky but preserving docker image'
if [ $? != 0 ]; then
  exit 1
fi
su -c 'cd /home/asgsuser/asgs && ./init-asgs.sh -b -x "--run-steps ffmpeg"          || echo ffmpeg          - something went wonky but preserving docker image'
if [ $? != 0 ]; then
  exit 1
fi
su -c 'cd /home/asgsuser/asgs && ./init-asgs.sh -b -x "--run-steps gnuplot"         || echo gnuplot         - something went wonky but preserving docker image'
if [ $? != 0 ]; then
  exit 1
fi
su -c 'cd /home/asgsuser/asgs && ./init-asgs.sh -b -x "--run-steps units"           || echo units           - something went wonky but preserving docker image'
if [ $? != 0 ]; then
  exit 1
fi
su -c 'cd /home/asgsuser/asgs && ./init-asgs.sh -b -x "--run-steps nco"             || echo nco             - something went wonky but preserving docker image'
if [ $? != 0 ]; then
  exit 1
fi
su -c 'cd /home/asgsuser/asgs && ./init-asgs.sh -b -x "--run-steps pigz"             || echo pigz             - something went wonky but preserving docker image'
if [ $? != 0 ]; then
  exit 1
fi
