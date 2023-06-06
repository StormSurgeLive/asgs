#!/usr/bin/env bash

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
su -c 'cd /home/asgsuser/asgs; ./init-asgs.sh -b' - asgsuser
