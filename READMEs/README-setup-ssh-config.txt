ASGS uses ssh for sending files to remote hosts and
for executing commands on remote hosts (e.g., mkdir when
sending output files to THREDDS servers in the file,
output/opendap_post.sh.

Note: All servers in the default ssh_config referenced below
must also be registered in ./platforms.sh. Pay particular
attention to the hostname and port. Not all servers use the
default ssh port of 22.

To make managing authentication easier in the ASGS driver
code and for the centralization of authentication management
for operators, we assume that there exists a $HOME/.ssh/config
file that ssh naturally uses for any hosts that has been
defined in it.

Steps for Setting up ASGS SSH Access for the First Time:

0. if you have not already created a private key pair, do so
with the following command:

    ssh-keygen -b 4096 # follow the prompts, accept defaults

Note 1: Don't add a passphrase; also note that if the directory
$HOME/.ssh doesn't already exist, this command will create it
with the proper permissions.

Note 2: If you don't have accounts established on the remote
resources, contact the resource admins ASAP. They will need the
PUBLIC key of the pair you just created.

1.a. if there is no $HOME/.ssh/config file, copy the one in the
root directory of the ASGS repo to $HOME/.ssh

    cp ./ssh_config $HOME/.ssh/config
    chmod 600 $HOME/.ssh/config              # set it to read for $USER only

1.b. if you already have an $HOME/.ssh/config file, append the
contents of ./ssh_config to $HOME/.ssh/config:

    cat ./ssh_config >> $HOME/.ssh/config
    chmod 600 $HOME/.ssh/config              # set it to read for $USER only

2. Open up $HOME/.ssh/config in an editor (vim, etc), and inspect
the file. You'll note that each of the remote resources (e.g.,
THREDDS servers are defined along with details such as, "host",
"user", etc. 

3. Edit the file so that it reflects accurately for each host:

a. the correct username for each remote resource (might be different
based on how your access was enabled)

b. path to your ssh private key (IdentityFile)

4. Verify access to each resource by using ssh with the host aliases
that have been defined. For example, if properly established on the
remote machines, the following ssh commands should login you into them
directly without any prompting for user names or errors:

   ssh lsu_tds
   ssh renci_tds
   ssh tacc_tds

Assuming these all work, ssh access for ASGS has been established
successfully.
