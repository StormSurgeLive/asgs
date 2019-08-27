#!/bin/bash
#----------------------------------------------------------------
# operator_defaults.sh : Functions required for initializing
# parameters that are Operator dependent.  
#----------------------------------------------------------------
# Copyright(C) 2019 Jason Fleming
#
# This file is part of the ADCIRC Surge Guidance System (ASGS).
#
# The ASGS is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# ASGS is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with the ASGS.  If not, see <http://www.gnu.org/licenses/>.
#----------------------------------------------------------------
THIS=config/operator_defaults.sh
operator=$USER
allMessage "$THIS: Setting default values for the Operator '${operator}'."
#
# set values based on Operator ... not basing this on $USER b/c on some platforms (e.g., hatteras)
# user name is ambiguous
case $operator in
   "ncfs-dev")
      op="bob"
      # RMQ
      RMQMessaging_Enable="on"      #  enables message generation ("on" | "off")
      RMQMessaging_Transmit="off"    #  enables message transmission ("on" | "off")
      NOTIFY_SCRIPT=ncfs_nam_notify.sh
      ems="bblanton@renci.org"
      EMAILNOTIFY=yes         # no | yes to have host HPC platform email notifications
      ACTIVATE_LIST="$ems"
      NEW_ADVISORY_LIST="$ems"
      POST_INIT_LIST=null
      POST_LIST=null
      # opendap email notification
      OPENDAPNOTIFY="bblanton@renci.org"
      #OPENDAPNOTIFY="asgs.cera.lsu@gmail.com jason.g.fleming@gmail.com" 
      # the following are related to job failure
      JOB_FAILED_LIST="bblanton@renci.org"
      NOTIFYUSER="bblanton@renci.org"
      ASGSADMIN="bblanton@renci.org"
      ;;
   "jgflemin"|"ncfs")
      op="jgf"                      # initials as nickname for appending to asgs instance names
      # RMQ
      RMQMessaging_Enable="on"      #  enables message generation ("on" | "off")
      RMQMessaging_Transmit="on"    #  enables message transmission ("on" | "off")
      # email notifications
      NOTIFY_SCRIPT=corps_cyclone_notify.sh
      EMAILNOTIFY=yes         # no | yes to have host HPC platform email notifications
      ACTIVATE_LIST=null
      NEW_ADVISORY_LIST=null
      POST_INIT_LIST=null
      POST_LIST=null
      # opendap email notification
      OPENDAPNOTIFY="jason.g.fleming@gmail.com" # space delimited if a list
      #OPENDAPNOTIFY="asgs.cera.lsu@gmail.com jason.g.fleming@gmail.com" 
      # the following are related to job failure
      JOB_FAILED_LIST="jason.fleming@seahorsecoastal.com"
      NOTIFYUSER=jason.fleming@seahorsecoastal.com
      ASGSADMIN=jason.fleming@seahorsecoastal.com
      #
      # public keys:
      #
      # jason on asus-laptop
      # ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC0hoaWsQz+OPm/cyU+pycF76sJ9rQB+vQsvBFKchF2cXVAMORSKgmjzQmP1sYVVxF1blKQJcy686qRXOvhEmJZwAOstvurHYNMO+p7es4P5+Gvcyd662jEh7MJwonvAeou45+G2QYpSQwweSOBfTJm+Hp8LUBhXfwmqripN5Ma9AFGqDK5fxz3tT3CsayqPzKP1bq0co0m8EnX8usLEPubsN187XslfstvTKx1U6iRZ7FX1Czj5rLbRGaSlGxX1iIHbTdPUbND3Z3z/FB0tkKhKDYfLmgdJdcg/pKB8qnpw+Gav4gVmbZ8XiO0vE7hhKsfY+tKRlAihU7gtLkivFMP jason@asus-laptop
      # jason on jason-desktop
      # ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQClx771H1NlolKuq2E7rjH8MxS/j8oh/eDh3YogIftpltIGcBbZUxMjfy+UJLFbBavcCEIFiL9NGx7sJTnIoXp/E6KlCmCpDaS6Ry7XKYs+tpJSwJS8b/q+T2IfSNtgCQJGyuPkqscuUlnAJjZpC1pNvfYWeT6iZsyRw1TeNmdGMuBPNaNHq0Ng6HcCFIGxa4HfcwFYq/TwAm7paAg9dJWKkrVQSSrl3S7CbURvspye8eA9nw80Kt96Xk8t3aEtoaEDtBQ7uGKj+Hxf3wUe8fEpFunxyQSqnYNQPxxxEE81E9po4kkS8t6gPUMF+byACyKsWQ7VHg8usxYrO9ARgnZR jason@jason-desktop
      # ncfs on hatteras
      # ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEA8kHpbs9pJuvd/hgPe5H21z3XcqlFgHVEFwlO/yveRFbORuy3A5N/+J9fwXIqUc5hZ/RV56WMAc9FuIAY/0HAiLmlItYCk5EQeoeIgmm9PTVAGxEtrcYfFlS2dHIqJ4LWs4vdBrFETa0G8GQ3Cwd3LtNnSj5AVjEccx/riNjjD0doNo8MbmyBODekTw1NcUDNlTnMFc4CHV1QloaFpG6pE7dZZMVZDgeF1zomqtBOGSiMpvoxOAwOvj67Q++fr3YbocxNNbo1aHSlBgekAKowxQQm6bMQVC22EzO2mzYOME6sjRwsdlW0yMuJA/ZU0ZmuQuQTZDfMk6Q7EMA1oPT/YQ== ncfs@ht4.renci.org
      # jgflemin on supermic
      # ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEAtNob16A/qrDUTJoFORaOFAy/4d6eXR4r3Jd+5sgULzAnsfCF8UExRO5l1h9MDykXc67+lPTblq1w42ZNUSK+Nzo3ejTvZdq+QBjnuOhcgC32brCxvU29KzxncKgSYliLVca9gTNlx6+vPYFsjYnqfGJd5Qgy13/NwHKVlnkMvUHPksyMnbkO/dZvP0Pk6r2udzDw/ByHkrmmljK0n/aj5po0htwkc396ZXZH0p7AMqQvMXgJWh8mpl4MG6jnSpyy3Yhr5Z4uVCaegl4DPnP3L5rdsq+iqPNE0xc1lYHc4op4ERMMR2pjGEpf7EEsgnAonL8tPcRkXh5zVsW2W4gc2Q== jgflemin@smic1
      # jgflemin on queenbee
      # ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEApUs4ourJlMHqMPq1DNV9d+IPelFnWhZHk/bOAZ/fDKHwYVP8ST5gVPk6PUF3LgPlRD151Uw78Ti7HbI8TrbOpKDroCGfR8P19AL3frtBuN+f867ae9PX12CleDSPQj7PDv5dkzLtdC04djhIebgFuhk2Ge+B92n/2rDI4BWfIhXse6Yo1Y/QBcxLfnK3Autr8VsJvdvNHpOHo/2d6evDq1Gw4XC8L3Iy0Vhp20cLiCPWFZjnymtgGJPIYHxJLHKBysmWKdEtuku4WyhI5ywPkqnLLNqAIVreKl8SHLprkkKBhRSMpa/NITlCQyR7Ee+CWlAmWyF2ugRWFedrTFbc7Q== jgflemin@qb1
      # jgflemin on lonestar5
      # ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC7uOAZwl/vFMtA/1etGQdtQeduIaoLVv0aLqnb0/1sgWpbZ1BFrzkjLuwDCu98qt24Wi4E6PFkdleFut83ujVeIt4MTXk5NFQC5/twGlvL/57s9L8e5Dm86OYrhUGkZTDpjEFA+ILT3fHeLl7KvoDgpp9HPMFpTnqT+n6uJZ/WHbw7xeFwHyF2X6nGyQ5A//IdGcESMsHU8FJUvHVwBVKIf+IXvBMYfBevIlwu/ZBn8gRVViNO7pJSzka6ULhCx3Q6ZqNcRsNsp+Mvq2QPqNXZgyyHltQbOkysxw2AwHw+vSAVP7jmwCb8WOkLudRGYQT55Fddm+E1QEKgX4EBZbgx jgflemin@login1
      # jgflemin on stampede2
      # ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDEGqQH21kwRT7b+HmR9wJ9S6TICLs50zYPM9jtZhlmLNfoIh0U4rrB88FkbBbUDPrIB+b3lINcjFGvZt3B7JeVeTHvLr0coHzl8303UlwuocXj1dWm5cQM4nYnQpSZjG0EsVX2xzXUkYL8SSrTpzkNyXRuC1Hwlw0X4GFK+OQrQ7/XdSRcs/uhGyk0gdMBWdFibb7/vBuh9nBayWMy8NwpHLdPRbVXC+BhDBPhGRaH4v3fWhYfgz/BeS+EPVyshY9y8N7rEWY04gpj4gir4uh0fjwDn2DHlcukm4r3q904UcKQci4uT6j4OPaF8rhVUjRlVaY6f9Zd7KZJtzs2CWIR jgflemin@login4.stampede2.tacc.utexas.edu
      # jgflemin on hatteras (not used for anything but put here just in case)
      # ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC2lvGOMmBb33d+GEqHsSkFUwsJkIOiv1l8FXK16Qbv+w/5uwH5MBbyAVBxKxrwFx5dc9JJBUbxZ6p67A+I73cE5OLkUkQNp8HPCbVWuvjWEAn8m9GJRKgc+xExzQLK6pa2rGtS+PyLypfXMLRbS9maYXF2CMoTcUjmrjOGS47JP0lXkCD/L/G0K7IIqkDCW4Ww98OTCnvWl/2zsO9y6CK/5v6BJHnA6Qul2lsaPq3Afm1o9BOQacNhT2XR/axk96hLCzxFx6bnBUlU9QFLwGDBpmOsZFb3NHIlk70Sn4HE8tpUSXSomC1UHF8kM/u9yUjxxpBXk1ameaZv+ALlNNNh jgflemin@ht4.renci.org
      ;;
   *)
      warn "cycle $CYCLE: $SCENARIO: $THIS: Operator $operator was not recognized."
      ;;
esac
