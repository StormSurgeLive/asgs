#!/usr/bin/env python
import sys
import datetime
import getopt
import pika
import json

def usage():
    s = """
Usage:
asgs-msgr_startup.py -h -u <Uid> -l <LocationName> -c <ClusterName>  -d <UTCDateTime> 
          -m <Message>  -i <InstanceName> -k <Transmit> 
				
 where -h | --Help		the text you are looking at right now
       -u | --Uid        	proc ID of asgs_main.sh process
       -l | --LocationName	name of location where model is running, i.e. UNC
       -c | --ClusterName 	name of cluster running model, i.e. Hatteras
       -d | --UTCDateTime	UTC date time - ISO 8601 format; if not provided defaults to now
       -m | --Message		the actual message to send
       -i | --InstanceName	ASGS Instance Name for "group" id in messaging
       -k | --Transmit   	Whether (or not) to actually transmit message. def="on"
 
"""
    print(s)

def JsonifyMessage(Uid,
                   LocationName,
                   ClusterName,
                   UTCDateTime,
                   Message,
                   InstanceName):
	
	msg_obj = {'uid': Uid, 
                   'name': 'asgs', 
                   'physical_location': LocationName, 
                   'clustername': ClusterName, 
                   'date-time': UTCDateTime, 
                   'message': Message,
                   'instance_name': InstanceName}

	return json.dumps(msg_obj)

def transmit_message(message):

        # Need to get this stuff from yaml file
	credentials = pika.PlainCredentials('asgs', 'ZippityD0Da')
	parameters = pika.ConnectionParameters('asgs-monitor.renci.org', 5672, '/', credentials, socket_timeout=2)
	connection = pika.BlockingConnection(parameters)
	channel = connection.channel()
	channel.queue_declare(queue="asgs_config")
	channel.basic_publish(exchange='',routing_key='asgs_config',body=message)
	connection.close()

def main(argv):

	Uid = '0'
        LocationName = 'unknown'
	ClusterName = 'unknown'
	tmpDateTime = datetime.datetime.utcnow()
	UTCDateTime = tmpDateTime.strftime("%Y-%m-%d %H:%M:%S")
	Message = 'none'
        InstanceName = 'N/A'
        Transmit = "on"

	try:
        	opts, args = getopt.getopt(argv,"hu:l:c:d:i:k",
                        ["Help","Uid=","LocationName=","ClusterName=","UTCDateTime=",
                         "Message=","InstanceName=","Transmit="])
	except getopt.GetoptError as err:
        	print('\nCommand line option error: ' + str(err))
        	usage()
        	sys.exit(2)

	for opt, arg in opts:
		if opt in ("-h", "--Help"):
			usage()
			sys.exit(2)
		elif opt in ("-k", "--Transmit"):
        		Transmit = arg
		elif opt in ("-i", "--InstanceName"):
			InstanceName = arg
		elif opt in ("-u", "--Uid"):
			Uid = arg
		elif opt in ("-l", "--LocationName"):
			LocationName = arg
		elif opt in ("-c", "--ClusterName"):
			ClusterName = arg
		elif opt in ("-d", "--UTCDateTime"):
			UTCDateTime = arg
		elif opt in ("-m", "--Message"):
			Message = arg

        if (Message == 'none'):
               # return without sending 
        	sys.exit()

	msg = JsonifyMessage(
			Uid,
                        LocationName, 
			ClusterName, 
			UTCDateTime, 
			Message, 
                        InstanceName
			)

	if (Transmit == 'on'):
                transmit_message(msg)
	        #print('\n')
	        #print(msg)
	        #print('\n')
        else:
                print('Message not transmitted.\n')
                #print('\n')


if __name__ == "__main__":
    	main(sys.argv[1:])
