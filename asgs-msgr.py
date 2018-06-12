import sys
import datetime
import getopt
import yaml
import pika
import json

def usage():
	print('\nUsage:\n')
	print('asgs-msgr.py -h -u <Uid> -l <LocationName> -c <ClusterName> -t <RunType> -d <UTCDateTime> -s <StormName> -n <StormNumber> -a <AdvisoryNumber> -m <Message> -y <MessageType')
	print('		')		
	print(' where -h | --Help		the text you are looking at right now')
	print('       -u | --Uid		a unique id for this particular model run')
	print('       -l | --LocationName	name of location where model is running, i.e. UNC')
	print('       -c | --ClusterName 	name of cluster running model, i.e. Hatteras')
	print('       -t | --RunType		hurricane | weather')
	print('       -d | --UTCDateTime	UTC date time - ISO 8601 format; if not provided defaults to now')
	print('       -s | --StormName		hurricane name, i.e. Irene')
	print('       -n | --StormNumber 	hurricane id number')
	print('       -a | --AdvisoryNumber	NHC advisory number for this run')
	print('       -m | --Message		the actual message to send')
	print('       -p | --MessageType	message type, i.e. startup | info | warning | error, etc')
	print(' ')

def JsonifyMessage(Uid,
                   LocationName,
                   ClusterName,
                   RunType,
                   UTCDateTime,
                   StormName,
                   StormNumber,
                   AdvisoryNumber,
                   Message,
                   MessageType,
                   Process,
                   PctComplete,
                   State):
	
	if (RunType == 'hurricane'):
		hurr_obj = {'storm': StormName, 'storm_number': StormNumber, 'advisory_number': AdvisoryNumber}
		main_obj = {'name': 'asgs', 'uid': Uid, 'physical_location': LocationName, 'clustername': ClusterName, 
                            'run_type': RunType, 'date-time': UTCDateTime, 'hurricane': hurr_obj, 'message': Message, 
                            'message_type': MessageType ,'process': Process, 'pctcomplete':  PctComplete, 'state':State}
	else:
		main_obj = {'name': 'asgs', 'uid': Uid, 'physical_location': LocationName, 'clustername': ClusterName, 
                            'run_type': RunType, 'date-time': UTCDateTime, 'message': Message, 'message_type': MessageType, 
                            'process': Process, 'pctcomplete': PctComplete, 'state':State}

	return json.dumps(main_obj)


def queue_message(message):

	#print(message)
	#return

########## NEED TO GET THIS STUFF FROM YAML FILE #########################
	credentials = pika.PlainCredentials('asgs', 'ZippityD0Da')
	parameters = pika.ConnectionParameters('asgs-monitor.renci.org', 5672, '/', credentials, socket_timeout=2)
	connection = pika.BlockingConnection(parameters)
	channel = connection.channel()
	channel.queue_declare(queue="asgs_queue")
	channel.basic_publish(exchange='',routing_key='asgs_queue',body=message)
	connection.close()

def main(argv):

	Uid = ''
	LocationName = ''
	ClusterName = ''
	RunType = ''
	tmpDateTime = datetime.datetime.utcnow()
	UTCDateTime = tmpDateTime.strftime("%Y-%m-%d %H:%M:%S")
	StormName = ''
	StormNumber = ''
	AdvisoryNumber = ''
	Message = 'test message'
	MessageType = ''
	Process = ''
	PctComplete = ''
        State=''

	try:
        	opts, args = getopt.getopt(argv,"hu:l:c:t:d:s:n:a:m:p:q:r:u",["Help","Uid=","LocationName=","ClusterName=","RunType=","UTCDateTime=","StormName=", "StormNumber=", "AdvisoryNumber=", "Message=", "MessageType=", "Process=", "PctComplete=", "State="])
	except getopt.GetoptError as err:
        	print('\nCommand line option error: ' + str(err))
        	usage()
        	sys.exit(2)

	for opt, arg in opts:
		if opt in ("-h", "--Help"):
			usage()
			sys.exit(2)
		elif opt in ("-u", "--Uid"):
			Uid = arg
		elif opt in ("-l", "--LocationName"):
			LocationName = arg
		elif opt in ("-c", "--ClusterName"):
			ClusterName = arg
		elif opt in ("-t", "--RunType"):
			RunType = arg
		elif opt in ("-d", "--UTCDateTime"):
			UTCDateTime = arg
		elif opt in ("-s", "--StormName"):
			StormName = arg
		elif opt in ("-n", "--StormNumber"):
			StormNumber = int(arg)
		elif opt in ("-a", "--AdvisoryNumber"):
			AdvisoryNumber = arg     
		elif opt in ("-m", "--Message"):
			Message = arg
		elif opt in ("-p", "--MessageType"):
			MessageType = arg
		elif opt in ("-q", "--Process"):
			Process = arg
		elif opt in ("-r", "--PctComplete"):
			PctComplete = arg
		elif opt in ("-u", "--State"):
			State = arg

	msg = JsonifyMessage(
			Uid, 
			LocationName, 
			ClusterName, 
			RunType, 
			UTCDateTime, 
			StormName, 
			StormNumber, 
			AdvisoryNumber, 
			Message, 
			MessageType,
                        Process,
                        PctComplete,
                        State
			)

#	if (SendMessage):
	queue_message(msg)


if __name__ == "__main__":
    	main(sys.argv[1:])
