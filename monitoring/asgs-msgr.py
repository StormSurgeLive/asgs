#!/usr/bin/env python
import sys
import datetime
import getopt
import pika
import json

def usage():
    print('\nUsage:\n')
    print('asgs-msgr.py -h -u <Uid> -l <LocationName> -c <ClusterName>  -d <UTCDateTime> -s <StormName> -n <StormNumber> -a <AdvisoryNumber> -m <Message> -y <EventType> -p <Process> -t <PctComplete> -b <SubPctComplete> -r <State> -q <RunParams> -i <InstanceName> -k <Transmit> ')
    print('        ')        
    print(' where -h | --Help        the text you are looking at right now')
    print('       -u | --Uid            proc ID of asgs_main.sh process')
    print('       -l | --LocationName    name of location where model is running, i.e. UNC')
    print('       -c | --ClusterName     name of cluster running model, i.e. Hatteras')
    print('       -d | --UTCDateTime    UTC date time - ISO 8601 format; if not provided defaults to now')
    print('       -s | --StormName        hurricane name, i.e. Irene')
    print('       -n | --StormNumber     hurricane id number')
    print('       -a | --AdvisoryNumber    NHC advisory number for this run')
    print('       -m | --Message        the actual message to send')
    print('       -y | --EventType        event type, RSTR | PRE1 | NOWC | PRE2 | FORC | POST | REND ')
    print('       -p | --Process        software process issuing this message')
    print('       -t | --PctComplete    numeric percentage of completion of process running, i.e. 34.2')
    print('       -b | --SubPctComplete    numeric percentage of completion of a subprocess running, i.e. 34.2')
    print('       -r | --State        ASGS run state, STRT | RUNN | PEND | FAIL | WARN | IDLE | EXIT')
    print('       -q | --RunParams        Info string to put on Dashboard')
    print('       -i | --InstanceName    ASGS Instance Name for "group" id in messaging')
    print('       -k | --Transmit       Whether (or not) to actually transmit message. def="on"')
    print(' ')

def JsonifyMessage(Uid,
                   LocationName,
                   ClusterName,
                   UTCDateTime,
                   StormName,
                   StormNumber,
                   AdvisoryNumber,
                   Message,
                   EventType,
                   Process,
                   PctComplete,
                   SubPctComplete,
                   State,
                   RunParams,
                   InstanceName):
    
    msg_obj = {'uid': Uid, 'name': 'asgs', 'physical_location': LocationName, 'clustername': ClusterName, 
                   'date-time': UTCDateTime, 'message': Message, 'event_type': EventType, 'process': Process, 
                   'pctcomplete': PctComplete, 'state': State, 'storm': StormName, 'storm_number': StormNumber, 
                   'advisory_number': AdvisoryNumber, 'run_params': RunParams, 'instance_name': InstanceName,
           'subpctcomplete': SubPctComplete}

    return json.dumps(msg_obj)


def queue_message(message):

########## NEED TO GET THIS STUFF FROM YAML FILE #########################
    credentials = pika.PlainCredentials('asgs', 'ZippityD0Da')
    parameters = pika.ConnectionParameters('asgs-monitor.renci.org', 5672, '/', credentials, socket_timeout=2)
    connection = pika.BlockingConnection(parameters)
    channel = connection.channel()
    channel.queue_declare(queue="asgs_queue")
    channel.basic_publish(exchange='',routing_key='asgs_queue',body=message)
    connection.close()


def main(argv):

    Uid = '0'
    LocationName = 'unknown'
    ClusterName = 'unknown'
    tmpDateTime = datetime.datetime.utcnow()
    UTCDateTime = tmpDateTime.strftime("%Y-%m-%d %H:%M:%S")
    StormName = 'unknown'
    StormNumber = 'unknown'
    AdvisoryNumber = 'unknown'
    Message = 'none'
    EventType = ''
    Process = 'asgs'
    PctComplete = '0'
    SubPctComplete = '0'
    State = 'unknown'
    RunParams = 'N/A'
    InstanceName = 'N/A'
    Transmit = "on"

    try:
        opts, args = getopt.getopt(argv,"hu:l:c:d:s:n:a:m:y:p:t:r:q:i:k:u",
                    ["Help","Uid=","LocationName=","ClusterName=","UTCDateTime=",
                     "StormName=", "StormNumber=", "AdvisoryNumber=", "Message=",
                     "EventType=", "Process=", "PctComplete=", "State=", "RunParams=",
                     "InstanceName=", "Transmit=", "SubPctComplete="])
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
        elif opt in ("-q", "--RunParams"):
            RunParams = arg
        elif opt in ("-u", "--Uid"):
            Uid = arg
        elif opt in ("-l", "--LocationName"):
            LocationName = arg
        elif opt in ("-c", "--ClusterName"):
            ClusterName = arg
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
        elif opt in ("-y", "--EventType"):
            EventType = arg
        elif opt in ("-p", "--Process"):
            Process = arg
        elif opt in ("-t", "--PctComplete"):
            PctComplete = arg
        elif opt in ("-b", "--SubPctComplete"):
            SubPctComplete = arg
        elif opt in ("-r", "--State"):
            State = arg

    if (Message == 'none'):
        # return without sending 
        sys.exit()

    msg = JsonifyMessage(
            Uid,
            LocationName, 
            ClusterName, 
            UTCDateTime, 
            StormName, 
            StormNumber, 
            AdvisoryNumber, 
            Message, 
            EventType,
            Process,
            PctComplete,
            SubPctComplete,
            State,
            RunParams,
            InstanceName
            )

    if (Transmit == 'on'):
        queue_message(msg)
    else:
        print('Message not transmitted.\n')


if __name__ == "__main__":
    main(sys.argv[1:])
