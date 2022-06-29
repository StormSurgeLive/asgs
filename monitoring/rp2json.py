#!/usr/bin/env python

import os
import pika
import datetime
import json

queue_name='asgs_prod'
destinations={}
destinations['renci-prod']={
 'queue_name': queue_name,
 'queue_user': 'asgs',
 'queue_passwd': 'ZippityD0Da',
 'queue_address': 'asgs-monitor.renci.org',
 'queue_port': 5672
}
destinations['renci-dev']={
 'queue_name': queue_name,
 'queue_user': 'asgsuser',
 'queue_passwd': '6f925b19f7',
 'queue_address': 'asgs-rabbitmq-queue-dev.apps.renci.org',
 'queue_port': 5672
}

def ReadRPToDict(input_filename, uid, instance_name, physical_location):
    """
    """
    rp={"uid": uid, "instance_name": instance_name, "physical_location": physical_location}
    param_list=[]
    with open(input_filename) as f:
        for line in f:
            (key, val) = line.split(':',1)
            key=key.strip()
            val=val.strip()
            key=key.replace(' ','_')
            param_list.append([key,val])
    rp['param_list']=param_list
    return rp

def DictToJson(in_dict):
    """
    """
    return json.dumps(in_dict, indent = 4)   

def transmit_message(message):
    """
    """
    for destination,queue_params in destinations.items():
        print(f'Sending run.properties message to {destination}')
        credentials = pika.PlainCredentials(queue_params['queue_user'], 
                                            queue_params['queue_passwd'])
        parameters = pika.ConnectionParameters(queue_params['queue_address'], 
                                               queue_params['queue_port'], 
                                               '/', credentials, socket_timeout=2)
        connection = pika.BlockingConnection(parameters)
        channel = connection.channel()
        channel.queue_declare(queue=queue_params['queue_name'])
        channel.basic_publish(exchange='',routing_key=queue_params['queue_name'],body=message)
        connection.close()

def main(args):
    """
    Main entry point 
    """
    uid = args.Uid
    instance_name = args.InstanceName
    physical_location = args.LocationName
    input_filename = args.input_filename
    output_filename = args.output_filename

    if not os.path.exists(input_filename):
        print("{} DNE. No message to send.".format(input_filename))
        sys.exit(-1)

    rp_dict = ReadRPToDict(input_filename, uid, instance_name, physical_location)
    rp_json = DictToJson(rp_dict)

    if output_filename is not None: 
        with open(output_filename, "w") as outfile:  
            json.dump(rp_dict, outfile) 

    if args.Transmit: 
        transmit_message(rp_json)

    if args.Print: 
        print(rp_json)

if __name__ == '__main__':

    from argparse import ArgumentParser
    import sys

    parser = ArgumentParser(description=main.__doc__)
    parser.add_argument('--Uid', default=-1, help='ASGS UID', type=int)
    parser.add_argument('--InstanceName', default="Unknown", help='ASGS Instance name', type=str)
    parser.add_argument('--LocationName', default="Unknown", help='HPC Name/Tag', type=str)
    parser.add_argument('--input_filename', default="run.properties", help='run.properties filename', type=str)
    parser.add_argument('--output_filename', default="run.properties.json", help='Filename for outputting json', type=str)
    parser.add_argument('--Transmit', default=False, help='Whether (or not) to send message', type=bool)
    parser.add_argument('--Print', default=False, help='Whether (or not) to print json to terminal', type=bool)

    args = parser.parse_args()
    sys.exit(main(args))
