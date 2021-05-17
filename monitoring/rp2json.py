#!/usr/bin/env python

import os
import errno
import pika
import datetime
import json

def ReadRPToDict(input_filename, uid, instance_name, physical_location):
    """
    """
    rp={"uid": uid, "instance_name": instance_name, "physical_location": physical_location}

    #dsub={}
    #with open("run.properties") as f:
    #    for line in f:
    #        (key, val) = line.split(' : ')
    #        key=key.replace(' ','_')
    #        dsub[key] = val.strip()
    # dmain['param_list']=dsub

    param_list=[]
    with open(input_filename) as f:
        for line in f:
            (key, val) = line.split(' : ')
            key=key.replace(' ','_')
            val=val.strip()
            param_list.append([key,val])
    rp['param_list']=param_list
    return rp

def DictToJson(in_dict):
    """
    """
    return json.dumps(in_dict, indent = 4)   

def queue_message(message):
    """
    """
    ########## NEED TO GET THIS STUFF FROM YAML FILE #########################
    credentials = pika.PlainCredentials('asgs', 'ZippityD0Da')
    parameters = pika.ConnectionParameters('asgs-monitor.renci.org', 5672, '/', credentials, socket_timeout=2)
    connection = pika.BlockingConnection(parameters)
    channel = connection.channel()
    channel.queue_declare(queue='asgs_props')
    channel.basic_publish(exchange='',routing_key='asgs_props',body=message)
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
    #tmpDateTime = datetime.datetime.utcnow()
    #UTCDateTime = tmpDateTime.strftime("%Y-%m-%d %H:%M:%S")

    if not os.path.exists(input_filename):
        print("{} DNE. No message to send.".format(input_filename))
        sys.exit(-1)

    rp_dict = ReadRPToDict(input_filename, uid, instance_name, physical_location)
    rp_json = DictToJson(rp_dict)

    if output_filename is not None: 
        with open(output_filename, "w") as outfile:  
            json.dump(rp_dict, outfile) 

    if args.Transmit: 
        queue_message(rp_json)

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

