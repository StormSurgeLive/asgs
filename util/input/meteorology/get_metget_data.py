#!/usr/bin/env python3
#
# This file is part of Floodwater.
# Copyright (c) 2022 The Water Institute of the Gulf.
# 
# This program is free software: you can redistribute it and/or modify  
# it under the terms of the GNU General Public License as published by  
# the Free Software Foundation, version 3.
#
# This program is distributed in the hope that it will be useful, but 
# WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License 
# along with this program. If not, see <http://www.gnu.org/licenses/>.
#
#
#   Author: Zachary Cobell
#  Contact: zcobell@thewaterinstitute.org
#
#
import requests
import json
from datetime import datetime
import sys
import argparse
import time
import os

AVAILABLE_MODELS = {
    "gfs": "gfs-ncep",
    "nam": "nam-ncep",
    "hwrf": "hwrf",
    "coamps": "coamps-tc",
}
AVAILABLE_VARIABLES = {"wind_pressure", "rain", "temperature", "humidity", "ice"}
AVAILABLE_FORMATS = {"ascii", "owi-ascii", "adcirc-netcdf", "hec-netcdf", "delft3d"}


def valid_datetime_type(arg_datetime_str) -> datetime:
    try:
        return datetime.strptime(arg_datetime_str, "%Y-%m-%d %H:%M")
    except ValueError:
        msg = "Given DateTime ({0}) not valid! Expected format: 'YYYY-MM-DD HH:mm'".format(
            arg_datetime_str
        )
        raise argparse.ArgumentTypeError(msg)


def parse_domain_data(domain_list: list, level) -> dict:
    import warnings

    model = domain_list[0]
    if "hwrf" in model:
        storm = model.split("-")[1]
        model = "hwrf"
        warnings.warn(
            "HWRF not fully supported yet. Use at your own risk.", RuntimeWarning
        )
    elif "coamps" in model:
        storm = model.split("-")[1]
        model = "coamps"
        warnings.warn(
            "COAMPS not fully supported yet. Use at your own risk.", RuntimeWarning
        )

    res = float(domain_list[1])
    x0 = float(domain_list[2])
    y0 = float(domain_list[3])
    x1 = float(domain_list[4])
    y1 = float(domain_list[5])

    if model not in AVAILABLE_MODELS.keys():
        raise RuntimeError("Specified model '" + model + "' is not available")

    xmax = max(x0, x1)
    xmin = min(x0, x1)
    ymax = max(y0, y1)
    ymin = min(y0, y1)
    res = abs(res)
    if res <= 0:
        raise RuntimeError("Specified model resolution is invalid")

    if model == "hwrf" or model == "coamps":
        return {
            "name": AVAILABLE_MODELS[model] + "-" + storm,
            "service": AVAILABLE_MODELS[model],
            "storm": storm,
            "x_init": xmin,
            "y_init": ymin,
            "x_end": xmax,
            "y_end": ymax,
            "di": res,
            "dj": res,
            "level": level,
        }
    else:
        return {
            "name": model,
            "service": AVAILABLE_MODELS[model],
            "x_init": xmin,
            "y_init": ymin,
            "x_end": xmax,
            "y_end": ymax,
            "di": res,
            "dj": res,
            "level": level,
        }


def make_metget_request(endpoint, apikey, request_json):
    headers = {"x-api-key": apikey}
    r = requests.post(endpoint + "/build", headers=headers, json=request_json)
    if r.status_code != 200:
        raise RuntimeError(
            "Request to MetGet was returned status code = " + str(r.status_code)
        )
    return_data = json.loads(r.text)
    data_id = return_data["body"]["request_id"]
    status_code = return_data["statusCode"]
    if status_code != 200:
        with open("metget.debug", "a") as f:
            f.write("[WARNING]: MetGet returned status code " + str(status_code) + "\n")
            f.write(str(return_data["body"]["error_text"]))
    return data_id, status_code


def download_metget_data(data_id, endpoint, apikey, sleeptime, max_wait):
    from datetime import datetime, timedelta

    # ...Status check
    headers = {"x-api-key": apikey}
    request_json = {"request": data_id}

    # ...Wait time
    end_time = datetime.utcnow() + timedelta(hours=max_wait)

    # ...Wait for request data to appear
    tries = 0
    data_ready = False
    status = None
    print("Waiting for request id: ", data_id, flush=True)
    while datetime.utcnow() <= end_time:
        tries += 1
        try:
            print(
                "["
                + datetime.utcnow().strftime("%Y-%m-%d %H:%M:%S UTC")
                + "]: Checking request status...(n="
                + str(tries)
                + "): ",
                flush=True,
                end="",
            )
            response = requests.post(
                endpoint + "/check", headers=headers, json=request_json
            )
            json_response = json.loads(response.text)
            status = json_response["body"]["status"]
            data_url = json_response["body"]["destination"]
            print(status, flush=True)
            if status == "completed":
                # ...Parse the return to get data
                data_ready = True
                flist_url = data_url + "/filelist.json"
                u = requests.get(flist_url)
                if u.status_code == 200:
                    return_data = json.loads(u.text)
                    with open("filelist.json", "w") as jsonfile:
                        jsonfile.write(
                            json.dumps(return_data, indent=2, sort_keys=True)
                        )
                    break
            elif status == "error":
                print("Request could not be completed")
                return
            else:
                time.sleep(sleeptime)
                continue
        except KeyboardInterrupt:
            print("[ERROR]: Process was ended by the user")
            raise

    # ...Download files
    if data_ready:
        file_list = return_data["output_files"]
        for f in file_list:
            print("Getting file: " + f, flush=True)
            with requests.get(data_url + "/" + f, stream=True) as r:
                r.raise_for_status()
                with open(f, "wb") as wind_file:
                    for chunk in r.iter_content(chunk_size=8192):
                        wind_file.write(chunk)
    else:
        if status == "restore":
            print(
                "[WARNING]: Data for request "
                + data_id
                + " did not become ready before the max-wait time expired. You can rerun and ask for this request by id"
            )
        elif status == "running":
            print(
                "[WARNING]: Data for request "
                + data_id
                + " is still being constructed when the max-wait time expired. Please check on it later"
            )
        elif status == "queued":
            print(
                "[WARNING]: Data for request "
                + data_id
                + " is still queued. If this does not change soon, please contact an administrator"
            )
        else:
            print("[ERROR]: Data has not become available due to an unknown error")
        return


def main():
    import socket
    import json
    import getpass

    mlist = str()
    for m in AVAILABLE_MODELS.keys():
        if len(mlist) == 0:
            mlist += m
        else:
            mlist += ", " + m

    p = argparse.ArgumentParser(description="Make a request to MetGet")
    p.add_argument(
        "--domain",
        help="Wind domain specification. Model may be any of ["
        + mlist
        + "]. Resolution and corners are decimal degrees"
        " For HWRF/COAMPS, the model can be listed as 'hwrf-[stormname]' or 'coamps-[stormname]'",
        nargs=6,
        metavar=("model", "resolution", "x0", "y0", "x1", "y1"),
        action="append",
    )
    p.add_argument(
        "--start",
        help="Start time",
        type=datetime.fromisoformat,
        metavar="YYYY-MM-DD hh:mm",
    )
    p.add_argument(
        "--end",
        help="End time",
        type=datetime.fromisoformat,
        metavar="YYYY-MM-DD hh:mm",
    )
    p.add_argument(
        "--timestep", help="Time step of winds in seconds", metavar="dt", type=int
    )
    p.add_argument(
        "--analysis", help="Generate analysis wind fields", action="store_true"
    )
    p.add_argument(
        "--multiple_forecasts",
        help="Allow the use of multiple forecast wind fields",
        action="store_true",
    )
    p.add_argument("--output", help="Base name of output data", type=str, metavar="s")
    p.add_argument("--apikey", help="API key for access to MetGet", type=str)
    p.add_argument("--endpoint", help="MetGet API endpoint", type=str)
    p.add_argument(
        "--format",
        help="Output format (adcirc-ascii, adcirc-netcdf, hec-netcdf, delft3d)",
        metavar="f",
        default="owi-ascii",
    )
    p.add_argument(
        "--variable",
        help="Variable to request from MetGet (wind_pressure, rain, humidity, temperature, ice)",
        metavar="v",
        default="wind_pressure",
    )
    p.add_argument(
        "--check-interval",
        help="Time between status checks (default=30s)",
        metavar="t",
        default=30,
        type=float,
    )
    p.add_argument(
        "--max-wait",
        help="Maximum wait time for the request to complete in hours (default=24)",
        metavar="h",
        default=24,
        type=float,
    )
    p.add_argument(
        "--strict",
        action="store_true",
        help="Do not allow MetGet to make due with what it has. Force the response to match the request",
    )
    p.add_argument(
        "--backfill",
        action="store_true",
        help="Backfill data from lower priority domains",
        default=False,
    )
    # p.add_argument("--epsg", help="Coordinate system of the specified domain and output data (default: 4326)", required=False, metavar="#", type=int, default=4326)
    p.add_argument("--dryrun", help="Perform dry run only", action="store_true")
    p.add_argument(
        "--request",
        help="Check on and download specified request id",
        type=str,
        metavar="request_id",
    )

    args = p.parse_args()
    args.epsg = 4326

    if not args.endpoint:
        if not "METGET_ENDPOINT" in os.environ:
            raise RuntimeError("No endpoint found.")
        else:
            endpoint = os.environ["METGET_ENDPOINT"]
    else:
        endpoint = args.endpoint

    if not args.apikey:
        if not "METGET_API_KEY" in os.environ:
            raise RuntimeError("No API key was found.")
        else:
            apikey = os.environ["METGET_API_KEY"]
    else:
        apikey = args.apikey

    # ...Check for required arguments
    if not args.request:
        if not args.start:
            print("[ERROR]: Must provide '--start'")
            exit(1)
        if not args.end:
            print("[ERROR]: Must provide '--end'")
            exit(1)
        if not args.timestep:
            print("[ERROR]: Must provice '--timestep'")
            exit(1)
        if not args.output:
            print("[ERROR]: Must provide '--output'")
            exit(1)

        # ...Building the request
        domains = []
        idx = 0
        for d in args.domain:
            j = parse_domain_data(d, idx)
            domains.append(j)
            idx += 1

        if args.format not in AVAILABLE_FORMATS:
            print("ERROR: Invalid output format selected")
            exit(1)

        if args.variable not in AVAILABLE_VARIABLES:
            print("ERROR: Invalid variable selected")
            exit(1)

        request_from = getpass.getuser() + "." + socket.gethostname()
        request_data = {
            "version": "0.0.1",
            "creator": request_from,
            "background_pressure": 1013.0,
            "backfill": True,
            "nowcast": args.analysis,
            "multiple_forecasts": args.multiple_forecasts,
            "start_date": str(args.start),
            "end_date": str(args.end),
            "format": args.format,
            "data_type": args.variable,
            "time_step": args.timestep,
            "domains": domains,
            "compression": False,
            "epsg": args.epsg,
            "filename": args.output,
        }
        if args.strict:
            request_data["strict"] = True
        if args.dryrun:
            request_data["dry_run"] = True

        data_id, status_code = make_metget_request(endpoint, apikey, request_data)
        if not args.dryrun and status_code == 200:
            download_metget_data(
                data_id, endpoint, apikey, args.check_interval, args.max_wait
            )
        else:
            print(status_code)

    else:
        download_metget_data(
            args.request, endpoint, apikey, args.check_interval, args.max_wait
        )


if __name__ == "__main__":
    main()
