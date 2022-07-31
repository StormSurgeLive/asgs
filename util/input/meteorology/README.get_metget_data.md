General API documentation is available on SwaggerHub:
```
https://app.swaggerhub.com/apis/zcobell-dev/MetGet/0.0.1
```
The client python script is herein as well as a movie of the wind fields that was run through the the adcirc forecast system part of the 00Z cycle corresponding to Tropical Storm Alex (al012022).

The only additional information you need to provide is the URL of the API and your API key. The URL of the API is: https://api.metget.zachcobell.com and I've sent the API key separately. There are two ways to provide this info to the client. The first is via command line arguments:
```
--endpoint [URL] --apikey [APIKEY]
```
or, you can provide them as environment variables:
```
export METGET_ENDPOINT=[URL]
export METGET_API_KEY=[APIKEY]
```
Either one is fine and the client will complain if you don't provide one way or the other. The only dependency of the client is the requests python library. I think it is standard in some installations, but if it isn't you can install it with pip or conda using pip3 install requests.

The status of the files available can be found via the status endpoint:
```
URL="https://api.metget.zachcobell.com/status"
curl  -s -H "Content-Type: application/json" -H "x-api-key: $METGET_API_KEY" "$URL" | json_pp > metget.status.json
```
The `--dry_run` argument can be added to form the request but not submit it. For example:

```
./get_metget_data.py --dryrun --apikey $METGET_API_KEY --endpoint "https://api.metget.zachcobell.com" --domain coamps-01L 0.1 -98 5 -58 50 --start '2022-06-04 00:00' --end '2022-06-09 06:00' --timestep 3600 --strict --backfill --output adcirc_coamps_forecast_2022060400-2022060906
```
Sample JSON from the build request above:
```
{
        "version" : "0.0.1",
        "creator" : "jason.kitt",
        "background_pressure": 1013.0,
        "backfill": true,
        "nowcast": false,
        "multiple_forecasts": false,
        "start_date": "2022-06-04 00:00:00",
        "end_date": "2022-06-09 06:00:00",
        "format": "owi-ascii",
        "data_type": "wind_pressure",
        "time_step": 3600,
        "domains": [
                {
                        "name": "coamps-tc-01L",
                        "service": "coamps-tc",
                        "storm": "01L",
                        "x_init": -98.0,
                        "y_init": 5.0,
                        "x_end": -58.0,
                        "y_end": 50.0,
                        "di": 0.1,
                        "dj": 0.1,
                        "level": 0
                }
        ],
        "compression": false,
        "epsg": 4326,
        "filename": "adcirc_coamps_forecast_2022060400-2022060906",
        "strict": true,
        "dry_run": true
}
```
Sample JSON for a build request (from the SwaggerHub site linked above):
```
{
  "version": "string",
  "start_date": "string",
  "end_date": "string",
  "time_step": 0,
  "background_pressure": 1013,
  "null_value": -9999,
  "creator": "string",
  "filename": "string",
  "nowcast": true,
  "multiple_forecasts": true,
  "format": "adcirc-ascii",
  "epsg": 4326,
  "backfill": false,
  "domains": [
    {
      "name": "string",
      "service": "string",
      "level": 0,
      "x_init": 0,
      "y_init": 0,
      "di": 0,
      "dj": 0,
      "ni": 0,
      "nj": 0,
      "rotation": 0,
      "x_end": 0,
      "y_end": 0
    },
    {
      "name": "string",
      "service": "string",
      "level": 0,
      "x_init": 0,
      "y_init": 0,
      "di": 0,
      "dj": 0,
      "x_end": 0,
      "y_end": 0
    },
    {
      "name": "string",
      "service": "string",
      "predefined_name": "string"
    }
  ]
}
```
