The client python script is herein as well as a movie of the wind fields that was run through the the adcirc forecast system part of the 00Z cycle corresponding
to Tropical Storm Alex (al012022).

The only additional information you need to provide is the URL of the API and your API key. The URL of the API is: https://api.metget.zachcobell.com and I've sent the API key separately. There are two ways to provide this info to the client. The first is via command line arguments:
```
--endpoint [URL] --apikey [APIKEY]
```
or, you can provide them as environment variables:
```
export METGET_ENDPOINT=[URL]
export METGET_API_KEY=[APIKEY]
```
Either one is fine and the client will complain if you don't provide one way or the other. The only dependency of the client is the requests python library. I think it is standard in some installations, but if it isn't you can install it with pip or conda using pip3 install requests
```
./get_metget_data.py --domain coamps-01L 0.1 -98 5 -58 50 --start '2022-06-04 00:00' --end '2022-06-09 06:00' --timestep 3600 --strict --backfill --output adcirc_coamps_forecast_2022060400-2022060906`
```
