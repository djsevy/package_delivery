-module(riak_api).
-export([get_package/1, get_vehicle/1, get_eta/1]).
-export([put_vehicle_location/4]).


get_vehicle(_Vehicle_id) ->
	ok.

get_package(_Package_id) ->
	ok.

get_eta(_Package_id) ->
	ok.

put_vehicle_location(_Vehicle_id, _Lat, _Lon, _Time) ->
	ok.