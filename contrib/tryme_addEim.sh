#!/bin/bash
. ./tryme.cfg

JSON='{ "eidValue" : "'$EID'", "order" : { "eco" : [ { "addEim" : { "eimConfigurationData" : "3079800465494D32810E3132372E302E302E313A383030308301018401FFA55BA059301306072A8648CE3D020106082A8648CE3D03010703420004FE584A6F450459574AECA195D0299737F74C89BA2D36DF9286EC25D973037A0FBA70D14DF3E1F7D0A305E57B95B731C4DE218D2D7F9F22113ED5D18C2E3DDF1C" } } ] } }'
RC=`./restop.py -c -f eco -j "$JSON"`
echo $RC

echo "---------------------------------------8<---------------------------------------"
RESOURCE_ID=`echo $RC | cut -d '/' -f 6`
echo "ResourceId =" $RESOURCE_ID
./tryme_lookup.sh eco $RESOURCE_ID
