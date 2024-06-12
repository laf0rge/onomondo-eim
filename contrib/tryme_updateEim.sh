#!/bin/bash
. ./tryme.cfg

JSON='{ "eidValue" : "'$EID'", "order" : { "eco" : [ { "updateEim" : { "eimConfigurationData" : "3017800465494D32810F3132372E302E302E34323A39303030" } } ] } }'
RC=`./restop.py -c -f eco -j "$JSON"`
echo $RC

echo "---------------------------------------8<---------------------------------------"
RESOURCE_ID=`echo $RC | cut -d '/' -f 6`
echo "ResourceId =" $RESOURCE_ID
./tryme_lookup.sh eco $RESOURCE_ID
