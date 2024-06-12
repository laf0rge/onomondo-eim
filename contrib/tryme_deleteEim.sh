#!/bin/bash
. ./tryme.cfg
EIM_ID="eIM2"
JSON='{ "eidValue" : "'$EID'", "order" : { "eco" : [ { "deleteEim" : { "eimId" : "'$EIM_ID'" } } ] } }'
RC=`./restop.py -c -f eco -j "$JSON"`
echo $RC

echo "---------------------------------------8<---------------------------------------"
RESOURCE_ID=`echo $RC | cut -d '/' -f 6`
echo "ResourceId =" $RESOURCE_ID
./tryme_lookup.sh eco $RESOURCE_ID
