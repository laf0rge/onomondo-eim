#!/bin/bash
. ./tryme.cfg

JSON='{ "eidValue" : "'$EID'", "order" : { "psmo" : [ { "delete" : { "iccid" : "'$ICCID'" } } ] } }'
RC=`./restop.py -c -f psmo -j "$JSON"`
echo $RC

echo "---------------------------------------8<---------------------------------------"
RESOURCE_ID=`echo $RC | cut -d '/' -f 6`
echo "ResourceId =" $RESOURCE_ID
./tryme_lookup.sh psmo $RESOURCE_ID

