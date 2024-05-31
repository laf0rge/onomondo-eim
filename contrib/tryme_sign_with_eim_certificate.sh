#!/bin/bash

MESSAGE=message.bin
#PRIVATE_KEY=/home/user/work/eIM_IPAd/git/onomondo-eim/onomondo_eim/config/sample_eim_cert_brainpool.key
PRIVATE_KEY=/home/user/work/eIM_IPAd/git/onomondo-eim/onomondo_eim/config/sample_eim_cert_NIST.key

PUBLIC_KEY=pubkey.tmp
SIGNATURE=$MESSAGE.signature.bin

echo "========== KEY =========="
echo $PRIVATE_KEY
openssl ec -in $PRIVATE_KEY -noout -text
openssl ec -in $PRIVATE_KEY -pubout -out $PUBLIC_KEY
echo ""

echo "========== SIGN MESSAGE =========="
echo "Message:"
hexdump -C $MESSAGE
#openssl dgst -sha256 -sign $PRIVATE_KEY $MESSAGE > $SIGNATURE
openssl dgst -sha256 -sign $PRIVATE_KEY $MESSAGE > $SIGNATURE
echo "Signature:"
hexdump -C $SIGNATURE
echo ""

echo "========== VERIFY MESSAGE USING SIGNATURE =========="
openssl dgst -sha256 -verify $PUBLIC_KEY -signature $SIGNATURE $MESSAGE

rm $PUBLIC_KEY
rm $SIGNATURE
