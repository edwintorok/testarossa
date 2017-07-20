#!/bin/sh
set -x
set -e
echo "Joining node $1"
pcs cluster node add $1 --start --debug
#--wait=60
while ! (crm_mon -1 | grep Online | grep $1); do
    echo "Waiting for $1 to start"
    sleep 1
done
#echo "OK"
