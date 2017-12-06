#!/bin/bash

. /etc/xensource-inventory

echo -n $INSTALLATION_UUID,
sudo xe pif-list device=eth0 host-uuid=$INSTALLATION_UUID params=IP --minimal

