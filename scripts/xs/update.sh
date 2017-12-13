#!/bin/bash

rm /etc/sysconfig/network-scripts/ifcfg-eth0

systemctl enable xapi-clusterd.service
systemctl enable forkexecd.service
systemctl enable xcp-networkd.service 2>/dev/null
systemctl enable genptoken.service 2>/dev/null
systemctl enable squeezed.service 2>/dev/null
systemctl enable xcp-rrdd.service 2>/dev/null
systemctl enable xenopsd-xc.service 2>/dev/null
systemctl enable xapi.service 2>/dev/null
systemctl enable xapi-domains.service 2>/dev/null
