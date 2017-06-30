#!/bin/bash
set -e
vagrant up cluster{1,2,3}


echo "Cluster auth"
echo "============"

vagrant ssh cluster1 -c "sudo pcs cluster auth -u hacluster -p mysecurepassword cluster1 cluster2 cluster3"


echo "Cluster setup"
echo "============="

vagrant ssh cluster1 -c "sudo pcs cluster setup --name cluster cluster1 cluster2 cluster3"


echo "Cluster start"
echo "============="

vagrant ssh cluster1 -c "sudo pcs cluster start --all"

