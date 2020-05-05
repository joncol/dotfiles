#!/usr/bin/env bash

# Can be run like: `./kafka-ips.sh tjy1v0 production`

clusterId=${1:-dx8wx2}
suffix=$clusterId.c3.kafka.eu-central-1.amazonaws.com
env=kafka-${2:-staging}
zook=''
for i in $(seq 1 3); do
    dnsR=z-$i.$env.$suffix
    echo $dnsR
    ip=$(dig +short $dnsR)
    echo $ip
    zook="$zook$ip,"
done
echo "Zookeeper: $(echo $zook|sed 's/.$//')"
brokers=''
for i in $(seq 1 3); do
    dnsR=b-$i.$env.$suffix
    echo $dnsR
    ip=$(dig +short $dnsR)
    brokers="$brokers$ip,"
done
echo "Broker: $(echo $brokers|sed 's/.$//')"
