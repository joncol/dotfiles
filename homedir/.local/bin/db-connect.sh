#!/usr/bin/env bash

if [ "$#" -ne 2 ]; then
    echo "usage: db-connect.sh SERVICE ENVIRONMENT"
    exit 1
fi

set -euo pipefail
TUNNEL_HOST="$USER@bastion.zimpler.net"

DB_DATABASE=$1_$2
DB_HOST=$1-$2-db-replica.czldyizapuwt.eu-central-1.rds.amazonaws.com

DB_PASSWORD=$(ze-read $1 $2 DB_PASSWORD | awk '{print $4}')
if [ -z "$DB_PASSWORD" ]; then
    DB_PASSWORD=$(ze-read $1 $2 POSTGRES_PASSWORD | awk '{print $4}')
fi

if [ -z "$DB_PASSWORD" ]; then
    echo "Couldn't find DB_PASSWORD or POSTGRES_PASSWORD in parameter store"
    exit 1
fi

DB_PORT=5432
DB_USER=$1_$2
LOCAL_PORT=9004

ssh -f -L $LOCAL_PORT:$DB_HOST:$DB_PORT $TUNNEL_HOST sleep 10
PGHOST=localhost PGDATABASE=$DB_DATABASE PGUSER=$DB_USER PGPASSWORD=$DB_PASSWORD PGPORT=$LOCAL_PORT psql
