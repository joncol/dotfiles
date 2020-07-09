#!/usr/bin/env bash
set -o errexit -o pipefail -o noclobber -o nounset

# See: https://t.ly/wBrJd for info about `getopt` usage.

! getopt --test > /dev/null
if [[ ${PIPESTATUS[0]} -ne 4 ]]; then
    echo 'I’m sorry, `getopt --test` failed in this environment.'
    exit 1
fi

options=v,p:,t
longopts=verbose,port:,tunnel-only

! parsed=$(getopt --options=$options --longoptions=$longopts --name "$0" -- "$@")
if [[ ${PIPESTATUS[0]} -ne 0 ]]; then
    exit 2
fi

eval set -- "$parsed"

db_port=5432
local_port=9004
tunnel_only=false
verbose=false

while true; do
    case "$1" in
        -p|--port)
            local_port=$2
            shift 2
            ;;
        -t|--tunnel-only)
            tunnel_only=true
            shift
            ;;
        -v|--verbose)
            verbose=true
            shift
            ;;
        --)
            shift
            break
            ;;
        *)
            echo "Programming error $1"
            exit 3
            ;;
    esac
done

if [[ $# -ne 2 ]]; then
    echo "$(basename $0): SERVICE ENVIRONMENT"
    exit 4
fi

service=$1
service_snake_case=$(echo $service | sed 's/-/_/g')
environment=$2
tunnel_host="bastion.zimpler.net"

db_host=$(ze-read $service $environment POSTGRES_HOST | awk '{print $4}')
if [ -z "$db_host" ]; then
    db_host=$(ze-read $service $environment db_host | awk '{print $4}')
fi
if [ -z "$db_host" ]; then
    db_host=$service-$environment-db.czldyizapuwt.eu-central-1.rds.amazonaws.com
fi

db_database=$(ze-read $service $environment POSTGRES_DATABASE | \
                  awk '{print $4}')
if [ -z "$db_database" ]; then
    db_database=$(printf "%s_%s" $service_snake_case $environment)
fi

db_user=$(ze-read $service $environment POSTGRES_USER | awk '{print $4}')
if [ -z "$db_user" ]; then
    db_user=$(ze-read $service $environment db_user | awk '{print $4}')
fi

if [ -z "$db_user" ]; then
    db_user=${service}_$environment
fi

db_password=$(ze-read $service $environment db_password | awk '{print $4}')
if [ -z "$db_password" ]; then
    db_password=$(ze-read $service $environment DB_PASS | \
                      awk '{print $4}')
fi
if [ -z "$db_password" ]; then
    db_password=$(ze-read $service $environment POSTGRES_PASSWORD | \
                      awk '{print $4}')
fi

if [ -z "$db_password" ]; then
    echo "Couldn't find db_password or POSTGRES_PASSWORD in parameter store"
    exit 1
fi

if [ $verbose = true ]; then
    echo "Local port:    $local_port"
    echo "Database host: $db_host"
    echo "Database name: $db_database"
    echo "Username:      $db_database"
    echo "Password:      $db_password"
    echo ""
fi

if [ $tunnel_only = true ]; then
    echo "Setting up tunnel to database host..."
    ssh -NL $local_port:$db_host:$db_port $tunnel_host
else
    ssh -fL $local_port:$db_host:$db_port $tunnel_host sleep 10
    PGHOST=localhost PGDATABASE=$db_database PGUSER=$db_user \
          PGPASSWORD=$db_password PGPORT=$local_port \
          psql
fi
