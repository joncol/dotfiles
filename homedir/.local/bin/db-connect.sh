#!/usr/bin/env bash
set -o errexit -o pipefail -o noclobber -o nounset

# See: https://t.ly/wBrJd for info about `getopt` usage.

! getopt --test > /dev/null
if [[ ${PIPESTATUS[0]} -ne 4 ]]; then
    echo 'I’m sorry, `getopt --test` failed in this environment.'
    exit 1
fi

OPTIONS=v,p:,t
LONGOPTS=verbose,port:,tunnel-only

! PARSED=$(getopt --options=$OPTIONS --longoptions=$LONGOPTS --name "$0" -- "$@")
if [[ ${PIPESTATUS[0]} -ne 0 ]]; then
    exit 2
fi

eval set -- "$PARSED"

DB_PORT=5432
LOCAL_PORT=9004
TUNNEL_ONLY=false
VERBOSE=false

while true; do
    case "$1" in
        -p|--port)
            LOCAL_PORT=$2
            shift 2
            ;;
        -t|--tunnel-only)
            TUNNEL_ONLY=true
            shift
            ;;
        -v|--verbose)
            VERBOSE=true
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

SERVICE=$1
SERVICE_SNAKE_CASE=$(echo $SERVICE | sed 's/-/_/g')
ENVIRONMENT=$2
DB_DATABASE=$(printf "%s_%s" $SERVICE_SNAKE_CASE $ENVIRONMENT)
DB_HOST=$SERVICE-$ENVIRONMENT-db.czldyizapuwt.eu-central-1.rds.amazonaws.com
TUNNEL_HOST="bastion.zimpler.net"

DB_USER=$(ze-read $SERVICE $ENVIRONMENT DB_USER | awk '{print $4}')
if [ -z "$DB_USER" ]; then
    DB_USER=$(ze-read $SERVICE $ENVIRONMENT POSTGRES_USER | awk '{print $4}')
fi

if [ -z "$DB_USER" ]; then
    DB_USER=${SERVICE}_$ENVIRONMENT
fi

DB_PASSWORD=$(ze-read $SERVICE $ENVIRONMENT DB_PASSWORD | awk '{print $4}')
if [ -z "$DB_PASSWORD" ]; then
    DB_PASSWORD=$(ze-read $SERVICE $ENVIRONMENT DB_PASS | \
                      awk '{print $4}')
fi
if [ -z "$DB_PASSWORD" ]; then
    DB_PASSWORD=$(ze-read $SERVICE $ENVIRONMENT POSTGRES_PASSWORD | \
                      awk '{print $4}')
fi

if [ -z "$DB_PASSWORD" ]; then
    echo "Couldn't find DB_PASSWORD or POSTGRES_PASSWORD in parameter store"
    exit 1
fi

if [ $VERBOSE = true ]; then
    echo "Local port:    $LOCAL_PORT"
    echo "Database host: $DB_HOST"
    echo "Database name: $DB_DATABASE"
    echo "Username:      $DB_DATABASE"
    echo "Password:      $DB_PASSWORD"
    echo ""
fi

if [ $TUNNEL_ONLY = true ]; then
    echo "Setting up tunnel to database host..."
    ssh -NL $LOCAL_PORT:$DB_HOST:$DB_PORT $TUNNEL_HOST
else
    ssh -fL $LOCAL_PORT:$DB_HOST:$DB_PORT $TUNNEL_HOST sleep 10
    PGHOST=localhost PGDATABASE=$DB_DATABASE PGUSER=$DB_USER \
          PGPASSWORD=$DB_PASSWORD PGPORT=$LOCAL_PORT \
          psql
fi
