#!/usr/bin/env bash
set -o errexit -o pipefail -o noclobber -o nounset

# See: https://t.ly/wBrJd for info about `getopt` usage.

! getopt --test > /dev/null
if [[ ${PIPESTATUS[0]} -ne 4 ]]; then
    echo 'I’m sorry, `getopt --test` failed in this environment.'
    exit 1
fi

options=v
longopts=verbose

! parsed=$(getopt --options=$options --longoptions=$longopts --name "$0" -- "$@")
if [[ ${PIPESTATUS[0]} -ne 0 ]]; then
    exit 2
fi

eval set -- "$parsed"

db_port=5432
verbose=false

while true; do
    case "$1" in
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
service_snake_case=${service//-/_}
environment=$2

db_host=$(ze-read -v $service $environment shared POSTGRES_HOST)
db_host=${db_host:-$(ze-read -v $service $environment shared DB_HOST)}
db_host=${db_host:-$service-$environment-db.czldyizapuwt.eu-central-1.rds.amazonaws.com}

db_database=$(ze-read -v $service $environment shared POSTGRES_DATABASE)
db_database=${db_database:-"${service_snake_case}_$environment"}

db_user=$(ze-read -v $service $environment shared POSTGRES_USER)
db_user=${db_user:-$(ze-read -v $service $environment shared DB_USER)}
db_user=${db_user:-${service}_$environment}

db_password=$(ze-read -v $service $environment shared DB_PASSWORD)
db_password=${db_password:-$(ze-read -v $service $environment shared DB_PASS)}
db_password=${db_password:-$(ze-read -v $service $environment shared \
    POSTGRES_PASSWORD)}

if [ $verbose = true ]; then
    echo "Database host: $db_host"
    echo "Database name: $db_database"
    echo "Username:      $db_user"
    echo "Password:      ${db_password:?couldn\'t find DB_PASS or POSTGRES_PASSWORD in parameter store}"
    echo ""
fi

PGHOST=$db_host PGDATABASE=$db_database PGUSER=$db_user \
    PGPASSWORD=$db_password PGPORT=$db_port \
    psql
