#! /usr/bin/env sh

set -e

psql -a -U mq -f /docker-entrypoint-initdb.d/02.schema.sql.in
psql -a -U mq -f /docker-entrypoint-initdb.d/03.results.sql.in
