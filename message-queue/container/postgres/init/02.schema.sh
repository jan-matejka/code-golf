#! /usr/bin/env sh

set -e

psql -U mq -f /docker-entrypoint-initdb.d/02.schema.sql.in
psql -U mq -f /docker-entrypoint-initdb.d/03.results.sql.in
