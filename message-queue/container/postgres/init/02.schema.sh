#! /usr/bin/env sh

psql -U mq -f /docker-entrypoint-initdb.d/02.schema.sql.in
