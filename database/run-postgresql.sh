#! /usr/bin/env bash

export PGDATA=$(git rev-parse --show-toplevel)/tmp/postgres
export PGDATABASE=dars
export PGHOST=localhost
export PGPORT=5444
export PGUSER=postgres

if [ ! -d "$PGDATA" ]; then
  pg_ctl initdb -o "-U $PGUSER"
  cp database/postgresql.conf "$PGDATA"
fi

exec postgres -k "$PGDATA" -d "$PGDATABASE" -D "$PGDATA" -h "$PGHOST" -p "$PGPORT"
