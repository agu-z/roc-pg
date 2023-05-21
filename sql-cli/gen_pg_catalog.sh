#!/usr/bin/env bash 

## Generate roc-sql for pg_catalog using roc-sql

set -u

SRC="$(dirname $0)/src"

roc build $SRC/main.roc
chmod +X $SRC/roc-sql

$SRC/roc-sql                  \
    -h localhost              \
    -p 5432                   \
    -U postgres               \
    -d postgres               \
    --schema pg_catalog       \
    > $SRC/PgCatalog.new.roc

mv $SRC/PgCatalog.new.roc $SRC/PgCatalog.roc