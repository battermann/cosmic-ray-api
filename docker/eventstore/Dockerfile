FROM postgres:12.1

COPY create-db.sh /docker-entrypoint-initdb.d/
COPY eventstore.sql /docker-entrypoint-initdb.d/
