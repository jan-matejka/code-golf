build::

  podman build -t localhost/postgres ./

run::

  podman run --env "POSTGRES_HOST_AUTH_METHOD=trust" --name postgres -p 5432:5432 localhost/postgres

connect client (from host)::

  psql -U postgres -h localhost
