.DEFAULT_GOAL := image

.PHONY: image
image:

	podman build -t localhost/mq/postgres ./

.PHONY: schema
schema:

	psql -U postgres -h localhost -f role.sql
	psql -U mq -h localhost -f schema.sql
