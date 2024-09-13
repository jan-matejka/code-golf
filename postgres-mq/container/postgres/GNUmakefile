.PHONY: schema
schema:

	psql -U postgres -h localhost -f role.sql
	psql -U mq -h localhost -f schema.sql
