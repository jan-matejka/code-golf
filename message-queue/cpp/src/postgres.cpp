#include "postgres.hpp"

Postgres::Postgres(Config &c)
: conn(connection("postgres://mq@localhost/mq")) {
}
