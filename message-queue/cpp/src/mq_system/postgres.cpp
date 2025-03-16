#ifndef MQ_SYSTEM_POSTGRES_CPP
#define MQ_SYSTEM_POSTGRES_CPP

#include "./postgres.hpp"

namespace mqs::postgres {

sender::sender(unique_ptr<connection> conn) : conn(move(conn)) {}

void sender::send(int i) {
  work tx(*conn);
  tx.exec("insert into queue (data) values (" + tx.quote(i) + ")");
  tx.commit();
}

sender::~sender() {
  try {
    conn->disconnect();
  }catch(...) {}
}

mq::mq(Config &cg) : cg(cg) {}

unique_ptr<mqs::abc::sender> mq::connect() {
  unique_ptr<connection> conn = make_unique<connection>(
    "postgres://mq@localhost/mq"
  );
  return make_unique<sender>(move(conn));
}

}
#endif
