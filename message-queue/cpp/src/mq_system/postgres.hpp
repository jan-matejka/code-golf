#ifndef MQ_SYSTEM_POSTGRES_HPP
#define MQ_SYSTEM_POSTGRES_HPP

#include <pqxx/pqxx>

#include "../config.hpp"
#include "abc.hpp"

using namespace pqxx;
using namespace std;

namespace mqs::postgres {

class sender : public mqs::abc::sender {
  unique_ptr<connection> conn;
public:
  sender(unique_ptr<connection>);
  virtual ~sender();
  virtual void send(int);
};

class mq : public mqs::abc::mq {
  Config cg;
public:
  mq(Config&);
  virtual unique_ptr<mqs::abc::sender> connect();
  virtual ~mq() {};
};

}

#endif
