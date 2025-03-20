#ifndef MQ_SYSTEM_ABC_HPP
#define MQ_SYSTEM_ABC_HPP

using namespace std;

namespace mqs::abc {

class sender {
public:
  virtual void send(int) = 0;
  virtual ~sender() {}
};

class mq {
public:
  virtual unique_ptr<sender> connect() = 0;
  virtual ~mq() {}
};

}

#endif
