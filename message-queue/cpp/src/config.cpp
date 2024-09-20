#ifndef CONFIG_CPP
#define CONFIG_CPP
#include "./log.cpp"

class Config {
public:
  int duration = 3;
  int power = 0;

  Config() {
    duration = igetenv("DURATION", 3);
    power = igetenv("POWER", 0);
  }

  string str() {
    stringstream s;
    s << "duration=" << duration << " power=" << power;
    return s.str();
  }
};

#endif
