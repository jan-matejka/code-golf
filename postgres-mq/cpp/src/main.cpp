#include <iostream>
#include <pqxx/pqxx>

using namespace std;
using namespace pqxx;

void insert(connection &C, int i) {
  work W(C);
  W.exec("insert into queue (data) values (" + W.quote(i) + ")");
  W.commit();
}

int main(void) {
  try {
      connection C("postgres://mq@localhost/mq");
      if (!C.is_open()) {
         cerr << "Can't open database" << endl;
         return 1;
      }

      insert(C, 1);

      C.disconnect ();
   } catch (const std::exception &e) {
      cerr << e.what() << std::endl;
      return 1;
   }

   return 0;
}
