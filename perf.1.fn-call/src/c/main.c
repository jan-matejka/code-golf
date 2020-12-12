#include <math.h>
#include <time.h>
#include <stdio.h>

void noop() {
}

int main(void) {
  if (CLOCKS_PER_SEC != pow(10, 6)) {
    printf("unexpectred CLOCKS_PER_SEC\n");
    return 1;
  }

  double number = pow(10, 9);

  clock_t start = clock();
  if (start == -1) {
    printf("bad clock()\n");
    return 1;
  }

  for(int i=0; i<number; i++) {
    noop();
  }

  clock_t end = clock();
  if (end == -1) {
    printf("bad clock()\n");
    return 1;
  }

  double took = (double) (end-start);
  double took1 = took / number;

  printf("%0.3f nsec\n", took1 * 1000);
}
