#include <stdio.h>
#include <sys/types.h>

int main() {
  int pid = fork();
  pid = fork();
  pid = fork();
  printf("pid: %d\n",pid);
}
