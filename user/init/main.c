#include <stdarg.h>
#include <stdio.h>
#include <sys/wait.h>
#include <unistd.h>

int main() {
  while (1) {
    printf("\x1b[2J\x1b[H");
    printf("Welcome to zigux!\n");
    printf("You can find the source code at https://github.com/czapek1337/zigux\n\n");

    int pid = fork();

    if (pid == 0) {
      char *argv[] = {"/usr/bin/bash", NULL};
      char *envp[] = {"TERM=xterm-256color", "HOME=/root", "PATH=/bin:/usr/bin:/usr/local/bin", NULL};

      execve("/usr/bin/bash", argv, envp);
    } else {
      int status;

      waitpid(pid, &status, 0);
    }
  }

  return 0;
}
