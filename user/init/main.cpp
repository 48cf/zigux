#include <stdio.h>
#include <unistd.h>

int main(int argc, char *argv[], char *envp[]) {
    printf("Hello, world!\n");
    printf("Argument count: %d\n", argc);

    for (int i = 0; i < argc; i++) {
        printf("  %s\n", argv[i]);
    }

    printf("Environment variables:\n");

    for (int i = 0; envp[i] != nullptr; i++) {
        printf("  %s\n", envp[i]);
    }

    return 0;
}
