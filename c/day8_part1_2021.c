
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
    FILE *file;
    char line[256];
    int count = 0;

    file = fopen("input.txt", "r");
    if (!file) {
        perror("Unable to open file");
        return EXIT_FAILURE;
    }

    while (fgets(line, sizeof(line), file)) {
        char *output = strchr(line, '|');
        if (output && *(++output)) {
            char *token = strtok(output, " ");
            while (token) {
                int len = strlen(token);
                if (token[len - 1] == '\n') {
                    token[--len] = '\0';
                }
                if (len == 2 || len == 4 || len == 3 || len == 7) {
                    count++;
                }
                token = strtok(NULL, " ");
            }
        }
    }

    fclose(file);
    printf("%d\n", count);

    return 0;
}
