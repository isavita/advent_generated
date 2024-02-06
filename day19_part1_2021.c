
#include <stdio.h>

int main() {
    FILE *file;
    file = fopen("input.txt", "r");

    if (file) {
        char line[255];
        while (fgets(line, sizeof(line), file)) {
            printf("%s", line);
        }
        fclose(file);
    }

    return 0;
}
