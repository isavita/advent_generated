
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

uint64_t nextSecret(uint64_t s) {
    s ^= s * 64;
    s &= 0xFFFFFF;
    s ^= s / 32;
    s &= 0xFFFFFF;
    s ^= s * 2048;
    s &= 0xFFFFFF;
    return s;
}

int main() {
    FILE *fp;
    char *line = NULL;
    size_t len = 0;
    ssize_t read;
    uint64_t total = 0;

    fp = fopen("input.txt", "r");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    while ((read = getline(&line, &len, fp)) != -1) {
        if (read > 1) {
            uint64_t buyer = strtoull(line, NULL, 10);
             uint64_t s = buyer;
            for (int i = 0; i < 2000; i++) {
                s = nextSecret(s);
            }
            total += s;
        }
    }

    fclose(fp);
    if (line)
        free(line);

    printf("%lu\n", total);
    return 0;
}
