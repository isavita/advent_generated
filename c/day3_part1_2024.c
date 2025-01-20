
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

int main() {
    FILE *fp;
    char *line = NULL;
    size_t len = 0;
    ssize_t read;
    long long totalSum = 0;

    fp = fopen("input.txt", "r");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    while ((read = getline(&line, &len, fp)) != -1) {
        char *p = line;
        while ((p = strstr(p, "mul(")) != NULL) {
            p += 4;
            int x = 0, y = 0;
            while (isdigit(*p)) {
                x = x * 10 + (*p - '0');
                p++;
            }
            if (*p != ',') {
                continue;
            }
            p++;
            while (isdigit(*p)) {
                y = y * 10 + (*p - '0');
                 p++;
            }
             if (*p != ')') {
                continue;
            }
            totalSum += (long long)x * y;
        }
    }

    printf("%lld\n", totalSum);

    if (line)
        free(line);
    fclose(fp);
    return 0;
}
