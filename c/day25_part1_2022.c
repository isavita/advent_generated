
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

long long fromSnafu(const char *s) {
    long long n = 0;
    for (int i = 0; s[i] != '\0'; i++) {
        n *= 5;
        if (s[i] == '=') {
            n -= 2;
        } else if (s[i] == '-') {
            n--;
        } else {
            n += s[i] - '0';
        }
    }
    return n;
}

char* toSnafu(long long n) {
    char *b = malloc(30*sizeof(char));
    int len = 0;
    while (n > 0) {
        switch (n % 5) {
        case 3:
            n += 5;
            b[len++] = '=';
            break;
        case 4:
            n += 5;
            b[len++] = '-';
            break;
        default:
            b[len++] = '0' + n % 5;
        }
        n /= 5;
    }
    b[len] = '\0';
    for (int i = 0; i < len / 2; i++) {
        char temp = b[i];
        b[i] = b[len - i - 1];
        b[len - i - 1] = temp;
    }
    return b;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    long long sum = 0;
    char *line = NULL;
    size_t len = 0;
    ssize_t read;

    while ((read = getline(&line, &len, file)) != -1) {
        if (line[read - 1] == '\n') {
            line[read - 1] = '\0';
        }
        sum += fromSnafu(line);
    }
    free(line);
    fclose(file);
    char *snafu = toSnafu(sum);
    printf("%s\n", snafu);
    free(snafu);
    return 0;
}
