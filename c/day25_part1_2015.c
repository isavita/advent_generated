#include <stdio.h>
#include <stdlib.h>
#include <regex.h>

int getPosition(int row, int column) {
    return (row + column - 2) * (row + column - 1) / 2 + column;
}

int getCode(int position) {
    const long startCode = 20151125;
    const long multiplier = 252533;
    const long modulus = 33554393;
    long code = startCode;

    for (int i = 1; i < position; i++) {
        code = (code * multiplier) % modulus;
    }
    return code;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) return 1;

    char buffer[100];
    fgets(buffer, sizeof(buffer), file);
    fclose(file);

    regex_t regex;
    regcomp(&regex, "row ([0-9]+), column ([0-9]+)", REG_EXTENDED);
    
    regmatch_t matches[3];
    if (regexec(&regex, buffer, 3, matches, 0) != 0) return 1;

    int row = atoi(buffer + matches[1].rm_so);
    int column = atoi(buffer + matches[2].rm_so);

    int pos = getPosition(row, column);
    int code = getCode(pos);

    printf("%d\n", code);
    regfree(&regex);
    return 0;
}