#include <stdio.h>

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        return 1;
    }

    char c;
    long long decompressedLength = 0;
    while ((c = fgetc(file)) != EOF) {
        if (c == '(') {
            int numChars, repeat;
            fscanf(file, "%dx%d)", &numChars, &repeat);
            decompressedLength += numChars * repeat;
            fseek(file, numChars, SEEK_CUR);
        } else if (c != ' ' && c != '\n' && c != '\t') {
            decompressedLength++;
        }
    }

    fclose(file);

    printf("%lld\n", decompressedLength);

    return 0;
}