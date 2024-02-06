
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int calculateEncodedLength(const char *s) {
    int encodedLength = 2; // Start with 2 for the leading and trailing quotes
    for (int i = 0; s[i] != '\0'; i++) {
        if (s[i] == '\\' || s[i] == '"') {
            encodedLength++; // Add one for the backslash
        }
        encodedLength++; // Add one for the current character
    }
    return encodedLength;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    char *line = NULL;
    size_t len = 0;
    ssize_t read;
    int totalDiff = 0;

    while ((read = getline(&line, &len, file)) != -1) {
        // Remove newline character if present
        if (line[read - 1] == '\n') {
            line[read - 1] = '\0';
            read--;
        }
        int originalLength = strlen(line);
        int encodedLength = calculateEncodedLength(line);
        totalDiff += encodedLength - originalLength;
    }

    free(line);
    fclose(file);

    printf("%d\n", totalDiff);

    return 0;
}
