
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
    FILE* file;
    char* input;
    long length;
    int halfway, sum = 0;

    file = fopen("input.txt", "rb");
    if (file == NULL) {
        perror("File opening error");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    length = ftell(file);
    fseek(file, 0, SEEK_SET);

    input = malloc(length);
    if (input) {
        fread(input, 1, length, file);
    }
    fclose(file);

    input[strcspn(input, "\r\n")] = 0; // Remove newline characters if present
    halfway = strlen(input) / 2;

    for (int i = 0; i < strlen(input); i++) {
        int next = (i + halfway) % strlen(input);
        if (input[i] == input[next]) {
            sum += input[i] - '0';
        }
    }

    printf("%d\n", sum);
    free(input);
    return 0;
}
