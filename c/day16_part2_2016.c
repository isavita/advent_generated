
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DISK_LENGTH 35651584

char* readInitialState(const char* filename) {
    FILE* file = fopen(filename, "r");
    if (!file) {
        perror("Failed to open file");
        exit(EXIT_FAILURE);
    }
    char* line = NULL;
    size_t len = 0;
    if (getline(&line, &len, file) == -1) {
        perror("Failed to read initial state");
        fclose(file);
        exit(EXIT_FAILURE);
    }
    fclose(file);
    line[strcspn(line, "\n")] = 0; // Remove newline character
    return line;
}

char* generateData(char* initialState, int length) {
    char* data = (char*)malloc(length + 1);
    strcpy(data, initialState);
    while (strlen(data) < length) {
        int currentLength = strlen(data);
        data[currentLength] = '0'; // Append '0'
        for (int i = currentLength - 1, j = currentLength + 1; i >= 0; --i, ++j) {
            data[j] = (data[i] == '0') ? '1' : '0';
            if (j == length - 1) break;
        }
        data[length] = '\0'; // Ensure string is null-terminated
    }
    data[length] = '\0'; // Ensure correct string length
    return data;
}

char* calculateChecksum(char* data) {
    int length = strlen(data);
    while (length % 2 == 0) {
        int newLength = 0;
        for (int i = 0; i < length; i += 2) {
            data[newLength++] = (data[i] == data[i + 1]) ? '1' : '0';
        }
        length = newLength;
        data[length] = '\0'; // Ensure string is null-terminated
    }
    return data;
}

int main() {
    char* initialState = readInitialState("input.txt");
    char* data = generateData(initialState, DISK_LENGTH);
    char* checksum = calculateChecksum(data);
    printf("Checksum: %s\n", checksum);
    free(initialState);
    free(data);
    return 0;
}
