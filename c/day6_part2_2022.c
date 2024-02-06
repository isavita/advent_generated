
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char* readAll(const char* path) {
    FILE* file = fopen(path, "r");
    if (file == NULL) {
        exit(1);
    }

    fseek(file, 0, SEEK_END);
    long length = ftell(file);
    fseek(file, 0, SEEK_SET);

    char* buffer = (char*)malloc(length + 1);
    fread(buffer, 1, length, file);
    fclose(file);

    buffer[length] = '\0';
    
    return buffer;
}

char* SetOf(char* b) {
    int m[256] = {0};
    int len = strlen(b);
    
    for (int i = 0; i < len; i++) {
        m[(int)b[i]] = 1;
    }

    char* result = (char*)malloc(len + 1);
    int index = 0;
    
    for (int i = 0; i < 256; i++) {
        if (m[i] == 1) {
            result[index++] = (char)i;
        }
    }
    result[index] = '\0';
    
    return result;
}

int firstNUnique(char* s, int n) {
    int len = strlen(s);
    
    for (int i = n; i < len; i++) {
        char* b = (char*)malloc(n + 1);
        strncpy(b, s + i - n, n);
        b[n] = '\0';

        char* unique = SetOf(b);
        int unique_len = strlen(unique);

        if (unique_len == n) {
            free(b);
            free(unique);
            return i;
        }

        free(b);
        free(unique);
    }
    
    return -1;
}

int main() {
    char* s = readAll("input.txt");
    printf("%d\n", firstNUnique(s, 14));
    free(s);
    
    return 0;
}
