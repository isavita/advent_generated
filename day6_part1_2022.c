
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char* readAll(char* path) {
    FILE* file = fopen(path, "r");
    if (!file) {
        exit(1);
    }
    
    fseek(file, 0, SEEK_END);
    long size = ftell(file);
    rewind(file);
    
    char* buffer = (char*)malloc(size + 1);
    fread(buffer, size, 1, file);
    fclose(file);
    
    buffer[size] = '\0';
    return buffer;
}

char* SetOf(char* b) {
    int m[256] = {0};
    int len = strlen(b);
    for (int i = 0; i < len; i++) {
        m[(int)b[i]] = 1;
    }
    
    char* unique = (char*)malloc(len + 1);
    int idx = 0;
    for (int i = 0; i < len; i++) {
        if (m[(int)b[i]] == 1) {
            unique[idx++] = b[i];
            m[(int)b[i]] = 0;
        }
    }
    unique[idx] = '\0';
    
    return unique;
}

int firstNUnique(char* s, int n) {
    int len = strlen(s);
    for (int i = n; i < len; i++) {
        char* sub = (char*)malloc(n + 1);
        strncpy(sub, &s[i-n], n);
        sub[n] = '\0';
        
        char* unique = SetOf(sub);
        if (strlen(unique) == n) {
            free(sub);
            free(unique);
            return i;
        }
        free(sub);
        free(unique);
    }
    return -1;
}

int main() {
    char* s = readAll("input.txt");
    printf("%d\n", firstNUnique(s, 4));
    return 0; 
}
