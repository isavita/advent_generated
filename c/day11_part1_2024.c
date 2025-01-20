
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>

#define INITIAL_CAPACITY 1024

typedef struct {
    char** data;
    int size;
    int capacity;
} StringArray;

StringArray createStringArray() {
    StringArray arr;
    arr.data = (char**)malloc(sizeof(char*) * INITIAL_CAPACITY);
    arr.size = 0;
    arr.capacity = INITIAL_CAPACITY;
    return arr;
}

void appendString(StringArray* arr, const char* str) {
    if (arr->size == arr->capacity) {
        arr->capacity *= 2;
        arr->data = (char**)realloc(arr->data, sizeof(char*) * arr->capacity);
    }
    arr->data[arr->size] = strdup(str);
    arr->size++;
}

void freeStringArray(StringArray* arr) {
    for (int i = 0; i < arr->size; i++) {
        free(arr->data[i]);
    }
    free(arr->data);
    arr->size = 0;
    arr->capacity = 0;
}

bool evenDigits(const char* s) {
    int len = strlen(s);
    return len % 2 == 0;
}

char* trimLeadingZeros(char* s) {
    int i = 0;
    while (s[i] == '0' && s[i+1] != '\0') {
        i++;
    }
    if(i > 0){
        memmove(s, s + i, strlen(s) - i + 1);
    }
    return s;
}

int main() {
    FILE* fp = fopen("input.txt", "r");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    char* line = NULL;
    size_t len = 0;
    if (getline(&line, &len, fp) == -1) {
        fclose(fp);
        return 1;
    }
    fclose(fp);
    
    char* token;
    StringArray stones = createStringArray();
    token = strtok(line, " \t\n\r");
    while(token != NULL){
        appendString(&stones, token);
        token = strtok(NULL, " \t\n\r");
    }
    free(line);
    

    for (int i = 0; i < 25; i++) {
        StringArray next = createStringArray();
        for (int j = 0; j < stones.size; j++) {
            if (strcmp(stones.data[j], "0") == 0) {
                appendString(&next, "1");
            } else if (evenDigits(stones.data[j])) {
                int mid = strlen(stones.data[j]) / 2;
                char* left = strndup(stones.data[j], mid);
                char* right = strdup(stones.data[j] + mid);
                
                trimLeadingZeros(left);
                trimLeadingZeros(right);
                if(strlen(left) == 0) {
                    free(left);
                    left = strdup("0");
                }
                 if(strlen(right) == 0) {
                   free(right);
                   right = strdup("0");
                }
                
                appendString(&next, left);
                appendString(&next, right);
                free(left);
                free(right);
            } else {
                long n = atol(stones.data[j]);
                long result = n * 2024;
                char resultStr[32];
                snprintf(resultStr, sizeof(resultStr), "%ld", result);
                appendString(&next, resultStr);
            }
        }
        freeStringArray(&stones);
        stones = next;
    }
    
    printf("%d\n", stones.size);
    freeStringArray(&stones);
    
    return 0;
}
