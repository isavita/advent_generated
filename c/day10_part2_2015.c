#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char* next_sequence(const char* sequence) {
    int len = strlen(sequence), count = 0;
    char* result = malloc(len * 2 + 1);
    int pos = 0;

    for (int i = 0; i < len; i++) {
        count = 1;
        while (i + 1 < len && sequence[i] == sequence[i + 1]) {
            count++;
            i++;
        }
        pos += sprintf(result + pos, "%d%c", count, sequence[i]);
    }
    result[pos] = '\0';
    return result;
}

char* look_and_say(const char* sequence, int iterations) {
    char* result = strdup(sequence);
    for (int i = 0; i < iterations; i++) {
        char* temp = next_sequence(result);
        free(result);
        result = temp;
    }
    return result;
}

int main() {
    FILE* file = fopen("input.txt", "r");
    if (!file) return 1;

    char initial_sequence[256];
    fgets(initial_sequence, sizeof(initial_sequence), file);
    fclose(file);

    char* result = look_and_say(initial_sequence, 50);
    printf("%lu\n", strlen(result));
    free(result);
    return 0;
}