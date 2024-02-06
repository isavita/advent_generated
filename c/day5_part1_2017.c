
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *file;
    char *line = NULL;
    size_t len = 0;
    ssize_t read;
    int *offsets = NULL;
    int size = 0, capacity = 10;

    offsets = (int *)malloc(capacity * sizeof(int));
    if (offsets == NULL) {
        fprintf(stderr, "Memory allocation failed\n");
        return 1;
    }

    file = fopen("input.txt", "r");
    if (file == NULL) {
        fprintf(stderr, "Could not open file\n");
        return 1;
    }

    while ((read = getline(&line, &len, file)) != -1) {
        if (size >= capacity) {
            capacity *= 2;
            offsets = (int *)realloc(offsets, capacity * sizeof(int));
            if (offsets == NULL) {
                fprintf(stderr, "Memory reallocation failed\n");
                return 1;
            }
        }
        offsets[size++] = atoi(line);
    }
    fclose(file);
    free(line);

    int index = 0, steps = 0;
    while (index >= 0 && index < size) {
        int jump = offsets[index];
        offsets[index]++;
        index += jump;
        steps++;
    }

    printf("%d\n", steps);
    free(offsets);
    return 0;
}
