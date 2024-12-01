#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_NUMBERS 10000
#define BUFFER_SIZE 1024

int compare(const void *a, const void *b) {
    return (*(int*)a - *(int*)b);
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        printf("Error opening file\n");
        return 1;
    }

    int left_list[MAX_NUMBERS];
    int right_list[MAX_NUMBERS];
    int count = 0;
    char line[BUFFER_SIZE];

    while (fgets(line, sizeof(line), file)) {
        if (line[0] == '\n') continue;
        int left, right;
        sscanf(line, "%d %d", &left, &right);
        left_list[count] = left;
        right_list[count] = right;
        count++;
    }
    fclose(file);

    qsort(left_list, count, sizeof(int), compare);
    qsort(right_list, count, sizeof(int), compare);

    long total_distance = 0;
    for (int i = 0; i < count; i++) {
        total_distance += abs(left_list[i] - right_list[i]);
    }

    printf("Total distance: %ld\n", total_distance);
    return 0;
}
