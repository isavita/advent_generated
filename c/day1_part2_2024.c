#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_NUMBERS 10000
#define BUFFER_SIZE 1024

struct NumCount {
    int num;
    int count;
};

int compare(const void *a, const void *b) {
    return (*(int*)a - *(int*)b);
}

int count_occurrences(int num, int *arr, int size) {
    int count = 0;
    for (int i = 0; i < size; i++) {
        if (arr[i] == num) count++;
    }
    return count;
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

    long similarity_score = 0;
    for (int i = 0; i < count; i++) {
        int occurrences = count_occurrences(left_list[i], right_list, count);
        similarity_score += (long)left_list[i] * occurrences;
    }

    printf("Similarity score: %ld\n", similarity_score);
    return 0;
}
