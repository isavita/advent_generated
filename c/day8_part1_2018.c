#include <stdio.h>
#include <stdlib.h>

int parseTree(int *data, int index, int *sum) {
    int childCount = data[index++];
    int metaCount = data[index++];
    
    for (int i = 0; i < childCount; i++) {
        index = parseTree(data, index, sum);
    }
    
    for (int i = 0; i < metaCount; i++) {
        *sum += data[index++];
    }
    
    return index;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    int *numbers = malloc(10000 * sizeof(int));
    int count = 0;

    while (fscanf(file, "%d", &numbers[count]) != EOF) {
        count++;
    }
    fclose(file);

    int sum = 0;
    parseTree(numbers, 0, &sum);
    printf("%d\n", sum);
    
    free(numbers);
    return 0;
}