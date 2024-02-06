
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return EXIT_FAILURE;
    }

    int vals[10000]; // Assuming a maximum of 10000 integers in the file.
    int num, i = 0;

    while (fscanf(file, "%d", &num) == 1) {
        vals[i++] = num;
    }
    fclose(file);

    int prevSum = 0, count = 0;
    for (int j = 2; j < i; j++) {
        int currSum = vals[j-2] + vals[j-1] + vals[j];
        if (j > 2 && currSum > prevSum) {
            count++;
        }
        prevSum = currSum;
    }

    printf("%d\n", count);
    return 0;
}
