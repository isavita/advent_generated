
#include <stdio.h>

int main() {
    FILE *input_file = fopen("input.txt", "r");
    if (input_file == NULL) {
        return 1;
    }

    int count = 0;
    char line[100];
    while (fgets(line, sizeof(line), input_file)) {
        int start1, end1, start2, end2;
        sscanf(line, "%d-%d,%d-%d", &start1, &end1, &start2, &end2);

        if ((start1 <= start2 && end1 >= end2) || (start1 >= start2 && end1 <= end2)) {
            count++;
        }
    }

    printf("%d\n", count);

    fclose(input_file);
    return 0;
}
