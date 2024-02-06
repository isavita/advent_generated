
#include <stdio.h>
#include <limits.h>

int main() {
    FILE *file;
    file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("Error opening file\n");
        return 1;
    }

    int checksum = 0, part2sum = 0;
    while (!feof(file)) {
        int nums[100], size = 0, min = INT_MAX, max = INT_MIN;
        while (fscanf(file, "%d", &nums[size]) == 1) {
            if (nums[size] < min) min = nums[size];
            if (nums[size] > max) max = nums[size];
            size++;
            char next = fgetc(file);
            if (next == '\n' || next == EOF) break;
            ungetc(next, file);
        }
        checksum += max - min;

        for (int i = 0; i < size; ++i) {
            for (int j = 0; j < size; ++j) {
                if (i != j && nums[i] % nums[j] == 0) {
                    part2sum += nums[i] / nums[j];
                }
            }
        }
    }
    fclose(file);

    printf("Part 1: %d\n", checksum);
    printf("Part 2: %d\n", part2sum);

    return 0;
}
