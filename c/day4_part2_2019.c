#include <stdio.h>
#include <stdlib.h>

int check_criteria(int num) {
    int adj = 0;
    int adj_not_part_of_larger_group = 0;
    int prev_digit = num % 10;
    int curr_digit = 0;
    int num_copy = num / 10;
    int consecutive_count = 1;

    while (num_copy > 0) {
        curr_digit = num_copy % 10;

        if (curr_digit == prev_digit) {
            adj = 1;
            consecutive_count++;
        } else {
            if (consecutive_count == 2) {
                adj_not_part_of_larger_group = 1;
            }
            consecutive_count = 1;
        }

        if (curr_digit > prev_digit) {
            return 0;
        }

        prev_digit = curr_digit;
        num_copy /= 10;
    }

    if (consecutive_count == 2) {
        adj_not_part_of_larger_group = 1;
    }

    return adj && adj_not_part_of_larger_group;
}

int main() {
    FILE *fp;
    fp = fopen("input.txt", "r");
    if (fp == NULL) {
        printf("Error opening file\n");
        return 1;
    }

    int lower_bound, upper_bound;
    fscanf(fp, "%d-%d", &lower_bound, &upper_bound);
    fclose(fp);

    int count = 0;

    for (int i = lower_bound; i <= upper_bound; i++) {
        if (check_criteria(i)) {
            count++;
        }
    }

    printf("%d\n", count);

    return 0;
}