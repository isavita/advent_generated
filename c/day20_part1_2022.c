
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    int pos;
    int val;
} Num;

void mix(Num *nums, int size);
int coords(Num *nums, int size);

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return -1;
    }

    int size = 0;
    char line[256];
    Num *nums = NULL;

    while (fgets(line, sizeof(line), file)) {
        Num num;
        num.pos = size;
        num.val = atoi(line);
        size++;
        nums = (Num*)realloc(nums, size * sizeof(Num));
        nums[size - 1] = num;
    }

    Num *nums2 = (Num*)malloc(size * sizeof(Num));
    for (int i = 0; i < size; i++) {
        nums2[i].pos = nums[i].pos;
        nums2[i].val = 811589153 * nums[i].val;
    }

    mix(nums, size);
    printf("%d\n", coords(nums, size));

    free(nums);
    free(nums2);
    fclose(file);

    return 0;
}

void mix(Num *nums, int size) {
    int n = size - 1;
    for (int i = 0; i < size; i++) {
        int oldpos = nums[i].pos;
        int newpos = ((oldpos + nums[i].val) % n + n) % n;
        if (oldpos < newpos) {
            for (int j = 0; j < size; j++) {
                if (nums[j].pos > oldpos && nums[j].pos <= newpos) {
                    nums[j].pos--;
                }
            }
        }
        if (newpos < oldpos) {
            for (int j = 0; j < size; j++) {
                if (nums[j].pos >= newpos && nums[j].pos < oldpos) {
                    nums[j].pos++;
                }
            }
        }
        nums[i].pos = newpos;
    }
}

int coords(Num *nums, int size) {
    int l = size;
    int zeroPos;
    for (int i = 0; i < size; i++) {
        if (nums[i].val == 0) {
            zeroPos = nums[i].pos;
            break;
        }
    }

    int sum = 0;
    for (int i = 0; i < size; i++) {
        if (nums[i].pos == (zeroPos + 1000) % l || nums[i].pos == (zeroPos + 2000) % l || nums[i].pos == (zeroPos + 3000) % l) {
            sum += nums[i].val;
        }
    }

    return sum;
}
