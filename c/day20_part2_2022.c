
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    int pos;
    long long val;
} num;

void mix(num *nums, int n) {
    for (int i = 0; i < n; i++) {
        int oldpos = nums[i].pos;
        long long newpos = ((long long)oldpos + nums[i].val) % (n - 1);
        if (newpos < 0) newpos += (n - 1);
        
        if (oldpos < newpos) {
            for (int j = 0; j < n; j++) {
                if (nums[j].pos > oldpos && nums[j].pos <= newpos) {
                    nums[j].pos--;
                }
            }
        } else if (newpos < oldpos) {
            for (int j = 0; j < n; j++) {
                if (nums[j].pos >= newpos && nums[j].pos < oldpos) {
                    nums[j].pos++;
                }
            }
        }
        nums[i].pos = newpos;
    }
}

long long coords(num *nums, int n) {
    int zeroPos = 0;
    for (int i = 0; i < n; i++) {
        if (nums[i].val == 0) {
            zeroPos = nums[i].pos;
            break;
        }
    }
    long long sum = 0;
    for (int i = 0; i < n; i++) {
        if (nums[i].pos == (zeroPos + 1000) % n || 
            nums[i].pos == (zeroPos + 2000) % n ||
            nums[i].pos == (zeroPos + 3000) % n) {
            sum += nums[i].val;
        }
    }
    return sum;
}

int main() {
    FILE *fp = fopen("input.txt", "r");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    char *line = NULL;
    size_t len = 0;
    ssize_t read;
    int count = 0;
    num *nums = NULL;

    while ((read = getline(&line, &len, fp)) != -1) {
        nums = realloc(nums, (count + 1) * sizeof(num));
        if (nums == NULL) {
            perror("Realloc failed");
            fclose(fp);
            if(line) free(line);
            return 1;
        }
        
        nums[count].pos = count;
        nums[count].val = atoll(line) * 811589153;
        count++;
    }

    fclose(fp);
    if(line) free(line);

    for (int i = 0; i < 10; i++) {
        mix(nums, count);
    }
    
    printf("%lld\n", coords(nums, count));
    free(nums);
    return 0;
}
