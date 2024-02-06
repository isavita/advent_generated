
#include <stdio.h>

int main() {
    FILE *fp;
    fp = fopen("input.txt", "r");
    int lower, upper;
    fscanf(fp, "%d-%d", &lower, &upper);
    
    int count = 0;
    for (int i = lower; i <= upper; i++) {
        int num = i;
        int prev_digit = 10;
        int adjacent = 0;
        int valid = 1;
        
        while (num > 0) {
            int digit = num % 10;
            if (digit > prev_digit) {
                valid = 0;
                break;
            }
            if (digit == prev_digit) {
                adjacent = 1;
            }
            prev_digit = digit;
            num /= 10;
        }
        
        if (valid && adjacent) {
            count++;
        }
    }
    
    printf("%d\n", count);

    return 0;
}
