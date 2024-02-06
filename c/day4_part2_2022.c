
#include <stdio.h>

int main() {
    FILE* file = fopen("input.txt", "r");
    int num1, num2, num3, num4;
    int count1 = 0, count2 = 0;
    
    while(fscanf(file, "%d-%d,%d-%d\n", &num1, &num2, &num3, &num4) != EOF) {
        if((num1 <= num3 && num4 <= num2) || (num3 <= num1 && num2 <= num4)) {
            count1++;
        }
        if((num1 <= num4 && num4 <= num2) || (num3 <= num2 && num2 <= num4) || (num3 <= num1 && num1 <= num4) || (num3 <= num2 && num2 <= num4)) {
            count2++;
        }
    }
    
    printf("%d", count1);
    printf("%d", count2);
    
    fclose(file);
    return 0;
}
