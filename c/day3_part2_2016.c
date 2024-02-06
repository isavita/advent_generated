
#include <stdio.h>

int isValidTriangle(int a, int b, int c) {
    return a + b > c && a + c > b && b + c > a;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    int a, b, c, count1 = 0, count2 = 0;
    int arr[3][3];
    int i = 0;
    
    while (fscanf(file, "%d %d %d", &a, &b, &c) == 3) {
        if (isValidTriangle(a, b, c)) count1++;
        arr[i][0] = a; arr[i][1] = b; arr[i][2] = c;
        if (++i == 3) {
            for (int j = 0; j < 3; j++) {
                if (isValidTriangle(arr[0][j], arr[1][j], arr[2][j])) count2++;
            }
            i = 0;
        }
    }
    fclose(file);
    printf("%d %d\n", count1, count2);
    return 0;
}
