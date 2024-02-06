
#include <stdio.h>

int isValidTriangle(int a, int b, int c) {
    return (a + b > c) && (a + c > b) && (b + c > a);
}

int main() {
    FILE *file = fopen("input.txt", "r");
    int a, b, c;
    int possibleTriangles = 0;

    while (fscanf(file, "%d %d %d", &a, &b, &c) != EOF) {
        if (isValidTriangle(a, b, c)) {
            possibleTriangles++;
        }
    }

    printf("%d\n", possibleTriangles);

    fclose(file);

    return 0;
}
