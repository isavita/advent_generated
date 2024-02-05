#include <stdio.h>

int main() {
    FILE *fp;
    fp = fopen("input.txt", "r");

    int l, w, h;
    int total_paper = 0;

    while (fscanf(fp, "%dx%dx%d\n", &l, &w, &h) != EOF) {
        int side1 = l * w;
        int side2 = w * h;
        int side3 = h * l;

        int min_side = (side1 < side2) ? ((side1 < side3) ? side1 : side3) : ((side2 < side3) ? side2 : side3);

        total_paper += 2 * side1 + 2 * side2 + 2 * side3 + min_side;
    }

    printf("%d\n", total_paper);

    fclose(fp);

    return 0;
}