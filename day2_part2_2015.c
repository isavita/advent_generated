#include <stdio.h>

int main() {
    FILE *file = fopen("input.txt", "r");
    int l, w, h;
    int total_paper = 0;
    int total_ribbon = 0;

    while (fscanf(file, "%dx%dx%d", &l, &w, &h) != EOF) {
        int side1 = l * w;
        int side2 = w * h;
        int side3 = h * l;
        int smallest_side = side1 < side2 ? (side1 < side3 ? side1 : side3) : (side2 < side3 ? side2 : side3);

        total_paper += 2*l*w + 2*w*h + 2*h*l + smallest_side;
        total_ribbon += 2 * (l + w + h - (l > w ? (l > h ? l : h) : (w > h ? w : h))) + l * w * h;
    }

    printf("%d\n", total_ribbon);

    fclose(file);
    return 0;
}