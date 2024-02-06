#include <stdio.h>

#define FABRIC_SIZE 1000

int main() {
    FILE *fptr;
    fptr = fopen("input.txt", "r");
    
    int fabric[FABRIC_SIZE][FABRIC_SIZE] = {0};
    int id, left, top, width, height;
    
    while (fscanf(fptr, "#%d @ %d,%d: %dx%d\n", &id, &left, &top, &width, &height) != EOF) {
        for (int i = top; i < top + height; i++) {
            for (int j = left; j < left + width; j++) {
                fabric[i][j]++;
            }
        }
    }
    
    int total_overlap = 0;
    for (int i = 0; i < FABRIC_SIZE; i++) {
        for (int j = 0; j < FABRIC_SIZE; j++) {
            if (fabric[i][j] >= 2) {
                total_overlap++;
            }
        }
    }
    
    fclose(fptr);
    
    printf("%d\n", total_overlap);
    
    return 0;
}