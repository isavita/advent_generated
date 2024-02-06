#include <stdio.h>

int main() {
    FILE *fp;
    fp = fopen("input.txt", "r");
    
    int frequency = 0;
    int change;
    
    while (fscanf(fp, "%d", &change) != EOF) {
        frequency += change;
    }
    
    printf("%d\n", frequency);
    
    fclose(fp);
    
    return 0;
}