#include <stdio.h>

int main() {
    FILE *fp;
    fp = fopen("input.txt", "r");
    
    int prev_depth = -1;
    int depth;
    int count = 0;
    
    while(fscanf(fp, "%d", &depth) != EOF) {
        if(prev_depth != -1 && depth > prev_depth) {
            count++;
        }
        prev_depth = depth;
    }
    
    fclose(fp);
    
    printf("%d", count);
    
    return 0;
}