
#include <stdio.h>

int main() {
    FILE *file = fopen("input.txt", "r");
    int gridSerialNumber, x, y;
    fscanf(file, "%d", &gridSerialNumber);
    
    int grid[301][301] = {0};
    int maxPower = 0, maxX = 0, maxY = 0;
    
    for (int i = 1; i <= 300; i++) {
        for (int j = 1; j <= 300; j++) {
            int rackID = i + 10;
            int powerLevel = rackID * j;
            powerLevel += gridSerialNumber;
            powerLevel *= rackID;
            powerLevel = powerLevel / 100 % 10;
            powerLevel -= 5;
            
            grid[i][j] = powerLevel + grid[i-1][j] + grid[i][j-1] - grid[i-1][j-1];
        }
    }
    
    for (int i = 1; i <= 300 - 2; i++) {
        for (int j = 1; j <= 300 - 2; j++) {
            int power = grid[i+2][j+2] - grid[i-1][j+2] - grid[i+2][j-1] + grid[i-1][j-1];
            if (power > maxPower) {
                maxPower = power;
                maxX = i;
                maxY = j;
            }
        }
    }
    
    printf("%d,%d", maxX, maxY);
    
    fclose(file);
    return 0;
}
