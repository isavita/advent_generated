#import <Foundation/Foundation.h>

int main() {
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
    
    FILE *file = fopen("input.txt", "r");
    char line[100];
    int headX = 0, headY = 0, tailX = 0, tailY = 0;
    BOOL visited[1000][1000] = {NO};
    
    while (fgets(line, sizeof(line), file)) {
        char dir;
        int steps;
        sscanf(line, "%c %d", &dir, &steps);
        
        for (int i = 0; i < steps; i++) {
            switch (dir) {
                case 'R':
                    headX++;
                    break;
                case 'L':
                    headX--;
                    break;
                case 'U':
                    headY++;
                    break;
                case 'D':
                    headY--;
                    break;
            }
            
            if (abs(headX - tailX) > 1 || abs(headY - tailY) > 1) {
                if (headX != tailX && headY != tailY) {
                    if (headX > tailX) {
                        tailX++;
                    } else {
                        tailX--;
                    }
                    if (headY > tailY) {
                        tailY++;
                    } else {
                        tailY--;
                    }
                } else {
                    if (headX > tailX) {
                        tailX++;
                    } else if (headX < tailX) {
                        tailX--;
                    }
                    if (headY > tailY) {
                        tailY++;
                    } else if (headY < tailY) {
                        tailY--;
                    }
                }
            }
            
            visited[tailX + 500][tailY + 500] = YES;
        }
    }
    
    int count = 0;
    for (int i = 0; i < 1000; i++) {
        for (int j = 0; j < 1000; j++) {
            if (visited[i][j]) {
                count++;
            }
        }
    }
    
    printf("%d\n", count);
    
    fclose(file);
    [pool drain];
    
    return 0;
}