
#import <Foundation/Foundation.h>

int main() {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *instructions = [input componentsSeparatedByString:@"\n"];
        
        int screen[6][50] = {0};
        
        for (NSString *instruction in instructions) {
            if ([instruction hasPrefix:@"rect"]) {
                NSArray *components = [instruction componentsSeparatedByString:@" "];
                NSArray *size = [components[1] componentsSeparatedByString:@"x"];
                int width = [size[0] intValue];
                int height = [size[1] intValue];
                
                for (int i = 0; i < height; i++) {
                    for (int j = 0; j < width; j++) {
                        screen[i][j] = 1;
                    }
                }
            } else if ([instruction hasPrefix:@"rotate row"]) {
                NSArray *components = [instruction componentsSeparatedByString:@" "];
                int row = [[components[2] componentsSeparatedByString:@"="][1] intValue];
                int shift = [components[4] intValue];
                
                int temp[50];
                for (int i = 0; i < 50; i++) {
                    temp[(i + shift) % 50] = screen[row][i];
                }
                
                for (int i = 0; i < 50; i++) {
                    screen[row][i] = temp[i];
                }
            } else if ([instruction hasPrefix:@"rotate column"]) {
                NSArray *components = [instruction componentsSeparatedByString:@" "];
                int col = [[components[2] componentsSeparatedByString:@"="][1] intValue];
                int shift = [components[4] intValue];
                
                int temp[6];
                for (int i = 0; i < 6; i++) {
                    temp[(i + shift) % 6] = screen[i][col];
                }
                
                for (int i = 0; i < 6; i++) {
                    screen[i][col] = temp[i];
                }
            }
        }
        
        int litPixels = 0;
        for (int i = 0; i < 6; i++) {
            for (int j = 0; j < 50; j++) {
                if (screen[i][j] == 1) {
                    litPixels++;
                }
            }
        }
        
        printf("Part One: %d\n", litPixels);
        
        printf("Part Two:\n");
        for (int i = 0; i < 6; i++) {
            for (int j = 0; j < 50; j++) {
                printf("%c", screen[i][j] == 1 ? '#' : '.');
            }
            printf("\n");
        }
    }
    
    return 0;
}
