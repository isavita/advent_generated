#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *instructions = [input componentsSeparatedByString:@"\n"];
        
        NSMutableDictionary *tiles = [NSMutableDictionary dictionary];
        
        for (NSString *instruction in instructions) {
            int x = 0;
            int y = 0;
            int i = 0;
            while (i < instruction.length) {
                unichar c = [instruction characterAtIndex:i];
                if (c == 'e') {
                    x++;
                    i++;
                } else if (c == 'w') {
                    x--;
                    i++;
                } else if (c == 's' || c == 'n') {
                    NSString *dir = [instruction substringWithRange:NSMakeRange(i, 2)];
                    if ([dir isEqualToString:@"se"]) {
                        y--;
                        x++;
                    } else if ([dir isEqualToString:@"sw"]) {
                        y--;
                    } else if ([dir isEqualToString:@"nw"]) {
                        y++;
                        x--;
                    } else if ([dir isEqualToString:@"ne"]) {
                        y++;
                    }
                    i += 2;
                }
            }
            
            NSString *key = [NSString stringWithFormat:@"%d,%d", x, y];
            if (tiles[key]) {
                [tiles removeObjectForKey:key];
            } else {
                tiles[key] = @1;
            }
        }
        
        int blackTiles = (int)tiles.count;
        printf("Number of black tiles: %d\n", blackTiles);
    }
    return 0;
}