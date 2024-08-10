#import <Foundation/Foundation.h>

int isDigit(unichar c) {
    return c >= '0' && c <= '9';
}

int extractNumber(NSArray<NSArray<NSNumber *> *> *matrix, int *length, int x, int y) {
    NSMutableString *numberStr = [NSMutableString string];
    while (x < matrix[y].count && isDigit([matrix[y][x] intValue])) {
        [numberStr appendFormat:@"%C", [matrix[y][x] intValue]];
        x++;
    }
    *length = (int)numberStr.length;
    return [numberStr intValue];
}

BOOL isAdjacentToSymbol(NSArray<NSArray<NSNumber *> *> *matrix, int x, int y, int length) {
    for (int i = 0; i < length; i++) {
        for (int dy = -1; dy <= 1; dy++) {
            for (int dx = -1; dx <= 1; dx++) {
                int adjX = x + i + dx, adjY = y + dy;
                if (adjY >= 0 && adjY < matrix.count && adjX >= 0 && adjX < matrix[adjY].count) {
                    if (!isDigit([matrix[adjY][adjX] intValue]) && [matrix[adjY][adjX] intValue] != '.') {
                        return YES;
                    }
                }
            }
        }
    }
    return NO;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *filePath = @"input.txt";
        NSError *error = nil;
        NSString *content = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:&error];
        if (error) {
            NSLog(@"Error reading file: %@", error);
            return 1;
        }
        
        NSArray *lines = [content componentsSeparatedByString:@"\n"];
        NSMutableArray<NSMutableArray<NSNumber *> *> *matrix = [NSMutableArray array];
        for (NSString *line in lines) {
            NSMutableArray<NSNumber *> *row = [NSMutableArray array];
            for (NSUInteger i = 0; i < line.length; i++) {
                [row addObject:@([line characterAtIndex:i])];
            }
            [matrix addObject:row];
        }
        
        int sum = 0;
        BOOL visited[matrix.count][[matrix[0] count]];
        memset(visited, 0, sizeof(visited));
        
        for (int y = 0; y < matrix.count; y++) {
            for (int x = 0; x < matrix[y].count; x++) {
                if (!visited[y][x] && isDigit([matrix[y][x] intValue])) {
                    int length = 0;
                    int number = extractNumber(matrix, &length, x, y);
                    if (isAdjacentToSymbol(matrix, x, y, length)) {
                        sum += number;
                    }
                    for (int i = 0; i < length; i++) {
                        visited[y][x + i] = YES;
                    }
                }
            }
        }
        
        NSLog(@"%d", sum);
    }
    return 0;
}