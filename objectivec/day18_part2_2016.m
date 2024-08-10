#import <Foundation/Foundation.h>

const int totalRows = 400000;

int countSafeTiles(NSString *firstRow);
BOOL isTrap(int left, int center, int right, NSString *row);
unichar safeIfOutOfBounds(int index, NSString *row);
int countChar(NSString *str, unichar character);

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *firstRow = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        int safeTilesCount = countSafeTiles(firstRow);
        NSLog(@"%d", safeTilesCount);
    }
    return 0;
}

int countSafeTiles(NSString *firstRow) {
    NSString *currentRow = firstRow;
    int safeCount = countChar(currentRow, '.');

    for (int i = 1; i < totalRows; i++) {
        NSMutableString *nextRow = [NSMutableString stringWithCapacity:currentRow.length];
        for (int j = 0; j < currentRow.length; j++) {
            if (isTrap(j - 1, j, j + 1, currentRow)) {
                [nextRow appendString:@"^"];
            } else {
                [nextRow appendString:@"."];
                safeCount++;
            }
        }
        currentRow = nextRow;
    }
    return safeCount;
}

BOOL isTrap(int left, int center, int right, NSString *row) {
    unichar l = safeIfOutOfBounds(left, row);
    unichar c = [row characterAtIndex:center];
    unichar r = safeIfOutOfBounds(right, row);

    return (l == '^' && c == '^' && r == '.') ||
           (c == '^' && r == '^' && l == '.') ||
           (l == '^' && c == '.' && r == '.') ||
           (r == '^' && c == '.' && l == '.');
}

unichar safeIfOutOfBounds(int index, NSString *row) {
    if (index < 0 || index >= row.length) {
        return '.';
    }
    return [row characterAtIndex:index];
}

int countChar(NSString *str, unichar character) {
    int count = 0;
    for (NSUInteger i = 0; i < str.length; i++) {
        if ([str characterAtIndex:i] == character) {
            count++;
        }
    }
    return count;
}