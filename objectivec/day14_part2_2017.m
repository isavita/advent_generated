#import <Foundation/Foundation.h>

void reverseSection(NSMutableArray *arr, int start, int length, int n) {
    int i = start, j = start + length - 1;
    while (i < j) {
        int temp = [arr[i % n] intValue];
        arr[i % n] = arr[j % n];
        arr[j % n] = @(temp);
        i++; j--;
    }
}

void knotHash(NSString *input, char *result) {
    NSMutableArray *lengths = [NSMutableArray array];
    for (int i = 0; i < input.length; i++) {
        unichar c = [input characterAtIndex:i];
        [lengths addObject:@(c)];
    }
    [lengths addObjectsFromArray:@[@(17), @(31), @(73), @(47), @(23)]];
    
    NSMutableArray *list = [NSMutableArray arrayWithCapacity:256];
    for (int i = 0; i < 256; i++) list[i] = @(i);
    
    int position = 0, skip = 0;
    for (int round = 0; round < 64; round++) {
        for (NSNumber *len in lengths) {
            reverseSection(list, position, [len intValue], 256);
            position += [len intValue] + skip;
            skip++;
        }
    }
    
    NSMutableArray *denseHash = [NSMutableArray arrayWithCapacity:16];
    for (int i = 0; i < 16; i++) {
        int xor = 0;
        for (int j = 0; j < 16; j++) xor ^= [list[i * 16 + j] intValue];
        [denseHash addObject:@(xor)];
    }
    
    for (int i = 0; i < 16; i++) sprintf(&result[i*2], "%02x", [denseHash[i] intValue]);
}

void hexToBinary(const char *hexStr, char *binaryStr) {
    for (int i = 0; hexStr[i]; i++) {
        int num = (hexStr[i] >= '0' && hexStr[i] <= '9') ? hexStr[i] - '0' : hexStr[i] - 'a' + 10;
        for (int j = 3; j >= 0; j--) binaryStr[i * 4 + (3 - j)] = (num & (1 << j)) ? '1' : '0';
    }
    binaryStr[strlen(hexStr) * 4] = '\0';
}

void dfs(int x, int y, int grid[128][128]) {
    if (x < 0 || x >= 128 || y < 0 || y >= 128 || grid[x][y] != 1) return;
    grid[x][y] = 0;
    dfs(x - 1, y, grid); dfs(x + 1, y, grid); dfs(x, y - 1, grid); dfs(x, y + 1, grid);
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *keyString = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        keyString = [keyString stringByTrimmingCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        
        int grid[128][128] = {{0}}, regions = 0;
        
        for (int i = 0; i < 128; i++) {
            NSString *rowKey = [NSString stringWithFormat:@"%@-%d", keyString, i];
            char hash[33];
            knotHash(rowKey, hash);
            char binaryRow[129];
            hexToBinary(hash, binaryRow);
            
            for (int j = 0; binaryRow[j]; j++) if (binaryRow[j] == '1') grid[i][j] = 1;
        }
        
        for (int i = 0; i < 128; i++) {
            for (int j = 0; j < 128; j++) {
                if (grid[i][j] == 1) { regions++; dfs(i, j, grid); }
            }
        }
        
        printf("%d\n", regions);
    }
    return 0;
}