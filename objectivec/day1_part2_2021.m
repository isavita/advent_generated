#import <Foundation/Foundation.h>

int main() {
    NSFileManager *fileManager = [NSFileManager defaultManager];
    NSString *filePath = @"input.txt";
    if ([fileManager fileExistsAtPath:filePath]) {
        NSString *content = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [content componentsSeparatedByString:@"\n"];
        
        NSMutableArray *vals = [[NSMutableArray alloc] init];
        for (NSString *line in lines) {
            if ([line isEqualToString:@""]) {
                continue;
            }
            int val = [line intValue];
            [vals addObject:[NSNumber numberWithInt:val]];
        }
        
        int prevSum = [vals[0] intValue] + [vals[1] intValue] + [vals[2] intValue];
        int count = 0;
        for (int i = 3; i < [vals count]; i++) {
            int currSum = [vals[i-2] intValue] + [vals[i-1] intValue] + [vals[i] intValue];
            if (currSum > prevSum) {
                count++;
            }
            prevSum = currSum;
        }
        
        printf("%d\n", count);
    }
    
    return 0;
}