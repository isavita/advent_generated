#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSFileManager *fileManager = [NSFileManager defaultManager];
        NSString *filePath = @"input.txt";
        if ([fileManager fileExistsAtPath:filePath]) {
            NSString *fileContents = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
            NSArray *lines = [fileContents componentsSeparatedByString:@"\n"];
            int counts[12][2] = {0};
            for (NSString *line in lines) {
                for (int i = 0; i < [line length]; i++) {
                    NSString *num = [line substringWithRange:NSMakeRange(i, 1)];
                    counts[i][[num intValue]]++;
                }
            }
            
            int gammaRate = 0;
            int epsilonRate = 0;
            for (int i = 0; i < 12; i++) {
                if (counts[i][0] > counts[i][1]) {
                    gammaRate |= 1 << (12 - i - 1);
                } else {
                    epsilonRate |= 1 << (12 - i - 1);
                }
            }
            
            printf("%d\n", gammaRate * epsilonRate);
        }
    }
    return 0;
}